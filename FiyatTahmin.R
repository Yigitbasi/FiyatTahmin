###################24 saatte bir kod çalışır. while ile. try catchler eklenmiştir olası bir sorunda döngü çökmemelidir.

library(quantmod)
library(dplyr)
library(caret)
library(randomForest)
library(httr)
library(jsonlite)
library(zoo)

# Hisse sembolleri ve isimleri
# Türk hisse sembolleri ve isimleri
semboller <- c("AGHOL.IS", "AKBNK.IS", "AKSA.IS", "ALARK.IS", "ALGYO.IS", "ANSGR.IS",
               "ARCLK.IS", "ASELS.IS", "GARAN.IS", "SASA.IS", "THYAO.IS", "TKFEN.IS",
               "TSKB.IS", "TTRAK.IS", "ULKER.IS", "VESTL.IS", "YATAS.IS", "ZOREN.IS")

sembol_isimleri <- c(
  "ANADOLU GRUP HOLDING", "AKBANK T.A.Ş.", "AKSA AKRILIK KIMYA SANAYII A.Ş.",
  "ALARKO HOLDING A.Ş.", "ALARKO GAYRIMENKUL YATIRIM ORTAKLIĞI", "ANADOLU SIGORTA A.Ş.",
  "ARÇELIK A.Ş.", "ASELSAN ELEKTRONIK SANAYI VE TICARET A.Ş.", "TURKIYE GARANTI BANKASI A.Ş.",
  "SASA POLYESTER SANAYI A.Ş.", "TURK HAVA YOLLARI A.O.", "TEKFEN HOLDING A.Ş.",
  "TURKIYE SINAI KALKINMA BANKASI A.Ş.", "TURK TRAKTOR VE ZIRAAT MAKINELERI A.Ş.",
  "ULKER BISKUVI SANAYI A.Ş.", "VESTEL ELEKTRONIK SANAYI VE TICARET A.Ş.",
  "YATAŞ YATAK VE YORGAN SANAYI VE TICARET A.Ş.", "ZORLU ENERJI ELEKTRIK URETIM A.Ş."
)

# Amerikan hisse sembolleri ve isimleri
amerikan_sembolleri <- c("AAPL", "AMZN", "BABA", "GOOGL", "META", "MSFT", "NFLX", "NVDA", "RKLB", "TSLA")

amerikan_sembol_isimleri <- c(
  "APPLE INC.", "AMAZON.COM, INC.", "ALIBABA GROUP HOLDING LIMITED",
  "ALPHABET INC.", "META PLATFORMS, INC.", "MICROSOFT CORPORATION",
  "NETFLIX, INC.", "NVIDIA CORPORATION", "ROCKET LAB USA, INC.", "TESLA, INC."
)

# Firebase bağlantısı
firebase_url <- "https://users-59f1f-default-rtdb.europe-west1.firebasedatabase.app/"

while(TRUE) {
  start_time <- Sys.time()
  cat("Çalışma Başladı:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  tryCatch({
    # Borsa İstanbul Verileri
    for (i in seq_along(semboller)) {
      sembol <- semboller[i]
      sembol_isim <- sembol_isimleri[i]
      sembol_for_firebase <- gsub("\\.IS", "", sembol)
      cat("Hisse (BIST):", sembol_for_firebase, "\n")
      
      # Veri çekme (BIST)
      veri <- tryCatch({
        getSymbols(Symbols = sembol, src = "yahoo", from = Sys.Date() - 200, to = Sys.Date(), auto.assign = FALSE, adjust = TRUE)
      }, error = function(e) {
        cat("Hisse verisi alınamadı (BIST):", sembol_for_firebase, " - Hata:", e$message, "\n")
        return(NULL)
      })
      if (is.null(veri)) {
        cat("Hisse verisi boş, sonraki hisseye geçiliyor (BIST):", sembol_for_firebase, "\n")
        next
      }
      if(length(dim(veri)) < 2) {
        cat("Hisse verisi uygun formatta değil, sonraki hisseye geçiliyor (BIST):", sembol_for_firebase, "\n")
        next
      }
      
      print(colSums(is.na(veri)))
      veri<-na.omit(veri)
      if (is.null(veri) || nrow(veri) == 0) {
        cat("Veri çekme sonrası boş veri oluştu, sonraki hisseye geçiliyor(BIST):", sembol_for_firebase, "\n")
        next
      }
      
      colnames(veri) <- c("Acilis", "Yuksek", "Dusuk", "Kapanis", "Hacim", "Adj")
      sonkapanis <- as.numeric(tail(veri$Kapanis, 1))
      print(colSums(is.na(veri)))
      
      # xts nesnesini data.frame'e dönüştürme
      veri_df <- data.frame(Date = index(veri), coredata(veri))
      
      # Veri ön işleme
      veri_df <- veri_df %>%
        mutate(
          Daily = round((Kapanis - lag(Kapanis, n = 1)) / lag(Kapanis, n = 1) * 100, 2),
          Tomorrow = lead(Kapanis, n = 1),
          Three = lead(Kapanis, n = 3),
          Seven = lead(Kapanis, n = 7)
        )
      print(colSums(is.na(veri_df)))
      
      # Eksik değerleri doldurma
      veri_df <- veri_df %>%
        mutate(across(c(Kapanis, Daily, Tomorrow, Three, Seven), ~ na.locf(.x, na.rm = FALSE))) %>%
        mutate(across(c(Kapanis, Daily, Tomorrow, Three, Seven), ~ na.locf(.x, fromLast = TRUE, na.rm = FALSE))) %>%
        mutate(across(c(Kapanis, Daily, Tomorrow, Three, Seven), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))
      
      # Eksik veri kontrolü
      if (any(is.na(veri_df$Tomorrow))) {
        cat("Eksik değerler tam doldurulamadı, sonraki hisseye geçiliyor (BIST):", sembol_for_firebase, "\n")
        next
      }
      
      # Veri bölme
      div <- createDataPartition(veri_df$Tomorrow, p = 0.8, list = FALSE)
      egitim <- veri_df[div, ]
      testing <- veri_df[-div, ]
      
      # Hiperparametre Grid
      tuneGrid_rf <- expand.grid(.mtry = c(2, 4, 6))
      
      # Modelleri Eğitme
      modelrf_tomorrow <- train(Tomorrow ~ ., data = egitim, method = "rf", tuneGrid = tuneGrid_rf, ntree = 500)
      modelrf_three <- train(Three ~ ., data = egitim, method = "rf", tuneGrid = tuneGrid_rf, ntree = 500)
      modelrf_seven <- train(Seven ~ ., data = egitim, method = "rf", tuneGrid = tuneGrid_rf, ntree = 500)
      
      # Lineer Regresyon Modelleri
      modellm_tomorrow <- train(Tomorrow ~ ., data = egitim, method = "lm")
      modellm_three <- train(Three ~ ., data = egitim, method = "lm")
      modellm_seven <- train(Seven ~ ., data = egitim, method = "lm")
      
      # Tahminler
      gelecekveri <- tail(veri_df, 1)
      tahminler <- list(
        Tomorrow_LM = predict(modellm_tomorrow, gelecekveri),
        Three_LM = predict(modellm_three, gelecekveri),
        Seven_LM = predict(modellm_seven, gelecekveri),
        
        Tomorrow_RF = predict(modelrf_tomorrow, gelecekveri),
        Three_RF = predict(modelrf_three, gelecekveri),
        Seven_RF = predict(modelrf_seven, gelecekveri)
      )
      # postresample: RMSE, R-squared, MAE
      performans <- list(
        Tomorrow_LM = postResample(predict(modellm_tomorrow, testing), testing$Tomorrow),
        Three_LM = postResample(predict(modellm_three, testing), testing$Three),
        Seven_LM = postResample(predict(modellm_seven, testing), testing$Seven),
        
        Tomorrow_RF = postResample(predict(modelrf_tomorrow, testing), testing$Tomorrow),
        Three_RF = postResample(predict(modelrf_three, testing), testing$Three),
        Seven_RF = postResample(predict(modelrf_seven, testing), testing$Seven)
      )
      
      cat("Performans Metrikleri (BIST):\n")
      for (model_adi in names(performans)) {
        cat("Model:", model_adi, "\n")
        sonuclar <- performans[[model_adi]]
        for (metrik_adi in names(sonuclar)) {
          cat(metrik_adi, ":", sonuclar[[metrik_adi]], "\n")
        }
        cat("--------------------\n")
      }
      
      # Firebase'e gönderilecek veri
      data_to_send <- list(
        Kapanis = sonkapanis,
        Daily = as.numeric(tail(veri_df$Daily, 1)),
        Tahminler = tahminler,
        Performans = performans,
        Tarih = as.character(Sys.Date()),
        Sembol_Ismi = sembol_isim
      )
      
      url <- paste0(firebase_url, "hisse_verileri/borsa_istanbul/", sembol_for_firebase , ".json")
      
      response <- tryCatch({
        httr::PATCH(url, body = toJSON(data_to_send), encode = "json")
      }, error = function(e) {
        cat("Firebase'e veri gönderilirken hata oluştu (BIST):", e$message, "\n")
        return(NULL)
      })
      
      if (!is.null(response)) {
        cat(sembol_for_firebase, " için sonuçlar Firebase'e kaydedildi (BIST).\n")
      }
    }
    
    # Amerikan Borsası Verileri
    for (i in seq_along(amerikan_sembolleri)) {
      sembol <- amerikan_sembolleri[i]
      sembol_isim <- amerikan_sembol_isimleri[i]
      sembol_for_firebase <- gsub("\\.IS", "", sembol)
      cat("Hisse (Amerikan):", sembol_for_firebase, "\n")
      
      # Veri çekme
      veri <- tryCatch({
        getSymbols(Symbols = sembol, src = "yahoo", from = Sys.Date() - 200, to = Sys.Date(), auto.assign = FALSE, adjust = TRUE)
      }, error = function(e) {
        cat("Hisse verisi alınamadı (Amerikan):", sembol_for_firebase, " - Hata:", e$message, "\n")
        return(NULL)
      })
      if (is.null(veri)) {
        cat("Hisse verisi boş, sonraki hisseye geçiliyor (Amerikan):", sembol_for_firebase, "\n")
        next
      }
      if(length(dim(veri)) < 2) {
        cat("Hisse verisi uygun formatta değil, sonraki hisseye geçiliyor (Amerikan):", sembol_for_firebase, "\n")
        next
      }
      
      print(colSums(is.na(veri)))
      veri<-na.omit(veri)
      if (is.null(veri) || nrow(veri) == 0) {
        cat("Veri çekme sonrası boş veri oluştu, sonraki hisseye geçiliyor(Amerikan):", sembol_for_firebase, "\n")
        next
      }
      
      colnames(veri) <- c("Acilis", "Yuksek", "Dusuk", "Kapanis", "Hacim", "Adj")
      sonkapanis <- as.numeric(tail(veri$Kapanis, 1))
      print(colSums(is.na(veri)))
      
      # xts nesnesini data.frame'e dönüştürme
      veri_df <- data.frame(Date = index(veri), coredata(veri))
      
      # Veri ön işleme
      veri_df <- veri_df %>%
        mutate(
          Daily = round((Kapanis - lag(Kapanis, n = 1)) / lag(Kapanis, n = 1) * 100, 2),
          Tomorrow = lead(Kapanis, n = 1),
          Three = lead(Kapanis, n = 3),
          Seven = lead(Kapanis, n = 7)
        )
      print(colSums(is.na(veri_df)))
      
      # Eksik değerleri doldurma
      veri_df <- veri_df %>%
        mutate(across(c(Kapanis, Daily, Tomorrow, Three, Seven), ~ na.locf(.x, na.rm = FALSE))) %>%
        mutate(across(c(Kapanis, Daily, Tomorrow, Three, Seven), ~ na.locf(.x, fromLast = TRUE, na.rm = FALSE))) %>%
        mutate(across(c(Kapanis, Daily, Tomorrow, Three, Seven), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))
      
      # Eksik veri kontrolü
      if (any(is.na(veri_df$Tomorrow))) {
        cat("Eksik değerler tam doldurulamadı, sonraki hisseye geçiliyor (Amerikan):", sembol_for_firebase, "\n")
        next
      }
      
      # Veri bölme
      div <- createDataPartition(veri_df$Tomorrow, p = 0.8, list = FALSE)
      egitim <- veri_df[div, ]
      testing <- veri_df[-div, ]
      
      # Hiperparametre Grid
      tuneGrid_rf <- expand.grid(.mtry = c(2, 4, 6))
      
      # Modelleri Eğitme
      modelrf_tomorrow <- train(Tomorrow ~ ., data = egitim, method = "rf", tuneGrid = tuneGrid_rf, ntree = 500)
      modelrf_three <- train(Three ~ ., data = egitim, method = "rf", tuneGrid = tuneGrid_rf, ntree = 500)
      modelrf_seven <- train(Seven ~ ., data = egitim, method = "rf", tuneGrid = tuneGrid_rf, ntree = 500)
      
      # Lineer Regresyon Modelleri
      modellm_tomorrow <- train(Tomorrow ~ ., data = egitim, method = "lm")
      modellm_three <- train(Three ~ ., data = egitim, method = "lm")
      modellm_seven <- train(Seven ~ ., data = egitim, method = "lm")
      
      # Tahminler
      gelecekveri <- tail(veri_df, 1)
      tahminler <- list(
        Tomorrow_LM = predict(modellm_tomorrow, gelecekveri),
        Three_LM = predict(modellm_three, gelecekveri),
        Seven_LM = predict(modellm_seven, gelecekveri),
        
        Tomorrow_RF = predict(modelrf_tomorrow, gelecekveri),
        Three_RF = predict(modelrf_three, gelecekveri),
        Seven_RF = predict(modelrf_seven, gelecekveri)
      )
      # postresample: RMSE, R-squared, MAE
      performans <- list(
        Tomorrow_LM = postResample(predict(modellm_tomorrow, testing), testing$Tomorrow),
        Three_LM = postResample(predict(modellm_three, testing), testing$Three),
        Seven_LM = postResample(predict(modellm_seven, testing), testing$Seven),
        
        Tomorrow_RF = postResample(predict(modelrf_tomorrow, testing), testing$Tomorrow),
        Three_RF = postResample(predict(modelrf_three, testing), testing$Three),
        Seven_RF = postResample(predict(modelrf_seven, testing), testing$Seven)
      )
      
      cat("Performans Metrikleri (Amerikan):\n")
      for (model_adi in names(performans)) {
        cat("Model:", model_adi, "\n")
        sonuclar <- performans[[model_adi]]
        for (metrik_adi in names(sonuclar)) {
          cat(metrik_adi, ":", sonuclar[[metrik_adi]], "\n")
        }
        cat("--------------------\n")
      }
      
      # Firebase'e gönderilecek veri
      data_to_send <- list(
        Kapanis = sonkapanis,
        Daily = as.numeric(tail(veri_df$Daily, 1)),
        Tahminler = tahminler,
        Performans = performans,
        Tarih = as.character(Sys.Date()),
        Sembol_Ismi = sembol_isim
      )
      
      url <- paste0(firebase_url, "hisse_verileri/amerikan_borsa/", sembol_for_firebase, ".json")
      
      response <- tryCatch({
        httr::PATCH(url, body = toJSON(data_to_send), encode = "json")
      }, error = function(e) {
        cat("Firebase'e veri gönderilirken hata oluştu (Amerikan):", e$message, "\n")
        return(NULL)
      })
      
      if (!is.null(response)) {
        cat(sembol_for_firebase, " için sonuçlar Firebase'e kaydedildi (Amerikan).\n")
      }
    }
    
    end_time <- Sys.time()
    cat("Çalışma Bitti:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
    duration <- difftime(end_time, start_time, units = "secs")
    cat("Çalışma Süresi:", round(duration,2), "saniye \n")
    
    Sys.sleep(86400)
    cat("24 saat beklendi, yeni döngü başlıyor...\n")
    
  }, error = function(e) {
    cat("Beklenmeyen bir hata oluştu, döngü durduruluyor:", e$message, "\n")
    stop("Hata nedeniyle durduruldu.")
  })
}

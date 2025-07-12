# Функция загружает и преобразует данные космической погоды
get_space_weather_data <- function() {
  
  # Список URL-адресов для загрузки JSON-данных
  url <- list(
    mag_5min = "https://services.swpc.noaa.gov/products/solar-wind/mag-5-minute.json",         # Магнитное поле
    kp_now = "https://services.swpc.noaa.gov/products/noaa-planetary-k-index.json",            # Текущий Kp-индекс
    kp_forecast = "https://services.swpc.noaa.gov/products/noaa-planetary-k-index-forecast.json", # Прогноз Kp-индекса
    flux_30d = "https://services.swpc.noaa.gov/products/10cm-flux-30-day.json",                # Поток радиоизлучения 10 см
    plasma_5min = "https://services.swpc.noaa.gov/products/solar-wind/plasma-5-minute.json",   # Параметры солнечного ветра
    aurora = "https://services.swpc.noaa.gov/json/ovation_aurora_latest.json"                  # Прогноз полярных сияний
  )
  
  result <- list()
  
  # Запрос и загрузка JSON-данных
  for (name in names(url)) {
    res <- GET(url[[name]])
    if (status_code(res) == 200) {
      result[[name]] <- fromJSON(content(res, "text", encoding = "UTF-8"))
    } else {
      warning(paste("Ошибка при запросе:", name))
      result[[name]] <- NULL
    }
  }
  
  processed <- list()
  
  # Обработка магнитного поля
  if (!is.null(result$mag_5min)) {
    mag <- as.data.frame(result$mag_5min[-1, ])
    colnames(mag) <- result$mag_5min[1, ]
    processed$mag_5min_df <- mag %>%
      mutate(
        time_tag = as.POSIXct(time_tag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        bz_num = as.numeric(bz_gsm),
        bt_num = as.numeric(bt)
      ) %>% 
      select(time_tag, bz_num, bt_num)
  }
  
  # Обработка параметров солнечного ветра
  if (!is.null(result$plasma_5min)) {
    plasma <- as.data.frame(result$plasma_5min[-1, ])
    colnames(plasma) <- result$plasma_5min[1, ]
    processed$plasma_5min_df <- plasma %>%
      mutate(
        time_tag = as.POSIXct(time_tag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        density = as.numeric(density),
        speed = as.numeric(speed),
        temperature = as.numeric(temperature) - 273.15
      ) %>% 
      select(time_tag, density, speed, temperature)
  }
  
  # Обработка текущего Kp-индекса
  if (!is.null(result$kp_now)) {
    kp_now <- as.data.frame(result$kp_now[-1, ])
    colnames(kp_now) <- result$kp_now[1, ]
    processed$kp_now_df <- kp_now %>%
      mutate(
        time_tag = as.POSIXct(time_tag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        kp_index = as.numeric(Kp)
      ) %>% 
      select(kp_index, time_tag)
  }
  
  # Обработка прогноза Kp-индекса
  if (!is.null(result$kp_forecast)) {
    forecast_kp <- as.data.frame(result$kp_forecast[-1, ])
    colnames(forecast_kp) <- result$kp_forecast[1, ]
    processed$kp_forecast_df <- forecast_kp %>%
      mutate(
        time_tag = as.POSIXct(time_tag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        kp_index = as.numeric(kp)
      ) %>% 
      select(time_tag, kp_index)
  }
  
  # Обработка солнечного радиопотока (10.7 см)
  if (!is.null(result$flux_30d)) {
    flux <- as.data.frame(result$flux_30d[-1, ])
    colnames(flux) <- result$flux_30d[1, ]
    processed$flux_30d_df <- flux %>%
      mutate(
        time_tag = as.POSIXct(time_tag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        flux = as.numeric(flux)
      ) %>% 
      select(time_tag, flux)
  }
  
  # Обработка координат сияния
  if (!is.null(result$aurora)) {
    aurora_coords <- as.data.frame(result$aurora$coordinates)
    colnames(aurora_coords) <- c("lon", "lat", "aurora")
    processed$aurora_map_df <- aurora_coords
  }
  
  return(processed)
}
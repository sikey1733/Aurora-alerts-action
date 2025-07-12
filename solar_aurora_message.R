library(httr)
library(jsonlite)
library(tidyverse)
library(leaflet)

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






# Функция обработки данных, отправка оповещения с показаниями и изображениями
pull_data <- function() {
  
  # Вызов функции запроса
  space_data <- get_space_weather_data()
  
  # Последние данные
  mag_5min <- space_data$mag_5min_df %>% slice_tail(n = 1)
  plasma_5min <- space_data$plasma_5min_df %>% slice_tail(n = 1)
  kp_now <- space_data$kp_now_df %>% slice_tail(n = 1)
  kp_forecast <- space_data$kp_forecast_df %>% slice_tail(n = 1)
  
  # Данные сияния по Чувашии
  aurora_map <- space_data$aurora_map_df %>%
    filter(lon >= 46 & lon <= 49, lat >= 54 & lat <= 57)
  
  # Прогноз сияний на основе данных NOAA
  if (nrow(aurora_map) == 0) {
    message("📭 В Чувашии нет данных по сиянию на текущий момент.")
  } else if (all(aurora_map$aurora == 0)) {
    message("😔 Сияние в Чувашии не ожидается (все значения равны 0).")
  } else if (any(aurora_map$aurora > 0 & aurora_map$aurora < 5)) {
    message("🙂 Вероятность слабого полярного сияния в Чувашии есть!")
  } else if (any(aurora_map$aurora >= 5)) {
    message("🌌 Возможна видимая активность полярного сияния в Чувашии!")
  } else {
    message("ℹ️ Не удалось однозначно определить состояние сияния.")
  }
  

  
# Предварительный прогноз сияний на основе спутника
  
  # Пороговые значения
  bz_threshold <- -5
  speed_threshold <- 400
  density_threshold <- 4
  bt_threshold <- 10

  # Проверка и вычисление вероятности
  if (mag_5min$bz_gsm > 0) {
    probability_msg <- "Низкая вероятность сияния — Bz положительный, магнитосфера не возмущена"
  } else if (mag_5min$bz_gsm <= bz_threshold && 
             plasma_5min$speed >= speed_threshold && 
             plasma_5min$density >= density_threshold && 
             mag_5min$bt >= bt_threshold) {
    probability_msg <- "Высокая вероятность полярного сияния — сильный южный Bz, высокая скорость и плотность ветра"
  } else if (mag_5min$bz_gsm <= bz_threshold && 
             (plasma_5min$speed < speed_threshold || plasma_5min$density < density_threshold)) {
    probability_msg <- "Средняя вероятность — есть южный Bz, но скорость или плотность ветра ниже порогов"
  } else if (mag_5min$bz_gsm > bz_threshold && 
             (plasma_5min$speed >= speed_threshold || plasma_5min$density >= density_threshold)) {
    probability_msg <- "Низкая-умеренная вероятность — положительный Bz, но высокая скорость или плотность ветра"
  } else {
    probability_msg <- "Вероятность полярного сияния низкая по текущим данным"
  }
  
  # Текст сообщения
  msg <- paste0(
    "📡 Самые свежие показания:\n",
    "Компонент магнитного поля по оси Z: ", mag_5min$bz_gsm, " нТл\n",
    "Уровень Bt: ", mag_5min$bt, " нТл\n",
    "Плотность солнечного ветра: ", plasma_5min$density, " частиц/см³\n",
    "Скорость солнечного ветра: ", plasma_5min$speed, " км/с\n",
    "Температура протонов солнечного ветра: ", plasma_5min$temperature, " °C\n",
    "Текущий Кр-индекс: ", kp_now$kp, "\n",
    "Прогноз Кр-индекса: ", kp_forecast$kp, "\n",
    "\n🌟 Предварительная оценка вероятности полярного сияния:\n",
    probability_msg,
    "\n"
  )
  
  
  # График солнечного потока
  flux_30d_plot <- space_data$flux_30d_df %>%
    ggplot() +
    geom_line(aes(x = time_tag, y = flux), color = "steelblue", na.rm = TRUE) +
    geom_smooth(aes(x = time_tag, y = flux), color = "darkred", se = FALSE) +
    scale_y_continuous(name = "Солнечный радиопоток, Вт/м²/Гц") +
    scale_x_datetime(date_breaks = "1 days", date_labels = "%d %b") +
    labs(title = "Показания солнечной активности за последний месяц") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Сохранение графика
  if (!dir.exists("data")) dir.create("data")
  ggsave(filename = "data/flux_plot.png", plot = flux_30d_plot, width = 6, height = 6, units = "in")
  
}

s <-pull_data()

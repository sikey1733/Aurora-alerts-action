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

# Функция обработки данных, отправки оповещения с показаниями и изображением
pull_data <- function(bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                      chat_id = Sys.getenv("TELEGRAM_CHAT_ID")) {
  
  # Обработка ошибок
  if (bot_token == "" || chat_id == "") {
    message("TELEGRAM_TOKEN или TELEGRAM_CHAT_ID не заданы.")
    return(NULL)
  }
  
  # Вызов функции запроса и проверка на ошибки
  space_data <- get_space_weather_data()
  has_error <- FALSE
  if (is.null(space_data$mag_5min_df) || is.null(space_data$plasma_5min_df)) {
    message("Отсутствуют ключевые данные: магнитное поле или параметры солнечного ветра.")
    has_error <- TRUE
  }
  if (is.null(space_data$kp_now_df) || is.null(space_data$kp_forecast_df)) {
    message("Нет данных о Kp-индексе. Прогноз может быть неполным.")
  }
  if (is.null(space_data$flux_30d_df) || is.null(space_data$aurora_map_df)) {
    message("Нет данных для построения графика или прогноза сияния от NOAA.")
  }
  if (has_error) {
    message("Прогноз невозможен из-за отсутствия критичных данных.")
    return(NULL)
  }
  
  # Извлечение и сохранение в переменные актуальных данных
  mag_5min <- space_data$mag_5min_df %>% slice_tail(n = 1)
  plasma_5min <- space_data$plasma_5min_df %>% slice_tail(n = 1)
  kp_now <- space_data$kp_now_df %>% slice_tail(n = 1)
  kp_forecast <- space_data$kp_forecast_df %>% slice_tail(n = 1)
  flux_30d <- space_data$flux_30d_df
    
  # Фильтрация по долготе и широте "Чувашии"
  aurora_map <- space_data$aurora_map_df %>%
    filter(lon >= 46 & lon <= 49, lat >= 54 & lat <= 57)
  
  # Пороговые значения для прогноза на основе данных спутника "DSCOVR"
  bz_threshold <- -5
  speed_threshold <- 400
  density_threshold <- 4
  bt_threshold <- 10
  
  # Прогноз сияний на основе данных NOAA
  # Условия для прогноза
  if (nrow(aurora_map) == 0) {
    probability_NOAA <- "В Чувашии нет данных по сиянию на текущий момент!"
  } else if (all(aurora_map$aurora == 0)) {
    probability_NOAA <- "Сияние в Чувашии не ожидается (все значения равны 0)!"
  } else if (any(aurora_map$aurora > 0 & aurora_map$aurora < 5)) {
    probability_NOAA <- "Вероятность слабого полярного сияния в Чувашии есть!"
  } else if (any(aurora_map$aurora >= 5)) {
    probability_NOAA <- "Возможна видимая активность полярного сияния в Чувашии!"
  } else {
    probability_NOAA <- "Не удалось однозначно определить состояние сияния!"
  }
  
  # Прогноз сияний на основе данных спутника "DSCOVR" в точке лангража L1
  # Условия для прогноза
  if (mag_5min$bz_num > 0) {
    probability_DSCOVR <- "Низкая вероятность сияния — Bz положительный, магнитосфера не возмущена"
  } else if (mag_5min$bz_num <= bz_threshold && 
             plasma_5min$speed >= speed_threshold && 
             plasma_5min$density >= density_threshold && 
             mag_5min$bt_num >= bt_threshold) {
    probability_DSCOVR <- "Высокая вероятность полярного сияния — сильный южный Bz, высокая скорость и плотность ветра"
  } else if (mag_5min$bz_num <= bz_threshold && 
             (plasma_5min$speed < speed_threshold || plasma_5min$density < density_threshold)) {
    probability_DSCOVR <- "Средняя вероятность — есть южный Bz, но скорость или плотность ветра ниже порогов"
  } else if (mag_5min$bz_num > bz_threshold && 
             (plasma_5min$speed >= speed_threshold || plasma_5min$density >= density_threshold)) {
    probability_DSCOVR <- "Низкая-умеренная вероятность — положительный Bz, но высокая скорость или плотность ветра"
  } else {
    probability_DSCOVR <- "Вероятность полярного сияния низкая по текущим данным"
  }
  
  # Сформированный текст сообщения с основными показателями
  msg <- paste0(
    "Самые свежие показания на сегодня:\n",
    "Компонент магнитного поля по оси Z: ", mag_5min$bz_num, " наноТесла\n",
    "Уровень Bt: ", mag_5min$bt_num, " наноТесла\n",
    "Плотность солнечного ветра: ", plasma_5min$density, " частиц/см³\n",
    "Скорость солнечного ветра: ", plasma_5min$speed, " км/с\n",
    "Температура протонов солнечного ветра: ", plasma_5min$temperature, " °C\n",
    "Текущий Кр-индекс: ", kp_now$kp_index, "\n",
    "Прогнозы: \n",
    "Прогноз Кр-индекса: ", kp_forecast$kp_index, "\n",
    "\n Прогноз сияний на основе данных NOAA:\n", probability_NOAA, "\n",
    "\n Прогноз сияний на основе данных спутника DSCOVR: \n", probability_DSCOVR, "\n"
  )
  
  # График солнечного потока за месяц (обновляемый)
  plot <- ggplot(data = flux_30d) +
    geom_line(aes(x = time_tag, y = flux), color = "steelblue", na.rm = TRUE) +
    geom_smooth(aes(x = time_tag, y = flux), color = "darkred", se = FALSE) +
    scale_y_continuous(name = "Солнечный радиопоток, Вт/м²/Гц") +
    scale_x_datetime(date_breaks = "1 days", date_labels = "%d %b") +
    labs(title = "Показания солнечной активности за последний месяц") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Сохранение графика
  if (!dir.exists("output")) dir.create("output")
  ggsave(filename = "output/flux_plot.png", plot = plot, width = 6, height = 6, units = "in")
  
  # Отправка сообщения 
  tryCatch({
    send_telegram_message(bot_token, chat_id, msg)
    message("Сообщение успешно отправлено в Telegram.")
  }, error = function(e) {
    message("Ошибка при отправке текстового сообщения: ", e$message)
  })
  
  # Отправка изображения
  map_path <- "output/flux_plot.png"
  if (file.exists(map_path)) {
    tryCatch({
      send_telegram_image(bot_token, chat_id, image_path = map_path)
      message("Изображение карты отправлено в Telegram.")
    }, error = function(e) {
      message("Ошибка при отправке изображения: ", e$message)
    })
  } else {
    message("Картинка не найдена: ", map_path)
  }
}

# Функция отправки текстового сообщения в Telegram 
send_telegram_message <- function(bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                                  chat_id = Sys.getenv("TELEGRAM_CHAT_ID"), message_text) {
  # Формирует URL для запроса к Telegram Bot API
  url <- paste0("https://api.telegram.org/bot", bot_token, "/sendMessage")
  
  # Выполняет POST-запрос с параметрами: chat_id, текст сообщения, форматирование Markdown
  tryCatch({
    res <- POST(url, body = list(
      chat_id = chat_id,
      text = message_text,
      parse_mode = "Markdown"
    ), encode = "form")
    
    # Проверяет статус ответа
    if (status_code(res) == 200) {
      message("Сообщение успешно отправлено в Telegram.")
      return(TRUE)
    } else {
      warning("Ошибка при отправке сообщения в Telegram: ", content(res, "text"))
      return(FALSE)
    }
  }, error = function(e) {
    # Обработка ошибок при отправке запроса
    warning("Ошибка при попытке отправить сообщение: ", e$message)
    return(FALSE)
  })
}

# Функция отправляет изображение в Telegram 
send_telegram_image <- function(bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                                chat_id = Sys.getenv("TELEGRAM_CHAT_ID"), 
                                image_path = "output/flux_plot.png") {
  
  # Проверяет существование файла изображения
  if (!file.exists(image_path)) {
    warning("Файл для отправки не найден: ", image_path)
    return(FALSE)
  }
  
  # Формирует URL для запроса к Telegram Bot API для отправки фото
  url <- paste0("https://api.telegram.org/bot", bot_token, "/sendPhoto")
  
  # Выполняет POST-запрос с загрузкой файла методом multipart/form-data
  tryCatch({
    res <- POST(
      url,
      body = list(
        chat_id = chat_id,
        photo = upload_file(image_path)
      ),
      encode = "multipart"
    )
    
    # Проверяет статус ответа — 200 значит успешно
    if (status_code(res) == 200) {
      message("Изображение успешно отправлено в Telegram: ", basename(image_path))
      return(TRUE)
    } else {
      warning("Ошибка при отправке изображения в Telegram: ", hontent(res, "text"))
      return(FALSE)
    }
  }, error = function(e) {
    # Обработка ошибок при отправке запроса
    warning("Ошибка при попытке отправить изображение: ", e$message)
    return(FALSE)
  })
}

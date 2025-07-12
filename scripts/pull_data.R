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
  "*Самые свежие показания на сегодня:*\n",
  "• *Bz* (магнитное поле по оси Z): `", mag_5min$bz_num, "` нТл\n",
  "• *Bt* (уровень): `", mag_5min$bt_num, "` нТл\n",
  "• *Плотность ветра:* `", plasma_5min$density, "` частиц/см³\n",
  "• *Скорость ветра:* `", plasma_5min$speed, "` км/с\n",
  "• *Температура:* `", plasma_5min$temperature, "` °C\n",
  "• *Текущий Kp-индекс:* `", kp_now$kp_index, "`\n",
  "• *Прогноз Kp:* `", kp_forecast$kp_index, "`\n\n",
  "*Прогноз NOAA:*\n", probability_NOAA, "\n\n",
  "*Прогноз DSCOVR:*\n", probability_DSCOVR
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
  return(TRUE)
}

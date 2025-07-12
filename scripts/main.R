# Загрузка библиотек
library(httr)
library(jsonlite)
library(tidyverse)

# Подключение  вспомогательных скриптов 
source("scripts/get_space_weather_data.R")
source("scripts/pull_data.R")
source("scripts/send_telegram_message.R")
source("scripts/send_telegram_image.R")

# Функция запуска скрипта
main <- function() {
  result <- pull_data()
  
  if (is.null(result)) {
    message("Ошибка выполнения: нет данных или отправка не удалась!!!")
  } else {
    message("Скрипт успешно завершён!")
  }
}

main()

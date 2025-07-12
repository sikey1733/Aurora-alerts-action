# Функция запуска скрипта
main <- function() {
  result <- pull_data()
  
  if (is.null(result)) {
    message("Ошибка выполнения: нет данных или отправка не удалась!!!")
  } else {
    message("Скрипт успешно завершён!")
  }
}

# Запуск
main()

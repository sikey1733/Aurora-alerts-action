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
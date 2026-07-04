# Функция загружает и преобразует данные космической погоды
get_space_weather_data <- function() {

  # Список URL-адресов для загрузки JSON-данных
  url <- list(
    mag_5min = "https://services.swpc.noaa.gov/json/rtsw/rtsw_mag_1m.json",                  # Магнитное поле
    kp_now = "https://services.swpc.noaa.gov/products/noaa-planetary-k-index.json",          # Текущий Kp-индекс
    kp_forecast = "https://services.swpc.noaa.gov/products/noaa-planetary-k-index-forecast.json", # Прогноз Kp-индекса
    flux_30d = "https://services.swpc.noaa.gov/products/10cm-flux-30-day.json",              # Поток радиоизлучения 10 см
    plasma_5min = "https://services.swpc.noaa.gov/json/rtsw/rtsw_wind_1m.json",              # Параметры солнечного ветра
    aurora = "https://services.swpc.noaa.gov/json/ovation_aurora_latest.json"                # Прогноз полярных сияний
  )

  result <- list()

  # Запрос и загрузка JSON-данных
  for (name in names(url)) {
    res <- tryCatch(
      httr::GET(url[[name]], timeout(30)),
      error = function(e) {
        warning(paste("Ошибка запроса", name, ":", e$message))
        NULL
      }
    )

    if (!is.null(res) && httr::status_code(res) == 200) {
      result[[name]] <- tryCatch(
        jsonlite::fromJSON(
          httr::content(res, "text", encoding = "UTF-8")
        ),
        error = function(e) {
          warning(paste("Ошибка чтения JSON:", name))
          NULL
        }
      )
    } else {
      warning(paste("Ошибка при запросе:", name))
      result[[name]] <- NULL
    }
  }

  processed <- list()

  # ---------------------------------------------------------
  # Обработка магнитного поля
  # ---------------------------------------------------------
  if (!is.null(result$mag_5min)) {

    mag <- as.data.frame(result$mag_5min)

    processed$mag_5min_df <- mag %>%
      dplyr::filter(active == TRUE) %>%
      dplyr::mutate(
        time_tag = as.POSIXct(
          substr(time_tag, 1, 19),
          format = "%Y-%m-%dT%H:%M:%S",
          tz = "UTC"
        ),
        bz_num = as.numeric(bz_gsm),
        bt_num = as.numeric(bt)
      ) %>%
      dplyr::select(
        time_tag,
        bz_num,
        bt_num
      )
  }

  # ---------------------------------------------------------
  # Обработка параметров солнечного ветра
  # ---------------------------------------------------------
  if (!is.null(result$plasma_5min)) {

    plasma <- as.data.frame(result$plasma_5min)

    processed$plasma_5min_df <- plasma %>%
      dplyr::filter(active == TRUE) %>%
      dplyr::mutate(
        time_tag = as.POSIXct(
          substr(time_tag, 1, 19),
          format = "%Y-%m-%dT%H:%M:%S",
          tz = "UTC"
        ),
        density = as.numeric(proton_density),
        speed = as.numeric(proton_speed),
        temperature = as.numeric(proton_temperature)
      ) %>%
      dplyr::select(
        time_tag,
        density,
        speed,
        temperature
      )
  }

  # ---------------------------------------------------------
  # Обработка текущего Kp-индекса
  # ---------------------------------------------------------
  if (!is.null(result$kp_now)) {

    kp_now <- as.data.frame(result$kp_now)

    if (!"time_tag" %in% colnames(kp_now)) {
      colnames(kp_now) <- kp_now[1, ]
      kp_now <- kp_now[-1, ]
    }

    processed$kp_now_df <- kp_now %>%
      dplyr::mutate(
        time_tag = as.POSIXct(
          substr(time_tag, 1, 19),
          format = "%Y-%m-%dT%H:%M:%S",
          tz = "UTC"
        ),
        kp_index = as.numeric(
          if ("Kp" %in% colnames(kp_now)) Kp else kp
        )
      ) %>%
      dplyr::select(
        kp_index,
        time_tag
      )
  }

  # ---------------------------------------------------------
  # Обработка прогноза Kp
  # ---------------------------------------------------------
  if (!is.null(result$kp_forecast)) {

    forecast_kp <- as.data.frame(result$kp_forecast)

    if (!"time_tag" %in% colnames(forecast_kp)) {
      colnames(forecast_kp) <- forecast_kp[1, ]
      forecast_kp <- forecast_kp[-1, ]
    }

    processed$kp_forecast_df <- forecast_kp %>%
      dplyr::mutate(
        time_tag = as.POSIXct(
          substr(time_tag, 1, 19),
          format = "%Y-%m-%dT%H:%M:%S",
          tz = "UTC"
        ),
        kp_index = as.numeric(
          if ("kp" %in% colnames(forecast_kp)) kp else Kp
        )
      ) %>%
      dplyr::select(
        time_tag,
        kp_index
      )
  }

  # ---------------------------------------------------------
  # Обработка солнечного радиопотока
  # ---------------------------------------------------------
  if (!is.null(result$flux_30d)) {

    flux <- as.data.frame(result$flux_30d)

    if (!"time_tag" %in% colnames(flux)) {
      colnames(flux) <- flux[1, ]
      flux <- flux[-1, ]
    }

    processed$flux_30d_df <- flux %>%
      dplyr::mutate(
        time_tag = as.POSIXct(
          substr(time_tag, 1, 19),
          format = "%Y-%m-%dT%H:%M:%S",
          tz = "UTC"
        ),
        flux = as.numeric(
          if ("flux" %in% colnames(.)) {
            .data[["flux"]]
          } else if ("f10.7" %in% colnames(.)) {
            .data[["f10.7"]]
          } else {
            NA
          }
        )
      ) %>%
      dplyr::select(
        time_tag,
        flux
      )
  }

  # ---------------------------------------------------------
  # Обработка координат полярного сияния
  # ---------------------------------------------------------
  if (!is.null(result$aurora)) {

    aurora_coords <- as.data.frame(
      result$aurora$coordinates
    )

    colnames(aurora_coords) <- c(
      "lon",
      "lat",
      "aurora"
    )

    processed$aurora_map_df <- aurora_coords
  }

  return(processed)
}

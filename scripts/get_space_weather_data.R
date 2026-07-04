get_space_weather_data <- function() {

  # URL источников данных NOAA
  url <- list(
    mag_5min = "https://services.swpc.noaa.gov/json/rtsw/rtsw_mag_1m.json",
    kp_now = "https://services.swpc.noaa.gov/products/noaa-planetary-k-index.json",
    kp_forecast = "https://services.swpc.noaa.gov/products/noaa-planetary-k-index-forecast.json",
    flux_30d = "https://services.swpc.noaa.gov/products/10cm-flux-30-day.json",
    plasma_5min = "https://services.swpc.noaa.gov/json/rtsw/rtsw_wind_1m.json",
    aurora = "https://services.swpc.noaa.gov/json/ovation_aurora_latest.json"
  )

  result <- list()

  # Загрузка данных
  for (name in names(url)) {

    res <- tryCatch(
      httr::GET(
        url[[name]],
        httr::timeout(30)
      ),
      error = function(e) {
        warning(sprintf(
          "Ошибка подключения %s: %s",
          name,
          e$message
        ))
        return(NULL)
      }
    )

    if (is.null(res)) {
      result[[name]] <- NULL
      next
    }

    if (httr::status_code(res) != 200) {
      warning(sprintf(
        "HTTP ошибка %s: %s",
        name,
        httr::status_code(res)
      ))
      result[[name]] <- NULL
      next
    }

    result[[name]] <- tryCatch(
      jsonlite::fromJSON(
        httr::content(
          res,
          as = "text",
          encoding = "UTF-8"
        ),
        simplifyDataFrame = TRUE
      ),
      error = function(e) {
        warning(sprintf(
          "Ошибка чтения JSON %s: %s",
          name,
          e$message
        ))
        return(NULL)
      }
    )
  }

  processed <- list()

  # ==========================================================
  # Магнитное поле
  # ==========================================================
  if (!is.null(result$mag_5min) &&
      nrow(result$mag_5min) > 0) {

    mag <- as.data.frame(result$mag_5min)

    if ("active" %in% names(mag)) {
      mag <- mag %>%
        dplyr::filter(active == TRUE)
    }

    processed$mag_5min_df <- mag %>%
      dplyr::mutate(
        time_tag = as.POSIXct(
          time_tag,
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

  # ==========================================================
  # Солнечный ветер
  # ==========================================================
  if (!is.null(result$plasma_5min) &&
      nrow(result$plasma_5min) > 0) {

    plasma <- as.data.frame(result$plasma_5min)

    if ("active" %in% names(plasma)) {
      plasma <- plasma %>%
        dplyr::filter(active == TRUE)
    }

    processed$plasma_5min_df <- plasma %>%
      dplyr::mutate(
        time_tag = as.POSIXct(
          time_tag,
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

  # ==========================================================
  # Текущий Kp
  # ==========================================================
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

  # ==========================================================
  # Прогноз Kp
  # ==========================================================
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

  # ==========================================================
  # Поток F10.7
  # ==========================================================
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
          if ("flux" %in% names(.)) {
            .data[["flux"]]
          } else if ("f10.7" %in% names(.)) {
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

  # ==========================================================
  # Карта сияний
  # ==========================================================
  if (!is.null(result$aurora) &&
      "coordinates" %in% names(result$aurora)) {

    aurora_coords <- as.data.frame(
      result$aurora$coordinates
    )

    if (ncol(aurora_coords) >= 3) {
      colnames(aurora_coords)[1:3] <- c(
        "lon",
        "lat",
        "aurora"
      )

      processed$aurora_map_df <- aurora_coords
    }
  }

  cat("\nУспешно загружены:\n")
  print(names(processed))

  if (!is.null(warnings())) {
    print(warnings())
  }

  return(processed)
}

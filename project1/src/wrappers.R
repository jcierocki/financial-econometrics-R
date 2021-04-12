# Title     : Wrappers
# Objective : Wrapper prepared to make the analysis easier
# Created by: Jakub Cierocki
# Created on: 12.04.2021

require(tidyverse)
require(lubridate)
require(forecast)

calc_irf <- function(arima_model) {
  ARMAtoMA(
    ar = arima_model$coef[arima_model$coef %>% names() %>% str_detect("^ar")],
    ma = arima_model$coef[arima_model$coef %>% names() %>% str_detect("^ma")],
    lag.max = 10
  ) %>% `class<-`("irf")
}

plot.irf <- function(irf) {
  n <- length(irf)

  tibble(lag = 1L:n, coef = as.numeric(irf)) %>%
    ggplot(aes(x = lag, y = coef)) +
    geom_line() +
    geom_hline(yintercept = 0) +
    labs(x = "horizon", y = NULL, title = "IRF for ARIMA model") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = 1L:n)
}

arima_forecast <- function(.tbl, .y, .p, .q, ...) {
  n <- nrow(.tbl)
  y <- .tbl %>% pull(!!rlang::enquo(.y)) %>% as.ts()
  y_train <- y[-n]

  arima_fun <- function(.method) {
    Arima(
      y_train,
      order = c(.p, 1L, .q),
      seasonal = F,
      include.drift = F,
      method = .method,
      optim.method = "Nelder-Mead"
    )
  }

  arima_model <- tryCatch(
    arima_fun("CSS-ML"),
    error = function (e) arima_fun("CSS") %>% suppressWarnings()
  )

  suppressWarnings({
    fcst <- tryCatch({
      arima_model %>%
        forecast(h = 1L) %>%
        purrr::pluck("mean") %>%
        as.numeric()
    },
      error = function (e) NA
    )
  })

  tibble(fcst = fcst, err = y[n] - fcst)
}

rw_forecast <- function(.tbl, .y, ...) {
  n <- nrow(.tbl)
  y <- .tbl %>% pull(!!rlang::enquo(.y)) %>% as.ts()
  y_train <- y[-n]

  fcst <- y_train %>%
    rwf(
      h = 1L,
      drift = T
    ) %>%
    purrr::pluck("mean")

  tibble(fcst = fcst, err = y[n] - fcst)
}

var_forecast <- function(.tbl, .y1, .y2, .p, ...) {
  n <- nrow(.tbl)
  y_tbl <- .tbl %>% select(!!rlang::enquo(.y1), !!rlang::enquo(.y2))
  y_train <- y_tbl %>% slice(-n)
  y_forecast <- y_tbl %>% slice(n)

  fcst <- y_train %>%
    VAR(
      p = .p,
      type = "const"
    ) %>%
    predict(n.ahead = 1L)

  fcst_val <- fcst$fcst$nl[1]

  tibble(fcst = fcst_val, err = y_forecast %>% pull(!!rlang::enquo(.y1)) - fcst_val)
}

sumup_arima <- function(arima) {
  model_summary <- invisible(summary(arima)) %>% as.numeric()
  model_coefs <- invisible(lmtest::coeftest(arima))

  model_name <- "ARIMA(%i, %i, %i)" %>% sprintf(
    length(arima$model$phi),
    ifelse(length(arima$model$Delta) > 0, arima$model$Delta, 0),
    length(arima$model$theta)
  )

  if (!is.na(arima$coef["intercept"]) && length(arima$model$Delta) > 0) {
    model_name <- model_name %>% str_c("z niezerową stałą")
  }

  coef_names <- model_coefs %>% attr("dimnames") %>% purrr::pluck(1)
  coef_table <- model_coefs %>%
    `class<-`("matrix") %>%
    as_tibble() %>%
    select(1,4) %>%
    `colnames<-`(c("estimate", "p-value")) %>%
    mutate(`współczynnik` = coef_names, .before = 1)

  metric_table = tibble(
    miara = c("loglik", "aic", "aicc", "bic", "rmse", "mase"),
    `wartość` = c(arima$loglik, arima$aic, arima$aicc, arima$bic, model_summary[2], model_summary[6])
  )

  list(
    specyfikacja = model_name,
    "parameter estimates" = coef_table,
    "information criterion and godness of fit" = metric_table
  )
}
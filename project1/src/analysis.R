# Title     : Project I - the Dutch bonds yield analysis
# Objective : Analyse the Dutch bonds yields using t
# Created by: jcierocki
# Created on: 12.04.2021

require(forecast)
require(vars)
require(tidyverse)
require(lubridate)
require(tseries)
require(tsibble)
require(knitr)
require(slider)
require(furrr)

rm(list = ls())

future::plan(multicore, workers = 7L)

set.seed(69)

source("project1/src/wrappers.R")

## initial analysis

### data

df <- inner_join(
  read_csv("project1/data/nl_10y_bond_yield.csv") %>%
    rename(date = Data, nl = Zamkniecie) %>%
    dplyr::select(date, nl),

  read_csv2("project1/data/eu_10y_bond_yield.csv", col_names = F) %>%
    slice(., 6L:nrow(.)) %>%
    rename(date = X1, eu = X2) %>%
    type_convert(locale = locale(decimal_mark = ',')),

  by = "date"
) %>% dplyr::select(date, eu, nl)

N <- 2006L:2019L %>% map_int(~ sum(year(df$date) == .x)) %>% median()

### plots, ACF and unit root tests

df %>%
  pivot_longer(c(nl, eu)) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  labs(x = "date", y = NULL, title = "Duthc 10Y bonds yield") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  scale_color_manual(name = "emitent", values = c("#F8766D", "#00BFC4"), labels = c("The Netherlands", "EU")) +
  theme(plot.title = element_text(hjust = 0.5))

# ggsave("project1/output/ts.png")

corrplot_df <- df %>%
  mutate(., fake_date = date[1] + days(0:(nrow(.) - 1L))) %>%
  as_tsibble(index = fake_date)

corrplot_df %>%
  feasts::ACF(nl, lag_max = 100) %>%
  autoplot() +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(y = NULL, title = "ACF plot") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("project1/output/acf.png")

corrplot_df %>%
  feasts::PACF(nl, lag_max = 10) %>%
  autoplot() +
  scale_x_continuous(breaks = 1:10) +
  labs(y = NULL, title = "PACF plot") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("project1/output/pacf.png")

###

df$nl %>%
  pp.test() %>%
{ tibble(alt_hypothesis = .[["alternative"]], pval = .[["p.value"]]) } %>%
  kable(caption = "Phillips-Peron unit root test")

df$nl %>%
  kpss.test(null = "Level") %>%
{ tibble(null_hypothesis = "level", pval = .[["p.value"]]) } %>%
  kable(caption = "KPSS unit root test")

df$nl %>% adf.test()

df$nl %>%
  diff() %>%
  pp.test() %>%
{ tibble(alt_hypothesis = .[["alternative"]], pval = .[["p.value"]]) } %>%
  kable(caption = "Phillips-Peron unit root test for first differences")

df$nl %>% diff() %>% adf.test()

### ARMA and ARIMA models, IRF

arma1 <- df$nl %>% auto.arima(
  d = 0L,
  seasonal = F,
  stepwise = F,
  trace = F,
  test = "pp",
  parallel = T,
  num.cores = 7,
)

sumup_arima(arma1) %>% kable(digits = 4)

arma1 %>% calc_irf() %>% autoplot() + ggtitle("IRF for the ARMA(0,5) model")
ggsave("project1/output/arma_irf.png")

arima1 <- df$nl %>% auto.arima(
  d = 1L,
  seasonal = F,
  stepwise = F,
  trace = F,
  test = "pp",
  allowdrift = F,
  parallel = T,
  num.cores = 7,
)

sumup_arima(arima1) %>% kable(digits = 4)

arima1 %>% calc_irf() %>% autoplot() + ggtitle("IRF for the ARIMA(1,1,0) model")
ggsave("project1/output/arima_irf.png")

## VAR models

best_models <- df %>% select(-date) %>% zoo(., order.by = df$date) %>% VARselect()
best_models %>% kable()

freq_tab <- table(best_models$selection)
p_best <- names(freq_tab)[which.max(freq_tab)] %>% as.integer()

var1 <- df %>% select(-date) %>% zoo(., order.by = df$date) %>% VAR(p = p_best)
summary(var1)
serial.test(var1, lags.pt = 10, type="PT.adjusted")

### casuality

causality(var1, cause="nl")
causality(var1, cause="eu")

###

bmat <- c(NA, NA, 0, NA) %>% matrix(ncol = 2)
svar1 <- SVAR(var1, Amat = NULL, Bmat = bmat)
svar2 <- BQ(var1)

svar1$B %>% kable(caption = "Cholesky decomposition short-run impact matrix")
svar2$B %>% kable(caption = "BQ contemporaneous impact matrix")
svar2$LRIM %>% kable(caption = "BQ long-run impact matrix")

srim <- Phi(svar1, nstep=N)
srim[,,1]

### irf

irf(svar1, n.ahead=N, cum = F, boot = F) %>% plot(main = "short-run restrictions IRF for VAR(9) model")
ggsave("project1/output/var_irf_cholesky.png")

irf(svar2, n.ahead=N, cum = F, boot = F) %>% plot(main = "Blanchard-Quah restrictions IRF for VAR(9) model")
ggsave("project1/output/var_irf_bq.png")

### fevd

fevd(var1, n.ahead = 60) %>% plot()
ggsave("project1/output/fevd1.png")

## forecasts

### choosing best model

N1 <- N

df_slided <- df %>%
  select(eu, nl) %>%
  slide(~ .x, .before = N1, .complete = T) %>%
  set_names(df$date) %>%
  keep(~ !is.null(.x))

# mase_scaling_factor <-  df$nl[(N1 + 1L):nrow(df)] %>% diff() %>% abs() %>% mean()

# maes_arima_2years <- tibble(p = 0L:4L, q = 0L:4L) %>%
#   expand(p, q) %>%
#   filter(p + q > 0, p + q <= 4) %>%
#   pmap_dfr(~ {
#     p <- ..1
#     q <- ..2
#
#     results <- df_slided %>%
#       future_map_dfr(~ arima_forecast(.x, nl, .p = p, .q = q))
#
#     if (results$fcst %>% is.null() %>% any()) {
#       mae <- NA
#     } else {
#       mae <- results$err %>% abs() %>% mean()
#     }
#
#     print(sprintf("%i|%i: %f", p, q, mae))
#
#     tibble(p = p, q = q, mae = mae, mase = mae / mase_scaling_factor)
#   })

# results_var <- 1L:10L %>%
#   map_dfr(~ {
#     p <- .x
#     results <- df_slided %>%
#       future_map_dfr(~ var_forecast(.x, nl, eu, p))
#
#     mae <- results$err %>% abs() %>% mean()
#
#     print(sprintf("%i: %f", p, mae))
#
#     tibble(p = p, mae = mae, mase = mae / mase_scaling_factor)
#   })

### actual forecasts

results_full <- bind_rows(
  df_slided %>% future_map_dfr(~ rw_forecast(.x, nl)) %>% mutate(model = "RW", date = names(df_slided) %>% ymd()),
  df_slided %>% future_map_dfr(~ arima_forecast(.x, nl, 0L, 1L)) %>% mutate(model = "ARIMA(0,1,1)", date = names(df_slided) %>% ymd()),
  df_slided %>% future_map_dfr(~ var_forecast(.x, nl, eu, 1L)) %>% mutate(model = "VAR(1)", date = names(df_slided) %>% ymd()),
) %>% mutate(model = as.factor(model))

rw_err <- results_full %>% filter(model == "RW") %>% pull(err)

model_comparison <- results_full$model %>%
  unique() %>%
  map_dfr(~ {
    df_temp <- results_full %>% filter(model == .x)

    tibble(
      model = .x,
      me = mean(df_temp$err),
      mae = df_temp$err %>% abs() %>% mean(),
      rmse = df_temp$err^2 %>% mean() %>% sqrt(),
      dm = ifelse(.x == "RW", NA, dm.test(df_temp$err, rw_err, alternative = "two.sided", power = 2)$p.value)
    )
})

model_comparison %>% kable()

results_full2 <- bind_rows(
  results_full %>% rename(value = fcst),
  df_slided %>%
    map_dfr(~ .x %>% tail(n = 1)) %>%
    select(nl) %>%
    rename(value = nl) %>%
    mutate(
      err = NA,
      model = as.factor("REAL DATA"),
      date = names(df_slided) %>% ymd()
    )
)

results_full2 %>%
  filter(year(date) == 2020L) %>%
  ggplot(aes(x = date, y = value, color = model)) +
  geom_line() +
  labs(x = "../data", y = NULL, title = "Real data vs forecasts for 2020", color = "variable") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("project1/output/forecasts_2020.png")

### comaprison with eu forecasts

df_eu_commision_forecasts <- read_csv("project1/data/eu_commission_forecasts.csv") %>%
  filter(LOCATION == "NLD") %>%
  mutate(TIME = str_split(TIME, "Q") %>% map_chr(~ str_c(.x[1], as.integer(.x[2]) * 3L)) %>% ym()) %>%
  select(TIME, Value) %>%
  rename(date = TIME, value = Value) %>%
  filter(date > ymd("2021-03-31"))

df_bizday_shift <- ts(start = c(2021, 4), end = c(2022, 12), frequency = 12) %>%
  forecast::bizdays(FinCenter = "London") %>%
  accumulate(sum) %>%
  `names<-`(., names(.) %>% ym()) %>%
  identity()

interesting_periods <- df_bizday_shift[as.character(df_eu_commision_forecasts$date)]

idx_fcst_last <- which(df$date == ymd("2021-03-31"))
df_train_future_fcst <- df %>% slice((idx_fcst_last-N+1L):idx_fcst_last) %>% select(date, eu, nl)
horizon <- df_bizday_shift[as.character(max(df_eu_commision_forecasts$date))] %>% as.integer()

df_fcst_future <- tibble(
  `ARIMA(0,1,1) 1-year window` = df_train_future_fcst$nl %>%
    as.ts() %>%
    Arima(order = c(0, 1, 1), include.drift = F, seasonal = F, optim.method = "Nelder-Mead") %>%
    forecast(h = horizon) %>%
    purrr::pluck("mean") %>%
    as.numeric(),
  `VAR(1) 1-year window` = df_train_future_fcst %>%
    select(-date) %>%
    VAR(p = 1L, type = "const") %>%
    predict(n.ahead = horizon) %>%
    purrr::pluck("fcst") %>%
    purrr::pluck("nl") %>%
    .[,1],
  `ARIMA(1,1,0) whole dataset` = arima1 %>%
    forecast(h = horizon) %>%
    purrr::pluck("mean") %>%
    as.numeric(),
  `VAR(9) whole dataset` = var1 %>%
    predict(n.ahead = horizon) %>%
    purrr::pluck("fcst") %>%
    purrr::pluck("nl") %>%
    .[,1]
) %>%
  rowid_to_column() %>%
  filter(rowid %in% as.integer(interesting_periods)) %>%
  mutate(date = names(interesting_periods) %>% ymd()) %>%
  select(-rowid) %>%
  pivot_longer(cols = c(`ARIMA(0,1,1) 1-year window`, `VAR(1) 1-year window`, `ARIMA(1,1,0) whole dataset`, `VAR(9) whole dataset`), names_to = "model")

df_future_full <- bind_rows(
  df_eu_commision_forecasts %>% mutate(model = "EU Commission"),
  df_fcst_future
) %>%
  mutate(model = as.factor(model)) %>%
  select(date, model, value) %>%
  arrange(model, date)

df_future_full %>% kable()

df_future_full %>%
  ggplot(aes(x = date, y = value, color = model)) +
  geom_line() +
  labs(x = "../data", y = NULL, title = "Comparison of the VAR and ARIMA forecasts with the EU Commission ones", color = "variable") +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

ggsave("project1/output/eu_commission_comparison.png")


install.packages("readxl")
library(readxl)
install.packages("tsibble")
library(tsibble)
install.packages("tibble")
library(tibble)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)
install.packages("lubridate")
library(lubridate)
install.packages("imputeTS")
library(imputeTS)
install.packages("feasts")
library(feasts)
install.packages("tidyverse")
library(tidyverse)
install.packages("lmtest")
library(lmtest)
install.packages("nonlinearTseries")
library(nonlinearTseries)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)

setwd("E:/1) STUDY/ІТА/Практичні/1/Дані.xlsx/Дані.xlsx")
cryptos <- read_xlsx("cryptos.xlsx")

str(cryptos)
glimpse(cryptos, width=60)
cryptos$ds <- ymd(cryptos$ds)
glimpse(cryptos, width=60)
cryptos %>%
  group_by(coin) %>%
  mutate(label = ifelse(ds == max(ds), coin, NA)) %>%
  ggplot(., aes(ds, y, group = coin)) +
  geom_line() +
  geom_text_repel(aes(label = label),
                  size = 3, nudge_x = 50,
                  segment.size = 0.4,
                  segment.color = "gray60",
                  point.padding = 0.2,
                  force = 5, na.rm = TRUE) +
  scale_y_log10() +
  theme_minimal() +
  xlim(c(as.Date("2018-01-01"), as.Date("2020-06-01")))

unique(cryptos$coin)

(cryptos <- as_tsibble(cryptos, key = coin, index = ds))
has_gaps(cryptos)
na_positions <- which(is.na(cryptos), arr.ind = TRUE)
cryptos_na_dropped <- cryptos %>%
  na.omit()
has_gaps(cryptos_na_dropped)

scan_gaps(cryptos_na_dropped)

(gaps <- count_gaps(cryptos_na_dropped))

cryptos_na_filled <- cryptos %>%
  group_by_key() %>%
  mutate(y = na_locf(y))
cryptos_na_filled %>%
  ggplot(., aes(ds, y, color = coin)) +
  geom_line() + scale_y_log10() +
  theme_minimal()

.monthly <- cryptos %>%
  group_by_key() %>%
  index_by(year_month = ~ yearmonth(.)) %>%
  summarise(
    avg_y = mean(y),
    n = n()
  )
.monthly
.monthly %>%
  mutate(label=ifelse(year_month==max(year_month),coin,NA)) %>%
  ggplot(.,aes(year_month,avg_y,group=coin))+
  geom_line()+scale_y_log10()+
  geom_text_repel(aes(label=label),
                  size=3,nudge_x = 50,
                  segment.size=0.4,
                  segment.color="gray60",point.padding = 0.2,
                  force = 4,na.rm = TRUE)+
    theme_minimal()

tron_price <- dplyr::select(dplyr::filter(cryptos, coin == "tron"), y, ds)
summary(tron_price)

statsNA(tron_price$y)
tron_price$y <- na_locf(tron_price$y)
plot(tron_price$ds, tron_price$y, type = "l")
abline(reg=lm(tron_price$y~tron_price$ds, tron_price), col="blue")

tron_price %>% model(STL(y ~ trend(window = 30) + season(window = 13), robust = TRUE)) %>% components() %>% autoplot()

unitroot_ndiffs(tron_price$y)
unitroot_nsdiffs(tron_price$y)

tron_price %>%
  index_by(w = yearweek(ds)) %>%
  gg_season(y, period = "week") + theme_minimal()

cryptos %>%
  filter(coin == "tron") %>%
  gg_lag(geom = "point") + theme_minimal()

acf_plot <- acf(tron_price$y, lag.max=35, col="purple")

pacf_plot <- pacf(tron_price$y, lag.max=35, col="purple")

model <- lm(tron_price$y~tron_price$ds)
dw_test_result <- dwtest(model)
print(dw_test_result)

if(dw_test_result$statistic < 2) {
 print("Є позитивна автокореляція в залишках моделі")
 } else if(dw_test_result$statistic > 2) {
 print("Є негативна автокореляція в залишках моделі")
 } else {
 print("Автокореляція в залишках моделі відсутня")

if (dw_test_result$p.value < 0.05) {
 print("Є підстава відхилити нульову гіпотезу, бо автокореляція існує")
 } else {
 print("Немає підстав відхиляти нульову гіпотезу, бо автокореляція відсутня")
 }

bg_test_result <- bgtest(model, order=1)
print(bg_test_result)

if (bg_test_result$p.value < 0.05) {
 print("Є підстава відхилити нульову гіпотезу, бо автокореляція існує")
 } else {
 print("Немає підстав відхиляти нульову гіпотезу, бо автокореляція відсутня")
 }

nonlinearityTest(tron_price$y, TRUE)

adf_test_result <- tseries::adf.test(tron_price$y)
print(adf_test_result)

if (adf_test_result$p.value <= 0.05) {
 print("Відхилено нульову гіпотезу. Часовий ряд стаціонарний")
 } else {
 print("Нульову гіпотезу не відхилено. Часовий ряд є нестаціонарним")
 }

kpss_test_result <- tseries::kpss.test(tron_price$y)
print(kpss_test_result)

if (kpss_test_result$p.value > 0.05) {
 print("Нульову гіпотезу не відхилено, часовий ряд стаціонарний")
 } else {
 print("Нульову гіпотезу відхилено, часовий ряд нестаціонарний")
 }

pp_test_result <- tseries::pp.test(tron_price$y)
print(pp_test_result)

if (pp_test_result$p.value <= 0.05) {
 print("Відхилено нульову гіпотезу. Часовий ряд стаціонарний")
 } else {
 print("Не відхилено нульову гіпотезу. Часовий ряд нестаціонарний")
 }

y_tron_price <- log10(tron_price$y)
plot(y_tron_price, type = "l")
tsdisplay(y_tron_price)

tron_price_d <- diff(tron_price$y)
plot(tron_price_d, type = "l")
tsdisplay(tron_price_d)

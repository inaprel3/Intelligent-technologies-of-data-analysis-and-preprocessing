# Встановлення необхідних пакетів для аналізу часових рядів та графіків.
install.packages("quantmod")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("zoo")
install.packages("xts")
install.packages("forecast")
install.packages("lmtest")
install.packages("gridExtra")
install.packages("tseries")
install.packages("Metrics")
install.packages("AICcmodavg")
install.packages("np")

# Підключення встановлених бібліотек для подальшої роботи.
library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(xts)
library(forecast)
library(lmtest)
library(gridExtra)
library(tseries)
library(Metrics)
library(AICcmodavg)
library(np)

# Встановлення локалі для коректного відображення дат на графіках.
Sys.setlocale("LC_TIME", "C")

# Завантаження даних про акції Newell Brands Inc. з Yahoo! Finance. Період даних: з 1 січня 2023 по 31 грудня 2023 року.
getSymbols("NWL", src = "yahoo", from = "2023-01-01", to = "2023-12-31")

# Виведення перших і останніх значень завантаженого набору даних для перевірки.
head(NWL)
tail(NWL)

# Виділення цін закриття акцій у окремий часовий ряд.
nwl_close <- Cl(NWL)

# Побудова графіка цін закриття за 2023 рік.
ggplot(data = fortify.zoo(nwl_close), aes(x = Index, y = NWL.Close)) +
  geom_line(color = "blue") +
  labs(title = "Ціна закриття акцій Newell Brands Inc. за 2023 рік", 
       x = "Дата", y = "Ціна закриття (USD)") +
  theme_minimal()

# Підрахунок кількості пропущених значень у ряді.
sum(is.na(nwl_close))  

# Видалення всіх пропущених значень із ряду.
nwl_close_clean <- na.omit(nwl_close)  

# Перевірка, чи залишились пропуски після очищення.
sum(is.na(nwl_close_clean))  

# Виведення кількості записів після очищення.
print(paste("Кількість записів після очищення:", length(nwl_close_clean)))

# Побудова графіка очищеного часового ряду.
ggplot(data = fortify.zoo(nwl_close_clean), aes(x = Index, y = NWL.Close)) +
  geom_line(color = "darkgreen") +  # Лінія графіка темно-зеленого кольору.
  labs(title = "Ціна закриття акцій Newell Brands Inc. за 2023 рік (очищені дані)",
       x = "Дата", y = "Ціна закриття (USD)") +
  theme_minimal()

# Графік часового ряду
time_series_plot <- ggplot(data = fortify.zoo(nwl_close_clean), aes(x = Index, y = NWL.Close)) +
  geom_line(color = "darkgreen") + 
  labs(title = "Ціна закриття акцій Newell Brands Inc. за 2023 рік (очищені дані)",
       x = "Дата", y = "Ціна закриття (USD)") +
  theme_minimal()

# Графік ACF
acf_plot <- ggAcf(nwl_close_clean, main = "Автокореляційна функція (ACF)")

# Графік PACF
pacf_plot <- ggPacf(nwl_close_clean, main = "Приватна автокореляційна функція (PACF)")

# Комбінування графіків
grid.arrange(time_series_plot, acf_plot, pacf_plot, ncol = 1)

# Виконання декомпозиції за допомогою STL з числовим s.window
decomposed_stl <- stl(nwl_ts, s.window = 7)

# Перевірка структури часового ряду
str(nwl_ts)

# Візуалізація результатів декомпозиції
plot(decomposed_stl)

# Тест Дікі-Фуллера (ADF) для перевірки стаціонарності
adf_result <- adf.test(nwl_ts, alternative = "stationary")
print(adf_result)

# Висновок про стаціонарність
if (adf_result$p.value < 0.05) {
  print("Ряд є стаціонарним (відхиляємо нульову гіпотезу про нестаціонарність).")
} else {
  print("Ряд не є стаціонарним (приймаємо нульову гіпотезу про нестаціонарність).")
}

# Тест Квіка-Бокса для перевірки автокореляції
box_test_result <- Box.test(nwl_ts, type = "Ljung-Box", lag = 20)
print(box_test_result)

# Висновок про наявність автокореляції (нелінійності)
if (box_test_result$p.value < 0.05) {
  print("Часовий ряд має автокореляцію (можлива нелінійність).")
} else {
  print("Автокореляція незначна (немає явної нелінійності).")
}

# Виконання декомпозиції за допомогою STL
decomposed_stl <- stl(nwl_ts, s.window = 7)

# Отримання окремих компонент: тренд, сезонність, залишки
trend_component <- decomposed_stl$time.series[, "trend"]
seasonal_component <- decomposed_stl$time.series[, "seasonal"]
remainder_component <- decomposed_stl$time.series[, "remainder"]

# Перегляд перших кількох значень кожної компоненти
head(trend_component)
head(seasonal_component)
head(remainder_component)

# Побудова графіків для тренду, сезонності та залишків
library(ggplot2)
library(gridExtra)

# Тренд
plot_trend <- ggplot(data = data.frame(Date = time(nwl_ts), Trend = trend_component), 
                     aes(x = Date, y = Trend)) +
  geom_line(color = "blue") + 
  labs(title = "Тренд", x = "Дата", y = "Значення")

# Сезонність
plot_seasonal <- ggplot(data = data.frame(Date = time(nwl_ts), Seasonal = seasonal_component), 
                        aes(x = Date, y = Seasonal)) +
  geom_line(color = "green") + 
  labs(title = "Сезонність", x = "Дата", y = "Значення")

# Залишки (шум)
plot_remainder <- ggplot(data = data.frame(Date = time(nwl_ts), Remainder = remainder_component), 
                         aes(x = Date, y = Remainder)) +
  geom_line(color = "red") + 
  labs(title = "Залишки (Шум)", x = "Дата", y = "Значення")

# Комбінування графіків у одному вікні
grid.arrange(plot_trend, plot_seasonal, plot_remainder, ncol = 1)

# Перетворення очищених даних у формат ts
nwl_ts <- ts(nwl_close_clean, frequency = 252)  # 252 - кількість торгових днів на рік

# Виконання простого експоненційного згладжування
ses_model <- ses(nwl_ts, h = 30)  # h = горизонт прогнозування на 30 днів

# Перегляд результатів моделі
summary(ses_model)

# Візуалізація прогнозу
autoplot(ses_model) +
  labs(title = "Просте експоненційне згладжування", 
       x = "Дата", y = "Ціна закриття (USD)")

# Подвійне експоненційне згладжування (модель Холта)
holt_model <- holt(nwl_ts, h = 30)
summary(holt_model)

# Візуалізація моделі Холта
autoplot(holt_model) +
  labs(title = "Модель Холта", x = "Дата", y = "Ціна закриття (USD)") +
  theme_minimal()

# ETS-модель без сезонності (адитивна)
ets_no_season_model <- ets(nwl_ts, model = "ANN")  # "A" - add, "N" - no trend/season

# Перегляд підсумкової інформації
summary(ets_no_season_model)

# Візуалізація прогнозу на 30 днів
autoplot(forecast(ets_no_season_model, h = 30)) +
  labs(title = "ETS-модель без сезонності", 
       x = "Дата", y = "Ціна закриття (USD)") +
  theme_minimal()

# In-sample прогнози
ses_fitted <- fitted(ses_model)
holt_fitted <- fitted(holt_model)
ets_fitted <- fitted(ets_model)

# Порівняння метрик точності для моделей
accuracy(ses_fitted, nwl_ts)
accuracy(holt_fitted, nwl_ts)
accuracy(ets_fitted, nwl_ts)

# Виконання автоматичного підбору моделі експоненційного згладжування
ets_model <- ets(nwl_ts)  # автоматичний підбір

# Виведення звіту по моделі ETS
summary(ets_model)

# Порівняння з моделлю Холта (вручну)
holt_model <- holt(nwl_ts, h = 30)  # модель Холта

# Виведення звіту по моделі Холта
summary(holt_model)

# Порівняння параметрів моделей
cat("Модель ETS:\n")
print(ets_model)

cat("\nМодель Холта:\n")
print(holt_model)

# Прогноз за моделлю ETS
ets_forecast <- forecast(ets_model, h = 30)

# Прогноз за моделлю Холта
holt_forecast <- forecast(holt_model, h = 30)

# Візуалізація прогнозів обох моделей
autoplot(ets_forecast) +
    labs(title = "Прогноз за моделлю ETS", x = "Дата", y = "Ціна закриття (USD)") +
    theme_minimal() +
    geom_line(aes(x = time(holt_forecast$mean), y = holt_forecast$mean), color = "red", linetype = "dashed") +  # додавання прогнозу моделі Холта
    geom_line(aes(x = time(nwl_ts), y = nwl_ts), color = "blue")  # справжні дані

# Перетворення очищених даних у формат ts
nwl_ts <- ts(nwl_close_clean, frequency = 252)  # 252 - кількість торгових днів на рік

# Виконання простого експоненційного згладжування
ses_model <- ses(nwl_ts, h = 30)  # h = горизонт прогнозування на 30 днів

# Подвійне експоненційне згладжування (модель Холта)
holt_model <- holt(nwl_ts, h = 30)

# ETS-модель без сезонності (адитивна)
ets_model <- ets(nwl_ts)  # автоматичний підбір

# Візуалізація прогнозів обох моделей разом з первинним часовим рядом
autoplot(nwl_ts, series = "Ціна закриття") +
  autolayer(ses_model, series = "Просте експоненційне згладжування", PI = FALSE, color = "blue") +
  autolayer(holt_model, series = "Модель Холта", PI = FALSE, color = "red") +
  autolayer(forecast(ets_model, h = 30), series = "ETS-модель", PI = FALSE, color = "green") +
  labs(title = "Візуалізація моделей згладжування та первинного часового ряду",
       x = "Дата", y = "Ціна закриття (USD)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Основний графік
plot <- autoplot(nwl_ts, series = "Ціна закриття") +
  autolayer(ses_model$fitted, series = "Просте експоненційне згладжування", color = "blue") +
  autolayer(holt_model$fitted, series = "Модель Холта", color = "red") +
  autolayer(forecast(ets_model, h = 30), series = "ETS-модель", color = "green") +
  labs(title = "Візуалізація моделей згладжування та первинного часового ряду",
       x = "Дата", y = "Ціна закриття (USD)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Відображення графіка
print(plot)

# Функція для формування висновків на основі моделей
generate_summary <- function(ses_model, holt_model, ets_model) {
  # Визначення основних характеристик моделей
  ses_accuracy <- accuracy(ses_model)
  holt_accuracy <- accuracy(holt_model)
  ets_accuracy <- accuracy(ets_model)

  # Формування висновків
  summary <- list()

  summary$SES <- sprintf("Просте експоненційне згладжування (SES): RMSE = %.2f", ses_accuracy[2])  # Використання правильного індексу
  summary$Holt <- sprintf("Модель Холта: RMSE = %.2f", holt_accuracy[2])
  summary$ETS <- sprintf("ETS-модель: RMSE = %.2f", ets_accuracy[2])

  # Порівняння моделей
  summary$Comparison <- "Порівняння моделей:\n"
  summary$Comparison <- paste(summary$Comparison, sprintf("- SES: RMSE %.2f", ses_accuracy[2]), "\n")
  summary$Comparison <- paste(summary$Comparison, sprintf("- Модель Холта: RMSE %.2f", holt_accuracy[2]), "\n")
  summary$Comparison <- paste(summary$Comparison, sprintf("- ETS: RMSE %.2f", ets_accuracy[2]), "\n")

  # Рекомендації
  summary$Recommendations <- "Рекомендації:\n"
  rmse_values <- c(ses_accuracy[2], holt_accuracy[2], ets_accuracy[2])
  
  # Знаходимо модель з найменшим RMSE
  min_rmse_index <- which.min(rmse_values)
  models <- c("SES", "Holt", "ETS")
  recommended_model <- models[min_rmse_index]
  
  summary$Recommendations <- paste(summary$Recommendations, sprintf("Рекомендується використовувати модель %s для прогнозування.", recommended_model))

  # Повертаємо висновки
  return(summary)
}

# Використання функції
summary_results <- generate_summary(ses_model, holt_model, ets_model)

# Виведення результатів
cat(paste(summary_results, collapse = "\n"))

# Встановлення параметрів
window_size <- 10  # Для ковзного середнього
loess_span <- 0.2  # Параметр згладжування для LOESS

# Створення ковзного середнього
nwl_ts_ma <- zoo::rollmean(nwl_ts, k = window_size, fill = NA)

# Локальне згладжування (LOESS)
nwl_ts_loess <- loess(nwl_ts ~ time(nwl_ts), span = loess_span)

# Прогнозування значень LOESS
nwl_ts_loess_pred <- predict(nwl_ts_loess)

# Перетворення прогнозів LOESS у формат ts
nwl_ts_loess_pred_ts <- ts(nwl_ts_loess_pred, start = start(nwl_ts), frequency = frequency(nwl_ts))

# Візуалізація результатів
plot <- autoplot(nwl_ts, series = "Ціна закриття") +
  autolayer(nwl_ts_ma, series = "Ковзне середнє", PI = FALSE, color = "blue") +
  autolayer(nwl_ts_loess_pred_ts, series = "LOESS", PI = FALSE, color = "red") +
  labs(title = "Візуалізація ковзного середнього та LOESS",
       x = "Дата", y = "Ціна закриття (USD)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot)

# Прогнози для ковзного середнього (з вікном 5)
moving_average_fitted <- rollmean(nwl_ts, k = 5, fill = NA, align = "right")

# Модель LOESS та її прогнози
loess_model <- loess(nwl_ts ~ time(nwl_ts))
loess_fitted <- predict(loess_model)

# Обчислення метрик для ковзного середнього
ma_residuals <- na.omit(nwl_ts - moving_average_fitted)
cat("Метрики для Ковзного середнього:\n")
cat("ME:", mean(ma_residuals), "\n")
cat("RMSE:", sqrt(mean(ma_residuals^2)), "\n")
cat("MAE:", mean(abs(ma_residuals)), "\n")
cat("MAPE (%):", mean(abs(ma_residuals / nwl_ts) * 100), "\n")
cat("ACF1:", acf(ma_residuals, plot = FALSE)$acf[2], "\n\n")

# Обчислення метрик для LOESS
loess_residuals <- nwl_ts - loess_fitted
cat("Метрики для Локальної регресії:\n")
cat("ME:", mean(loess_residuals), "\n")
cat("RMSE:", sqrt(mean(loess_residuals^2)), "\n")
cat("MAE:", mean(abs(loess_residuals)), "\n")
cat("MAPE (%):", mean(abs(loess_residuals / nwl_ts) * 100), "\n")
cat("ACF1:", acf(loess_residuals, plot = FALSE)$acf[2], "\n")

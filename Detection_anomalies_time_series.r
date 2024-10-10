# Встановлення необхідних пакетів для роботи з даними, аномалізацією та візуалізацією
install.packages("readxl")
install.packages("anomalize")
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("timetk")
install.packages("tibbletime")
install.packages("tidyverse")
install.packages("lubridate")

# Завантаження бібліотек
library(readxl)
library(anomalize)
library(readr)
library(dplyr)
library(ggplot2)
library(timetk)
library(tibbletime)
library(tidyverse)
library(lubridate)

# Встановлення робочої директорії та завантаження даних з Excel-файлу
setwd("E:/1) STUDY/ІТА/Практичні/Дані.xlsx/Дані.xlsx")
cryptos <- read_xlsx("cryptos.xlsx")

# Фільтрація даних для криптовалюти Tron та сортування за датою
tron_price <- cryptos %>%
  filter(coin == "tron") %>%
  arrange(ds)

# Декомпозиція часового ряду на сезонну, трендову та залишкову компоненти
decomp_result <- tron_price %>%
  time_decompose(y, merge = TRUE)

# Виявлення аномалій у залишках
anomaly_result <- decomp_result %>%
  anomalize(remainder, .date_var = ds)

# Виведення назв колонок результату аномалізації
print(colnames(anomaly_result))

# Перейменування колонок для зручності
anomaly_result <- anomaly_result %>%
  rename(
    remainder_l1 = recomposed_l1,
    remainder_l2 = recomposed_l2
  )

# Відновлення часового ряду на основі аномалізованих даних
recomposed_result <- anomaly_result %>%
  time_recompose()

# Візуалізація декомпозиції аномалій
recomposed_result %>%
  plot_anomaly_decomposition() +
  ggtitle("Аномалії у часовому ряді TRON")

# Візуалізація виявлених аномалій
recomposed_result %>% plot_anomalies()

# Точкове представлення аномалій у графіку
ggplot(recomposed_result, aes(x = ds, y = observed)) +
  geom_point(aes(color = anomaly), size = 2) + 
  scale_color_manual(values = c("No" = "black", "Yes" = "red")) + 
  ggtitle("Точкове уявлення аномалій у часовому ряді TRON") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Дата", y = "Спостереження", color = "Аномалія")

# Лінійне представлення аномалій у графіку
recomposed_result %>%
  plot_anomalies() +
  geom_line()

# Лінійне представлення аномалій з використанням ggplot2
ggplot(recomposed_result, aes(x = ds, y = observed)) +
  geom_line(color = "grey", size = 1) + 
  geom_point(aes(color = anomaly), size = 2) + 
  scale_color_manual(values = c("No" = "black", "Yes" = "red")) + 
  ggtitle("Лінійне уявлення аномалій у часовому ряді TRON") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Дата", y = "Вартість", color = "Аномалії")

# Визначення різних частот для тестування декомпозиції
frequencies <- c(7, 14, 30)
results_frequency <- list()

# Декомпозиція та аномалізація для кожної частоти
for (freq in frequencies) {
  decomp_result <- tron_price %>%
    time_decompose(y, frequency = freq, trend = 91)
  
  anomaly_result <- decomp_result %>%
    anomalize(remainder, .date_var = ds)
  
  results_frequency[[as.character(freq)]] <- anomaly_result
}

# Візуалізація аномалій для кожної частоти
for (name in names(results_frequency)) {
  p <- ggplot(results_frequency[[name]], aes(x = ds, y = observed)) +
    geom_line(color = "grey") +
    geom_point(aes(color = anomaly), size = 2) +
    scale_color_manual(values = c("No" = "black", "Yes" = "red")) +
    ggtitle(paste("Аномалії за частотою", name)) +
    theme_minimal()
  
  print(p)
}

# Визначення методів для тестування аномалізації
methods <- c("iqr", "sd")

# Список для зберігання результатів аномалізації
results_anomalize <- list()

# Виявлення аномалій для кожного методу
for (method in methods) {
    anomaly_result <- decomp_result %>%
        anomalize(remainder, .date_var = ds)
    
    results_anomalize[[paste("Метод:", method)]] <- anomaly_result
}

# Отримання шаблону часового масштабу
get_time_scale_template()

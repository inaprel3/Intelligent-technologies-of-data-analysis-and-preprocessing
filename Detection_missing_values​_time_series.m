# Встановлення необхідних пакетів для роботи
install.packages(c("readxl", "dplyr", "tsibble", "ggplot2", "fpp3", "lubridate", "imputeTS", "pracma", "forecast"))

# Підключення бібліотек
library(readxl)     # Для читання файлів Excel
library(ggplot2)    # Для візуалізації
library(dplyr)      # Для маніпуляцій з даними
library(tsibble)    # Для роботи з часовими рядами (tsibble-об'єкти)
library(fpp3)       # Для роботи з часовими рядами та прогнозування
library(lubridate)  # Для роботи з датами
library(imputeTS)   # Для заповнення пропущених значень
library(pracma)     # Для пошуку пікових значень
library(forecast)   # Для моделювання та роботи з ARIMA

# Встановлення робочої директорії та зчитування даних
setwd("E:/1) STUDY/ІТА/Практичні/1/Дані.xlsx/Дані.xlsx")
cryptos <- read_xlsx("cryptos.xlsx")  # Читання файлу Excel з даними про криптовалюти

# Перетворення стовпця з датами на формат Date
cryptos$ds <- as.Date(cryptos$ds)

# Фільтрація даних для криптовалюти Tron за період з 01.01.2018 до 30.06.2018
tron_data <- cryptos %>%
  filter(coin == "tron") %>%
  filter(ds >= as.Date("2018-01-01") & ds <= as.Date("2018-06-30"))

# Перевірка перших рядків даних
head(tron_data)

# Побудова графіку зміни вартості TRON за вибраний період
ggplot(tron_data, aes(x = ds, y = y)) +
  geom_line(color = "blue") +
  labs(title = "Зміна вартості TRON (01.01.2018 - 30.06.2018)",
       x = "Дата",
       y = "Вартість (USD)") +
  theme_minimal()

# Конвертуємо наші дані в об'єкт tsibble (часовий ряд)
tron_tsibble <- tron_data %>%
  as_tsibble(index = ds)

# Візуалізація часових рядів та їхніх автокореляцій
gg_tsdisplay(tron_tsibble)

# Логарифмування даних для більш рівномірного розподілу
l_tron_data <- log(tron_tsibble$y)

# Пошук пікових значень у логарифмованому ряді
peaks <- findpeaks(l_tron_data, nups = 2, ndowns = 2, threshold = 0, sortstr = TRUE)

# Створюємо копію даних для модифікації
tron_data_na <- tron_tsibble

# Задаємо індекси пікових значень як пропущені
peak_indices <- peaks[,2]  # Другий стовпець містить індекси пікових значень
tron_data_na$y[peak_indices] <- NA

# Візуалізація розподілу пропущених значень у часовому ряді
ggplot_na_distribution(tron_data_na$y)
ggplot_na_distribution2(tron_data_na$y)

# Заповнення пропущених значень методом лінійної інтерполяції
tron_data_na_int <- na_interpolation(tron_data_na$y)

# Візуалізація результатів: порівняння даних до і після інтерполяції
ggplot_na_imputations(tron_data_na$y, tron_data_na_int, l_tron_data)

# Заповнення пропущених значень ARIMA-моделлю (вручну підібрані параметри)
mod_manual <- arima(tron_data_na$y, order = c(1,1,0), seasonal = list(order=c(0,1,0)))$model
tron_data_ar_manual <- na_kalman(tron_data_na$y, model = mod_manual)

# Візуалізація результатів для ручної ARIMA-моделі
ggplot_na_imputations(tron_data_na$y, tron_data_ar_manual, l_tron_data)

# Заповнення пропущених значень ARIMA-моделлю (автоматичний підбір параметрів)
mod_auto <- auto.arima(tron_data_na$y)
tron_data_ar_auto <- na_kalman(tron_data_na$y, model = 'auto.arima')

# Візуалізація результатів для автоматичної ARIMA-моделі
ggplot_na_imputations(tron_data_na$y, tron_data_ar_auto, l_tron_data)

# Заповнення пропущених значень методом STL розкладання (з частотою 7 - тижнева сезонність)
tron_data_ts <- ts(tron_data_na$y, frequency = 7)
tron_data_seas <- na_seadec(tron_data_ts)

# Візуалізація результатів для STL розкладання (тижнева частота)
ggplot_na_imputations(tron_data_na$y, tron_data_seas, l_tron_data)

# Заповнення пропущених значень методом STL розкладання (з частотою 30 - місячна сезонність)
tron_data_ts <- ts(tron_data_na$y, frequency = 30)
tron_data_seas <- na_seadec(tron_data_ts)

# Візуалізація результатів для STL розкладання (місячна частота)
ggplot_na_imputations(tron_data_na$y, tron_data_seas, l_tron_data)

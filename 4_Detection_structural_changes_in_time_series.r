# Встановлюємо необхідні пакети для роботи
install.packages("readxl")
install.packages("devtools")
install.packages("BreakoutDetection")
install.packages("makeit")
install.packages("dplyr")

# Встановлюємо пакет BreakoutDetection з GitHub
devtools::install_github("twitter/BreakoutDetection")

# Підключаємо необхідні бібліотеки для подальшої роботи
library(readxl)
library(devtools)
require(BreakoutDetection)
library(dplyr)

# Встановлюємо робочий каталог, де зберігається файл даних
setwd("E:/1) STUDY/ІТА/Практичні/Дані.xlsx/Дані.xlsx")

# Завантажуємо дані з Excel-файлу у змінну cryptos
cryptos <- read_xlsx("cryptos.xlsx")

# Фільтруємо дані для криптовалюти Tron
tron_data <- subset(cryptos, coin == "Tron")

# Перетворюємо колонку з датами на формат POSIXct для коректної обробки
tron_data$ds <- as.POSIXct(tron_data$ds)

# Застосовуємо метод EDM для виявлення точки зламу (модель BO0)
BO0 <- breakout(log(tron_data$y), plot = TRUE, ylab = "Ціна Tron")

# Виконуємо перестановочний тест з 1000 ітерацій для оцінки статистичної значущості зсуву
BO0_perm <- breakout(log(tron_data$y), nperm = 1000)

# Виводимо результати перестановочного тесту
print(BO0_perm)

# Виводимо графік з виявленою точкою зламу
BO0$plot

# Застосовуємо модель BO1 для отримання кількох точок зламу з параметрами percent і beta
BO1 <- breakout(log(tron_data$y), method = "multi", percent = 0.08, beta = 0.001, plot = TRUE)

# Виводимо порядкові номери спостережень, що відповідають виявленим точкам зламу
print(BO1$loc)

# Будуємо графік з логарифмічною ціною Tron з виявленими точками зламу
plot(
  log(tron_data$y), type = "l", main = "Кілька точок зламу в ціні Tron", 
  ylab = "Логарифмічна ціна", xlab = "Індекс"
)

# Додаємо вертикальні червоні пунктирні лінії для позначення точок зламу
abline(v = BO1$loc, col = "red", lty = 2, lwd = 2)

# Відключаємо регуляризацію у моделі BO2 для виявлення точок зламу
BO2 <- breakout(log(tron_data$y), method = "multi", degree = 0, plot = TRUE)

# Виводимо порядкові номери спостережень для точок зламу в моделі BO2
print(BO2$loc)

# Будуємо графік без регуляризації
plot(
  log(tron_data$y), type = "l", main = "Ціна Tron без регуляризації (BO2)",
  ylab = "Логарифмічна ціна", xlab = "Індекс"
)

# Додаємо вертикальні червоні пунктирні лінії для позначення точок зламу
abline(v = BO2$loc, col = "red", lty = 2, lwd = 2)

# Застосовуємо модель BO3 з низьким значенням параметра percent для виявлення точок зламу
BO3 <- breakout(log(tron_data$y), method = "multi", percent = 0.05, plot = TRUE)

# Виводимо порядкові номери спостережень для точок зламу в моделі BO3
print(BO3$loc)

# Будуємо графік для BO3
plot(
  log(tron_data$y), type = "l", main = "Ціна Tron (BO3: percent = 0.05)",
  ylab = "Логарифмічна ціна", xlab = "Індекс"
)

# Додаємо вертикальні червоні пунктирні лінії для позначення точок зламу
abline(v = BO3$loc, col = "red", lty = 2, lwd = 2)

# Застосовуємо модель BO4 з зниженим параметром beta для виявлення точок зламу
BO4 <- breakout(log(tron_data$y), method = "multi", beta = 0.0001, plot = TRUE)

# Виводимо порядкові номери спостережень для точок зламу в моделі BO4
print(BO4$loc)

# Будуємо графік для BO4
plot(
  log(tron_data$y), type = "l", main = "Ціна Tron (BO4: beta = 0.0001)",
  ylab = "Логарифмічна ціна", xlab = "Індекс"
)

# Додаємо вертикальні червоні пунктирні лінії для позначення точок зламу
abline(v = BO4$loc, col = "red", lty = 2, lwd = 2)

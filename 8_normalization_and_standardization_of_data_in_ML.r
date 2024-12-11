# Встановимо робочу директорію
setwd("E:/1) STUDY/7. ІТА/Практичні/Дані.xlsx/yeast")

# Завантажимо файл як текстовий файл із роздільником пробілом
data <- read.table("yeast.data", header = FALSE, sep = "", stringsAsFactors = FALSE)

# Переглянемо перші кілька рядків даних
head(data)

# Визначимо назви стовпців на основі структури даних
colnames(data) <- c("Gene", "Attribute1", "Attribute2", "Attribute3", "Attribute4", "Attribute5", "Attribute6", "Attribute7", "Attribute8", "Class")

# Переглянемо оновлену структуру даних після зміни назв стовпців
head(data)

# Видалимо стовпець з ідентифікаторами генів (Gene) та стовпець класу (Class), оскільки це не числові дані
numeric_data <- data[, -c(1, 10)]

# Обчислимо описову статистику для числових змінних (min, max, range)
summary(numeric_data)

# Обчислимо min, max та range для кожної змінної
min_values <- apply(numeric_data, 2, min)
max_values <- apply(numeric_data, 2, max)
range_values <- apply(numeric_data, 2, function(x) diff(range(x)))

# Виведемо результати
min_values
max_values
range_values

# Завантажимо необхідні бібліотеки
install.packages("ggplot2")
install.packages("nortest")
library(ggplot2)
library(nortest)

# Для кожної числової змінної
for (col in colnames(numeric_data)) {
  # Гістограма
  ggplot(numeric_data, aes_string(x = col)) +
    geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
    ggtitle(paste("Histogram of", col)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Q-Q графік
  qqnorm(numeric_data[[col]], main = paste("Q-Q plot of", col))
  qqline(numeric_data[[col]], col = "red")
  
  # Тест Шапіро-Уілка
  shapiro_test <- shapiro.test(numeric_data[[col]])
  print(paste("Shapiro-Wilk test for", col, "p-value:", shapiro_test$p.value))
  
  # Тест Андерсона-Дарлінга
  ad_test <- ad.test(numeric_data[[col]])
  print(paste("Anderson-Darling test for", col, "p-value:", ad_test$p.value))
  
  # Тест Крамера фон Мізеса
  cvm_test <- cvm.test(numeric_data[[col]])
  print(paste("Cramer-von Mises test for", col, "p-value:", cvm_test$p.value))
  
  # Тест Лілієфорса
  lillie_test <- lillie.test(numeric_data[[col]])
  print(paste("Lilliefors test for", col, "p-value:", lillie_test$p.value))
}

# Встановимо та завантажимо необхідний пакет
install.packages("car")
install.packages("bestNormalize")
library(car)
library(bestNormalize)

# Перевіримо, чи всі дані позитивні для використання Бокса-Кокса
min(numeric_data)  # перевіримо мінімальне значення
[1] 0

# Застосуємо перетворення Йео-Джонсона до кожної числової змінної
transformed_data <- as.data.frame(lapply(numeric_data, function(x) bestNormalize(x, method = "yeo.johnson")$x))

# Переглянемо результат перетворення
head(transformed_data)

# Перевіримо описову статистику для перетворених даних
summary(transformed_data)

# Тепер можна знову зробити графіки та тести на нормальність
for (col in colnames(transformed_data)) {
    # Гістограма
    ggplot(transformed_data, aes_string(x = col)) +
        geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
        ggtitle(paste("Histogram of", col)) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    
    # Q-Q графік
    qqnorm(transformed_data[[col]], main = paste("Q-Q plot of", col))
    qqline(transformed_data[[col]], col = "red")
    
    # Повторно проводимо тести на нормальність
    shapiro_test <- shapiro.test(transformed_data[[col]])
    print(paste("Shapiro-Wilk test for", col, "p-value:", shapiro_test$p.value))
    
    ad_test <- ad.test(transformed_data[[col]])
    print(paste("Anderson-Darling test for", col, "p-value:", ad_test$p.value))
    
    cvm_test <- cvm.test(transformed_data[[col]])
    print(paste("Cramer-von Mises test for", col, "p-value:", cvm_test$p.value))
    
    lillie_test <- lillie.test(transformed_data[[col]])
    print(paste("Lilliefors test for", col, "p-value:", lillie_test$p.value))
}

# Завантажимо необхідні пакети для роботи з Махаланобісом
install.packages("stats")
library(stats)

# Обчислимо Махаланобісову відстань для кожного рядка даних
mahalanobis_data <- mahalanobis(numeric_data, colMeans(numeric_data), cov(numeric_data))

# Переглянемо статистику Махаланобісової відстані
summary(mahalanobis_data)

# Логарифмічне перетворення
log_transformed_data <- log1p(numeric_data)  # log1p застосовує log(x+1) для уникнення log(0)

# Переглянемо перші кілька рядків після перетворення
head(log_transformed_data)

# Перевіримо описову статистику для логарифмічно перетворених даних
summary(log_transformed_data)

# Застосуємо перетворення Бокса-Кокса
boxcox_transformed_data <- as.data.frame(lapply(numeric_data, function(x) bestNormalize(x, method = "boxcox")$x))

# Переглянемо результат перетворення
head(boxcox_transformed_data)

# Перевіримо описову статистику для перетворених даних
summary(boxcox_transformed_data)

# Візуалізація для перетворених даних
for (col in colnames(log_transformed_data)) {
  # Гістограма
  ggplot(log_transformed_data, aes_string(x = col)) +
    geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
    ggtitle(paste("Log-transformed Histogram of", col)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Q-Q графік
  qqnorm(log_transformed_data[[col]], main = paste("Q-Q plot of", col))
  qqline(log_transformed_data[[col]], col = "red")
  
  # Тести на нормальність
  shapiro_test <- shapiro.test(log_transformed_data[[col]])
  print(paste("Shapiro-Wilk test for", col, "p-value:", shapiro_test$p.value))
}

# Встановимо необхідні пакети
install.packages("dplyr")
install.packages("scales")
install.packages("outliers")
library(dplyr)
library(scales)
library(outliers)

# Функція для перевірки на наявність викидів
check_outliers <- function(data) {
  # Виявлення викидів за правилом 1.5*IQR
  iqr_values <- apply(data, 2, IQR)  # міжквартильний розмах
  q1_values <- apply(data, 2, quantile, 0.25)  # 1-й квартиль
  q3_values <- apply(data, 2, quantile, 0.75)  # 3-й квартиль
  lower_bound <- q1_values - 1.5 * iqr_values
  upper_bound <- q3_values + 1.5 * iqr_values
  
  # Перевірка, чи є значення за межами [lower_bound, upper_bound]
  outliers <- apply(data, 2, function(x) sum(x < lower_bound | x > upper_bound))
  return(outliers)
}

# Перевіряємо на наявність викидів у трансформованих даних
outliers <- check_outliers(transformed_data)
print("Кількість викидів у кожній змінній:")
print(outliers)

# Умовна нормалізація залежно від наявності викидів
if (all(outliers == 0)) {
  print("Викидів немає. Використовуємо лінійну нормалізацію.")
  
  # Лінійна нормалізація (мін-макс нормалізація)
  linear_normalized_data <- as.data.frame(lapply(transformed_data, function(x) rescale(x, to = c(0, 1))))
  
  # Переглянемо результат лінійної нормалізації
  summary(linear_normalized_data)
  
  # Зберігаємо дані для подальшого використання
  final_normalized_data <- linear_normalized_data
} else {
  print("Виявлено викиди. Використовуємо методи нелінійної нормалізації.")
  
  # Нелінійна нормалізація (наприклад, логарифмічна)
  nonlinear_normalized_data <- as.data.frame(lapply(transformed_data, log1p))  # log(x+1)
  
  # Переглянемо результат нелінійної нормалізації
  summary(nonlinear_normalized_data)
  
  # Зберігаємо дані для подальшого використання
  final_normalized_data <- nonlinear_normalized_data
}

# Переглядаємо фінально нормалізовані дані
head(final_normalized_data)

# Можна додатково перевірити розподіл після нормалізації
for (col in colnames(final_normalized_data)) {
  # Гістограма
  ggplot(final_normalized_data, aes_string(x = col)) +
    geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
    ggtitle(paste("Histogram of", col)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Q-Q графік
  qqnorm(final_normalized_data[[col]], main = paste("Q-Q plot of", col))
  qqline(final_normalized_data[[col]], col = "red")
}

# Описова статистика для початкових (трансформованих) даних
original_summary <- summary(transformed_data)

# Описова статистика для нормалізованих даних
normalized_summary <- summary(final_normalized_data)

# Виведемо звіти
print("Описова статистика для початкових (трансформованих) даних:")
print(original_summary)

print("Описова статистика для нормалізованих даних:")
print(normalized_summary)

# Додатково можна згенерувати таблицю для порівняння середніх значень і стандартних відхилень
original_means <- apply(transformed_data, 2, mean)
original_sds <- apply(transformed_data, 2, sd)
normalized_means <- apply(final_normalized_data, 2, mean)
normalized_sds <- apply(final_normalized_data, 2, sd)

comparison <- data.frame(
  Variable = colnames(transformed_data),
  Original_Mean = original_means,
  Original_SD = original_sds,
  Normalized_Mean = normalized_means,
  Normalized_SD = normalized_sds
)

print("Порівняльна таблиця середніх значень і стандартних відхилень:")
print(comparison)

setwd("E:/1) STUDY/ІТА/Практичні/Дані.xlsx/yeast")

# Завантажимо файл як текстовий файл із роздільником пробілом
data <- read.table("yeast.data", header = FALSE, sep = "", stringsAsFactors = FALSE)

# Переглянемо перші кілька рядків набору даних
head(data)

# Переглянемо останні кілька рядків
tail(data)

# Описова статистика для кожної змінної
summary(data)

# Перевірка структури даних
str(data)

# Встановимо пакет psych
install.packages("psych")

# Підключимо пакет після встановлення
library(psych)

# Виконання детальної описової статистики
describe(data)

# Перетворення текстових змінних на фактори
data$V1 <- as.factor(data$V1)  # Перетворення назв білків на фактор
data$V10 <- as.factor(data$V10)  # Перетворення класів на фактор

# Перевіримо структуру даних після змін
str(data)

# Перевіримо наявність пропущених значень
missing_data <- colSums(is.na(data))  # Підраховуємо пропущені значення по кожному стовпцю

# Частота пропущених значень
missing_percentage <- missing_data / nrow(data) * 100
print(missing_percentage)

# Якщо пропусків менше 50%, заповнюємо (наприклад, середнім значенням для числових змінних або модою для факторів)
for (i in 1:ncol(data)) {
  if (missing_percentage[i] < 50) {
    if (is.numeric(data[[i]])) {
      data[[i]][is.na(data[[i]])] <- mean(data[[i]], na.rm = TRUE)  # Заповнюємо середнім значенням
    } else if (is.factor(data[[i]])) {
      data[[i]][is.na(data[[i]])] <- as.factor(names(sort(table(data[[i]]), decreasing = TRUE)[1]))  # Заповнюємо модою
    }
  } else {
    data[[i]] <- NULL  # Видаляємо змінну, якщо пропусків більше 50%
  }
}

# Перевіримо структуру даних після обробки пропусків
str(data)

# Побудова boxplot для числових змінних
numeric_vars <- data[, sapply(data, is.numeric)]  # Вибираємо лише числові змінні

# Візуалізація boxplot для кожної числової змінної
par(mfrow = c(2, 4))  # Розміщуємо графіки в 2 рядки по 4 графіка
for (col in colnames(numeric_vars)) {
    boxplot(numeric_vars[[col]], main = col, col = "lightblue")
}

# Функція для обробки викидів
remove_outliers <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Замінюємо викиди на NA
    x[x < lower_bound | x > upper_bound] <- NA
    return(x)
}

# Обробка викидів для всіх числових змінних
data_clean <- data
for (col in colnames(numeric_vars)) {
    data_clean[[col]] <- remove_outliers(data_clean[[col]])
}

# Перевіримо описову статистику до та після обробки
summary(data)  # Описова статистика до обробки
summary(data_clean)  # Описова статистика після обробки

# Візуалізація boxplot після обробки викидів
par(mfrow = c(2, 4))
for (col in colnames(numeric_vars)) {
    boxplot(data_clean[[col]], main = col, col = "lightgreen")
}

# Перевірка на унікальність кожного спостереження
duplicates <- duplicated(data)  # Перевірка на дублікати
sum(duplicates)  # Підраховуємо кількість дублікатів

# Якщо є дублікати, можемо їх видалити
data_unique <- data[!duplicates, ]  # Видалення дублікатів

# Вибір числових змінних
numeric_vars <- data[, sapply(data, is.numeric)]

# Побудова гістограм для кожної числової змінної
par(mfrow = c(2, 4))  # Розміщуємо 8 графіків на одному екрані
for (col in colnames(numeric_vars)) {
    hist(numeric_vars[[col]], main = paste("Histogram of", col), 
         xlab = col, col = "lightblue", border = "black")
}

# Логарифмування числових змінних
numeric_vars_log <- log(numeric_vars + 1)  # Додаємо 1, щоб уникнути логарифмування нуля

# Побудова гістограм для логарифмованих значень
par(mfrow = c(2, 4))  # Розміщуємо 8 графіків на одному екрані
for (col in colnames(numeric_vars_log)) {
    hist(numeric_vars_log[[col]], main = paste("Log-transformed Histogram of", col), 
         xlab = paste("log", col), col = "lightgreen", border = "black")
}

# Оновлюємо дані з логарифмованими значеннями
data_log <- data
data_log[, sapply(data_log, is.numeric)] <- numeric_vars_log

install.packages("corrplot")

# Обчислення матриці кореляції для числових змінних
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Виведення матриці кореляції
print(cor_matrix)

# Візуалізація матриці кореляції
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Візуалізація матриці розсіювання
pairs(numeric_vars, 
      panel = panel.smooth,      # Додає регресійну лінію до кожного графіка
      main = "Матриця розсіювання змінних")

# Виведення матриці розсіювання з кореляціями між змінними
cat("Матриця розсіювання для числових змінних:\n")

# Функція для обчислення та виведення кореляції в кожній парі змінних
pairs(numeric_vars, 
      panel = function(x, y) {
        points(x, y, pch = 19, col = rgb(0.5, 0, 0, 0.5))  # Додає точки
        correlation <- cor(x, y, use = "complete.obs")  # Обчислює кореляцію між кожною парою змінних
        mtext(paste("r =", round(correlation, 2)), cex = 0.7)  # Додає значення кореляції на графік
        cat(paste("Кореляція між змінними:", deparse(substitute(x)), "і", deparse(substitute(y)), "=", round(correlation, 2), "\n"))
      },
      main = "Матриця розсіювання змінних")

# Виведення кореляцій в консоль для кожної пари змінних
for (i in 1:(ncol(numeric_vars)-1)) {
  for (j in (i+1):ncol(numeric_vars)) {
    correlation <- cor(numeric_vars[[i]], numeric_vars[[j]], use = "complete.obs")
    cat("Кореляція між", colnames(numeric_vars)[i], "та", colnames(numeric_vars)[j], "=", round(correlation, 2), "\n")
  }
}

# Виведення результатів кореляцій у таблицю
correlation_results <- data.frame(Variable1 = character(), Variable2 = character(), Correlation = numeric())

for (i in 1:(ncol(numeric_vars)-1)) {
  for (j in (i+1):ncol(numeric_vars)) {
    correlation <- cor(numeric_vars[[i]], numeric_vars[[j]], use = "complete.obs")
    correlation_results <- rbind(correlation_results, 
                                 data.frame(Variable1 = colnames(numeric_vars)[i], 
                                            Variable2 = colnames(numeric_vars)[j], 
                                            Correlation = correlation))
  }
}

# Виведення таблиці кореляцій
print(correlation_results)

# Встановлення set.seed для відтворюваності результатів
set.seed(123)

# Розподіл даних на навчальну та тестову вибірки
train_index <- sample(1:nrow(data_clean), size = 0.8 * nrow(data_clean))  # 80% для навчання
train_data <- data_clean[train_index, ]  # Навчальна вибірка
test_data <- data_clean[-train_index, ]  # Тестова вибірка

# Збереження вибірок у CSV файли
write.csv(train_data, "train_data.csv", row.names = FALSE)
write.csv(test_data, "test_data.csv", row.names = FALSE)

# Перевірка збережених файлів
head(train_data)
head(test_data)

# Встановимо робочу директорію
setwd("E:/1) STUDY/ІТА/Практичні/Дані.xlsx/yeast")

# Завантажимо файл як текстовий файл із роздільником пробілом
data <- read.table("yeast.data", header = FALSE, sep = "", stringsAsFactors = FALSE)

# Додамо назви стовпців (згідно з описом набору даних)
colnames(data) <- c("SequenceName", "mcg", "gvh", "alm", "mit", "erl", "pox", "vac", "nuc")

# Переглянемо перші кілька рядків, щоб переконатися, що дані завантажилися коректно
head(data)

# Перевіримо, чи є пропущені значення
missing_values <- sum(is.na(data))
cat("Кількість пропущених значень у наборі даних:", missing_values, "\n")

# Встановлення та підключення пакета leap
install.packages("leaps")
library(leaps)

# Вилучимо текстову змінну SequenceName, щоб залишилися лише числові змінні
data_numeric <- data[, -1]

# Задамо залежну змінну (gvh) і незалежні змінні
dependent_var <- "gvh"
independent_vars <- setdiff(colnames(data_numeric), dependent_var)

# Перевіримо структуру даних
str(data_numeric)

# Видалимо текстову колонку NA (або інші непридатні змінні)
data_numeric <- data_numeric[, sapply(data_numeric, is.numeric)]

# Задамо залежну змінну (gvh) і незалежні змінні
dependent_var <- "gvh"
independent_vars <- setdiff(colnames(data_numeric), dependent_var)

# Виконуємо відбір підмножин змінних
subset_selection <- regsubsets(
  as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+"))),
  data = data_numeric,
  nvmax = length(independent_vars) # Максимальна кількість змінних у моделі
)

# Отримуємо результати
subset_summary <- summary(subset_selection)

# Переглянемо короткий підсумок результатів
print(subset_summary)

# Візуалізація критеріїв
par(mfrow = c(2, 3)) # Установимо формат графіків (2 ряди, 3 колонки)

# R-squared
plot(subset_summary$rsq, type = "b", xlab = "Кількість змінних", ylab = "R²", main = "R²")
points(which.max(subset_summary$rsq), max(subset_summary$rsq), col = "red", pch = 20) # Максимальне значення

# Adjusted R-squared
plot(subset_summary$adjr2, type = "b", xlab = "Кількість змінних", ylab = "Adjusted R²", main = "Скоригований R²")
points(which.max(subset_summary$adjr2), max(subset_summary$adjr2), col = "red", pch = 20) # Максимальне значення

# RSS
plot(subset_summary$rss, type = "b", xlab = "Кількість змінних", ylab = "RSS", main = "RSS")
points(which.min(subset_summary$rss), min(subset_summary$rss), col = "red", pch = 20) # Мінімальне значення

# Cp
plot(subset_summary$cp, type = "b", xlab = "Кількість змінних", ylab = "Cp", main = "Cp")
points(which.min(subset_summary$cp), min(subset_summary$cp), col = "red", pch = 20) # Мінімальне значення

# BIC
plot(subset_summary$bic, type = "b", xlab = "Кількість змінних", ylab = "BIC", main = "BIC")
points(which.min(subset_summary$bic), min(subset_summary$bic), col = "red", pch = 20) # Мінімальне значення

# Повертаємо початковий режим малювання графіків
par(mfrow = c(1, 1))

# Визначення оптимальних моделей
optimal_bic <- which.min(subset_summary$bic) # Мінімальний BIC
optimal_cp <- which.min(subset_summary$cp)   # Мінімальний Cp

# Отримуємо змінні для цих моделей
cat("Оптимальна модель за BIC включає змінні:", names(coef(subset_selection, optimal_bic))[-1], "\n")
cat("Оптимальна модель за Cp включає змінні:", names(coef(subset_selection, optimal_cp))[-1], "\n")

# Метод покрокового включення змінних (forward selection)
subset_forward <- regsubsets(
  as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+"))),
  data = data_numeric,
  nvmax = length(independent_vars),
  method = "forward"
)

# Метод покрокового виключення змінних (backward elimination)
subset_backward <- regsubsets(
  as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+"))),
  data = data_numeric,
  nvmax = length(independent_vars),
  method = "backward"
)

# Результати forward selection
summary_forward <- summary(subset_forward)

# Результати backward elimination
summary_backward <- summary(subset_backward)

# Виведемо результати для forward selection
cat("Forward Selection:\n")
print(summary_forward)

# Виведемо результати для backward elimination
cat("\nBackward Elimination:\n")
print(summary_backward)

# Forward Selection
par(mfrow = c(2, 3)) # Установимо формат графіків

# R-squared
plot(summary_forward$rsq, type = "b", xlab = "Кількість змінних", ylab = "R²", main = "R² (Forward)")
points(which.max(summary_forward$rsq), max(summary_forward$rsq), col = "red", pch = 20)

# Adjusted R-squared
plot(summary_forward$adjr2, type = "b", xlab = "Кількість змінних", ylab = "Adjusted R²", main = "Скоригований R² (Forward)")
points(which.max(summary_forward$adjr2), max(summary_forward$adjr2), col = "red", pch = 20)

# RSS
plot(summary_forward$rss, type = "b", xlab = "Кількість змінних", ylab = "RSS", main = "RSS (Forward)")
points(which.min(summary_forward$rss), min(summary_forward$rss), col = "red", pch = 20)

# Cp
plot(summary_forward$cp, type = "b", xlab = "Кількість змінних", ylab = "Cp", main = "Cp (Forward)")
points(which.min(summary_forward$cp), min(summary_forward$cp), col = "red", pch = 20)

# BIC
plot(summary_forward$bic, type = "b", xlab = "Кількість змінних", ylab = "BIC", main = "BIC (Forward)")
points(which.min(summary_forward$bic), min(summary_forward$bic), col = "red", pch = 20)

# Повертаємо початковий режим малювання графіків
par(mfrow = c(1, 1))

# Forward Selection
optimal_bic_forward <- which.min(summary_forward$bic)
optimal_cp_forward <- which.min(summary_forward$cp)

cat("Оптимальна модель (Forward) за BIC включає змінні:", names(coef(subset_forward, optimal_bic_forward))[-1], "\n")
cat("Оптимальна модель (Forward) за Cp включає змінні:", names(coef(subset_forward, optimal_cp_forward))[-1], "\n")

# Backward Elimination
optimal_bic_backward <- which.min(summary_backward$bic)
optimal_cp_backward <- which.min(summary_backward$cp)

cat("Оптимальна модель (Backward) за BIC включає змінні:", names(coef(subset_backward, optimal_bic_backward))[-1], "\n")
cat("Оптимальна модель (Backward) за Cp включає змінні:", names(coef(subset_backward, optimal_cp_backward))[-1], "\n")

# Встановимо seed для відтворюваності результатів
set.seed(123)

# Розбиваємо дані на 80% навчальну та 20% перевірочну вибірки
train_indices <- sample(1:nrow(data_numeric), size = 0.8 * nrow(data_numeric))
train_data <- data_numeric[train_indices, ]
test_data <- data_numeric[-train_indices, ]

# Перевіримо розміри вибірок
cat("Розмір навчальної вибірки:", nrow(train_data), "\n")
cat("Розмір перевірочної вибірки:", nrow(test_data), "\n")

# Виконуємо відбір підмножин змінних на навчальній вибірці
subset_selection_train <- regsubsets(
  as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+"))),
  data = train_data,
  nvmax = length(independent_vars) # Максимальна кількість змінних у моделі
)

# Отримуємо результати для навчальної вибірки
subset_summary_train <- summary(subset_selection_train)

# Знаходимо індекс оптимальної моделі
optimal_model_index <- which.min(mse_values)

cat("Оптимальна модель за MSE має кількість змінних:", optimal_model_index, "\n")

# Завантаження необхідних бібліотек
library(leaps)
library(caret)

# Кількість блоців для перехресної перевірки
k <- 10

# Створюємо індекси для перехресної перевірки (k-fold cross-validation)
set.seed(123)  # для відтворюваності результатів
folds <- createFolds(data_numeric$gvh, k = k, list = TRUE, returnTrain = TRUE)

# Матриця для збереження помилок
cv.errors <- matrix(NA, nrow = k, ncol = length(independent_vars))

# Виконуємо перехресну перевірку
for (i in 1:k) {
  # Отримуємо навчальні та тестові дані для поточного блоку
  train_data <- data_numeric[folds[[i]], ]
  test_data <- data_numeric[-folds[[i]], ]
  
  # Вибір підмножини змінних на основі навчальних даних
  subset_selection <- regsubsets(
    as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+"))),
    data = train_data,
    nvmax = length(independent_vars)
  )
  
  # Отримуємо результати
  subset_summary <- summary(subset_selection)
  
  # Обчислюємо помилки (MSE) для кожної моделі
  for (j in 1:length(independent_vars)) {
    # Створення прогнозів для поточної моделі
    selected_vars <- names(coef(subset_selection, j))[-1]
    model <- lm(as.formula(paste(dependent_var, "~", paste(selected_vars, collapse = "+"))), data = train_data)
    predictions <- predict(model, newdata = test_data)
    
    # Обчислення MSE для поточної моделі
    mse <- mean((predictions - test_data[[dependent_var]])^2)
    cv.errors[i, j] <- mse
  }
}

# Обчислення середнього MSE для кожної моделі з j змінними
mean_cv_errors <- colMeans(cv.errors, na.rm = TRUE)

# Вивести помилки для кожної моделі
print(mean_cv_errors)

# Визначення оптимальної моделі на основі мінімальної помилки перехресної перевірки
optimal_num_vars_cv <- which.min(mean_cv_errors)

cat("Оптимальна модель за перехресною перевіркою має кількість змінних:", optimal_num_vars_cv, "\n")

# Створення моделі на повному наборі даних з оптимальним числом змінних
optimal_vars <- names(coef(subset_selection, optimal_num_vars_cv))[-1]
final_model <- lm(as.formula(paste(dependent_var, "~", paste(optimal_vars, collapse = "+"))), data = data_numeric)

cat("Оптимальна модель за перехресною перевіркою включає змінні:", optimal_vars, "\n")

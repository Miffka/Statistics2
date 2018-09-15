setwd(as.character("C:/Users/miffka/Documents/!DataMining/Stats2"))
getwd()

library(dplyr)
library(ggplot2)
library(vcd)

#    Задача 1 - возврат вектора с экспонентой коэффициентов

test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
# переведем переменные в фактор
test_data  <- transform(test_data, x = factor(x), y = factor(y))

get_coefficients <- function(dataset){
  model1 <- glm(y ~ x, dataset, family = "binomial")
  return(exp(coef(model1)))
}

get_coefficients(test_data)


#    Задача 2 - центрировать переменные

test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
var_names <-  c("X4", "X2", "X1")
test_data2[var_names] #обращаемся к нужным данным при помощи простого среза!

centered <- function(df, v) {
  df[v] <- apply(df[v], 2, function(col) col-mean(col))
  return(df)
}

var_names <- c("X4")
centered(test_data2, var_names)

test_data21 <- data.frame(rnorm(5))
centered(test_data21)


#    Задача 3 - логистическая регрессия, возвращение знач.переменных

test_data31 <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
test_data32 <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")
str(test_data31)
task32 <- glm(is_prohibited ~ ., test_data31, family = "binomial")
summary(task32)
task33 <- anova(task32, test = "Chisq")
task33
(task33$`Pr(>Chi)`<0.05) == (task33[,5]<0.05)
?subset
length(task34)

get_features <- function(df) {
  task31 <- glm(is_prohibited ~ ., df, family = "binomial")
  task311 <- anova(task31, test = "Chisq")
  task3111 <- rownames(subset.data.frame(task311, task311$`Pr(>Chi)`<0.05, 5))
  if (length(task3111) == 0) {
    print("Prediction makes no sense")
  }
  else {
    print(task3111)
  }
}

get_features(test_data31)
get_features(test_data32)


#    Задача 4 - предсказание по первому набору для второго набора

test_data41 <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
for_predict41 <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")
str(test_data41)
str(for_predict41)

most_suspicious <- function(test_df, to_predict) {
  model1 <- glm(is_prohibited ~ ., test_df, family = "binomial")
  to_predict$prob <- predict(model1, newdata = to_predict, type = "response")
  output <- to_predict$passangers[to_predict$prob == max(to_predict$prob)]
  return(as.vector(output))
}

most_suspicious(test_data41, for_predict41)


#    Задача 5 - для датасета вернуть р-значения теста Шапиро-Вилка
#              для всех количественных переменных

test5 <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")
ans52 <- sapply(iris[sapply(iris, is.numeric)], shapiro.test)

normality_test <- function(df) {
  shapres <- sapply(df[sapply(df, is.numeric)], shapiro.test)
  return(unlist(shapres[2,]))
}

normality_test(iris)
normality_test(test5)


#    Задача 6 - датасет из x(num) и y(factor 3 уровня). Если распределения
#     у трех уровней фактора значимо не отличаются от нормального, а 
#        дисперсии гомогенны, сравнить дисперсионным анализом
#       вернуть вектор с р-значением и именем ANOVA.
#       Если не нормальное или дисперсии негомогенны, сравнить
#  по критерию Краскела-Уоллиса и вернуть вектор с именем KW.

t6 <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")

smart_anova <- function(df) {
  c61 <- unlist(by(t6[,1], t6[,2], function(x) shapiro.test(x)$p.value))
  c62 <- bartlett.test(df$x, df$y)$p.value

  if (any(c61 <= 0.05) | (c62 <= 0.05)) {
    result <- c("KW" = kruskal.test(x ~ y, df)$p.value)
  } else {
    fit <- aov(x ~ y, df)
    result <- c("ANOVA" = summary(fit)[[1]]$'Pr(>F)'[1])
  }
  return(result)
}

smart_anova(t6)


#    Задача 7 - разбить количественную переменную по двум факторам
#     и для каждой группы из 4 определить р-значение теста Шапиро-Вилка

t7 <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")

t71 <- data.frame(mtcars[,c("mpg","am","vs")])

task7 <- as.data.frame.table(by(t71[,"mpg"], t71[,c("am","vs")], function(x) shapiro.test(x)$p.value, simplify = TRUE), responseName = "p_value")
task7

normality_by <- function(df) {
  p_values <- as.data.frame.table(by(df[,1], df[,c(2,3)], function(x) shapiro.test(x)$p.value), responseName = "p_value")
  return(p_values)
}

normality_by(mtcars[,c("mpg","am","vs")])
normality_by(t7)


#    Задача 8 - визуализировать распределение Sepal.Length
#    в трех группах данных iris

str(iris)

obj <- ggplot(iris, aes(Sepal.Length, fill = Species))+
  geom_density(alpha = 0.2)

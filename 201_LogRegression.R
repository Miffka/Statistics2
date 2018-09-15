setwd(as.character("C:/Users/miffka/Documents/!DataMining/Stats2"))
getwd()

library(dplyr)
library(ggplot2)
library(vcd)

#Week2 - logistic regression

# Считаем данные
titanic <- read.csv("https://stepic.org/media/attachments/course/524/train.csv")
titanic <- na.omit(titanic)
glimpse(titanic)
titanic <- mutate(titanic, 
                  Survived = factor(Survived, labels = c("No", "Yes")), 
                  Pclass = factor(Pclass, labels = c("First", "Second", "Third")), 
                  Sex = factor(Sex, labels = c("Female", "Male")))


#    Lesson 2 - intersept-only

simple_fit <- glm(Survived ~ 1, titanic, family = "binomial")
coef(simple_fit)

summary(simple_fit)
#смотрим на интерсепт - он значимо отличается от ноля,
#вероятность не выжить значимо больше вероятности выжить
#z - это соотношение среднего к ошибке, от него и идет p-значение

exp(coef(simple_fit)) #возвращаем сразу шансы!!!


#    Lesson 3 - one nominative predictor

fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
coef(fit1)
table(titanic$Survived, titanic$Sex)

summary(fit1)
#что такое интерсепт? - это такая градация фактора-предиктора,
#которой нет среди коэффициентов
#интерсепт - натуральный логарифм шансов положительного исхода для женщин
#коэффициент при SexMale - логарифм отношения шансов положительного
#исхода для мужчин и шансов для женщин

odds_male <- 93/360
odds_female <- 197/64
log(odds_female)
log(odds_male/odds_female)

fit11 <- glm(Survived ~ Pclass, titanic, family = "binomial")
summary(fit11)

#а что если мы спрашиваем, значим ли предиктор?
#сравниваем модель без предикторов, с моделью с предиктором!
anova(simple_fit, fit1, test="Chisq")
#то же самое - по умолчанию anova сравнивает с нулевой моделью
anova(fit1, test="Chisq")


#    Lesson 4 - two nominative predictors

# Построим мозаичный график
mosaic(~ Sex + Survived | Pclass, data=titanic) 
fit2 <- glm(Survived ~ Sex*Pclass, titanic, family="binomial")
coef(fit2)
summary(fit2)

table(titanic$Survived, titanic$Pclass, titanic$Sex)
#Intersept
female_p1_odds = 82/3
log(female_p1_odds) #это интерсепт
#SexMale
male_p1_odds = 40/61
log(male_p1_odds)
log(male_p1_odds/female_p1_odds) #это SexMale

# сравнение моделей
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")

anova(fit1, fit2, test="Chisq")

fit3 <- glm(Survived ~ Sex + Pclass + Age, titanic, family = "binomial")
summary(fit3)
anova(fit3, test="Chisq")

?glm


#     Lesson 6 - non-parametric tests and regression

#т-критерий - разница между средними групп, деленная на нормированное
#стандартное отклонение
#  Требования к т-тесту
# 1) независимость каждого из наблюдений
# 2) гомогенность дисперсий (желательно, но нарушение некритично)
# 3) нормальное распределение исследуемого признака в генеральной совокупности
# 4) достаточный объем выборки 
#         Проверка на нормальность

# shapiro.test()
# интересно - на очень больших выборках (тысячи наблюдений) наблюдается
# согласно тесту Шапиро-Вилка отклонение выборки от нормального распределения
# но по qq-plot корреляция более 99.9%
#Необходимо проверять распределение на нормальность!
# Если оно ненормальное - применяем тест Манна-Уитни.
# Тест Манна-Уитни нельзя применять для сравнения выборок с различным
# направлением асимметрии
#Если есть основания считать, что нарушается и нормальность распределения,
# и гомогенность дисперсии, нужно применять непараметрический аналог
# дисперсионного анализа - критерий Краскела-Уоллиса. Основная статистика
# этого теста - дисперсия сумм рангов в группах, которая подчиняется
# распределению хи-квадрат.
#Мощность непараметрических тестов в асимметричных выборках выше!!!




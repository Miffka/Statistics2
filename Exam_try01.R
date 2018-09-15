#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Users/miffka/Documents/!DataMining/Stats2")
library(QuantPsyc)


#     Задача 2
# Выясните, имеет ли смысл делить посетителей на группы по дням посещений 
#(отличаются ли посетители в своей средней покупательной активности по признаку day).
#В ответе укажите p-value для гипотезы о том, что типы посещения не различаются.

task2 <- read.csv("https://stepik.org/media/attachments/lesson/32437/gym_data.csv")
str(task2)
?aov
task21 <- aov(money ~ day, task2)
summary(task21)[[1]]$'Pr(>F)'[1]


#    Задача 3
Inter <- 1.433570e+01
bal <- -1.197759e-03
debt <- -2.502676e-02
infl <- 1.735660e-01
labfor <- -1.801842e-08

task3 <- Inter + bal*0 + debt*48 + infl*3 + labfor*20e+06
exp(task3)


#    Задача 4
# Проверьте гипотезу о независимости распределения факторов A и B среди наблюдений
#и напишите значение p-value для этой гипотезы.

task4 <- read.csv("https://stepik.org/media/attachments/lesson/33474/contigency_table_analysis_2.csv")
str(task4)

task4$A <- as.factor(task4$A)
task4$B <- as.factor(task4$B)
str(task4)

task41 <- table(task4[, 2:3])
?fisher.test
fit4 <- fisher.test(task41)
fit4$p.value
summary(fit4)
fit41 <- chisq.test(task41)
fit41$p.value


#    Задача 6

task6 <- read.csv("https://stepik.org/media/attachments/lesson/32437/hierarchical_clustering_1.csv")
centr6 <- read.csv("https://stepik.org/media/attachments/lesson/33474/centers_2.csv")

?kmeans
str(task6)
str(centr6)
centr61 <- centr6[,-1]
task6$clust <- kmeans(task6, centr61)$cluster
str(task6)
table(task6$clust) #отсюда берем наименьшее число - это 80


#    Задача 8
# Проведите анализ главных компонент этих данных (не забудьте нормализовать данные!) 
#и укажите, какой процент общей дисперсии исходных данных (число в [0;1])
#объясняют первые три главные компоненты.

task8 <- read.csv("https://stepik.org/media/attachments/lesson/32437/world_finance_data.csv")
str(task8)
task81 <- task8[, -1]
# Необходимо нормализовать данные - как мы это делаем-то?
?scale #вот эта функция все сделает!!!

task82 <- sapply(task81, scale)
fit8 <- prcomp(task82)
fit81 <- summary(fit8)
fit81$importance[3, 3] #вот и ответ!

#setwd(as.character("C:/Users/miffka/Documents/!DataMining/Stats2"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/Stats2")
getwd()

library(dplyr)
library(ggplot2)
library(vcd)
library(psych)
library(cluster)
library(ape)

# начнем с визуализации данных

ggplot(iris, aes(x=Sepal.Length, y=Petal.Width, size = 8))+
  geom_point()
#видим два возможных кластера данных - нижний, средний и верхний

#    Метод к-средних
# 1) решаем, сколько кластеров нам нужно (например, 3)
# 2) случайно выбираем начальные позиции центроидов кластера
# 3) выводим центроиды на наилучшие позиции
??"k-means"

clust <- kmeans(iris[,c("Sepal.Length", "Petal.Width")], 3)
iris$cluster <- factor(clust$cluster)
ggplot(iris, aes(x=Sepal.Length, y=Petal.Width, size = cluster, col = Species))+
  geom_point()
?kmeans

# Если добавление центроида сильно уменьшает внутригрупповую сумму квадратов,
# а дальнейшее добавление ее не меняет,
# эта точка и является оптимальным числом центроидов.

tssq1 <- array()
i <- 1
for (i in seq(1,15)){
  tssq1[i] <- kmeans(iris[,c("Sepal.Length", "Petal.Width")], i)$tot.withinss
}
tssq1 <- as.data.frame(tssq1)
#можем посмотреть и на дотплот - обвал явно видно
ggplot(tssq1, aes(x=tssq1))+
  geom_dotplot()

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width))+
  geom_point()

tssq2 <- array()
i <- 1
for (i in seq(1,15)){
  tssq2[i] <- kmeans(iris[,c("Petal.Length", "Petal.Width")], i)$tot.withinss
}
tssq2 <- as.data.frame(tssq2)
ggplot(tssq2, aes(x=tssq2))+
  geom_dotplot()

clust2 <- kmeans(iris[,c("Petal.Length", "Petal.Width")], 3)
iris$ClusterPetal <- factor(clust2$cluster)

ggplot(iris, aes(x=Petal.Length, y=Petal.Width, size=ClusterPetal, col=Species))+
  geom_point()


#    Иерархическая кластеризация

??"hierarchy"
?agnes
?pltree
hirclust <- agnes(iris[,c("Petal.Length", "Sepal.Width")], diss = FALSE, par.method = "flexible")
typeof(hirclust) #а это просто список)
pltree(hirclust)

#  Пример1

set.seed(222)
tr <- rtree(20, tip.label = c("B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U"))
#левое дерево
plot.phylo(tr)
plot.phylo(tr, use.edge.length=FALSE)


#    Анализ главных компонент

?princomp
str(mtcars)
pc1 <- princomp(mtcars[,c("mpg", "wt")])
summary(pc1)

?biplot
biplot(mtcars[,c("mpg", "wt")], pc1)

#  Пример2
head(swiss)
data(swiss)
fit <- prcomp(swiss, center = T)
plot(fit, type = "l")
summary(fit)
biplot(fit)


fit <- factanal(swiss, factors = 2, rotation = "varimax")
print(fit)

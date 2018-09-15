#setwd(as.character("C:/Users/miffka/Documents/!DataMining/Stats2"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/Stats2")
getwd()

library(dplyr)
library(ggplot2)
library(vcd)
library(psych)
library(cluster)
library(ape)


#    Задача 1 - написать функцию для кластеризации на произвольное число данных

test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
str(test_data1)

#dist_matrix <- dist(test_data) # расчет матрицы расстояний
#fit <- hclust(dist_matrix) # иерархическая кластеризация 
#cluster <- cutree(fit, 3) # номер кластера для каждого наблюдения
#test_data$cluster <- as.factor(cluster) #записываем переменную в датафрейм

smart_hclust <- function(df, n){
  df$cluster <- as.factor(cutree(hclust(dist(df)), n))
  return(df)
}

smart_hclust(test_data1, 3)


#    Задача 2 - по каким переменным наблюдается значимое отличие у кластеров

test_data21 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
test_data22 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")

task21 <- smart_hclust(test_data21, 2)
str(test_data21)
task22 <- aov(V1 ~ cluster, task21)
summary(task22)
summary(task22)[[1]]$'Pr(>F)'[1] #вытаскиваем р-значение из ановы
#а теперь применим все перечисленное к датафрейму через apply
task221 <- apply(task21[,-ncol(task21)], 2, function(col) summary(aov(col ~ cluster, task21))[[1]]$'Pr(>F)'[1])
names(task221[task221<0.05])
?names

get_difference <- function(test_data, n_cluster) {
  test_data$cluster <- as.factor(cutree(hclust(dist(test_data)), n_cluster))
  p_val <- apply(test_data[,-ncol(test_data)], 2, function(col) summary(aov(col ~ cluster, test_data))[[1]]$'Pr(>F)'[1])
  return(names(p_val[p_val<0.05]))
}

get_difference(test_data21, 2)
get_difference(test_data22, 2)


#    Задача 3 - добавление двух первых главных компонент

test_data31 <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
task31 <- prcomp(test_data31)
test_data31$PC1 <- task31$x[,"PC1"]
summary(task31)
?prcomp

get_pc <- function(df) {
  df <- cbind(df, prcomp(df)$x[,c("PC1","PC2")])
  return(df)
}

get_pc(test_data31)


#    Задача 4 - добавляем столько главных компонент, чтобы они объясняли
#      90 и более процентов изменчивости

fit41 <- prcomp(swiss)
fit42 <- summary(fit41)$importance
#в этой строке мы делаем почти невозможное - мы склеиваем вектор, состоящий
# из значений кумулятивной пропорции дисперсии менее 0,9 с первым значением
# вектора, состоящего из значений кумулятивной пропорции дисперсии более 0,9
names(c(fit42[3,][fit42[3,]<0.9], fit42[3,][fit42[3,]>0.9][1]))

get_pca2 <- function(df) {
  pcan <- prcomp(df)
  pci <- summary(pcan)$importance
  col_to_past <- names(c(pci[3,][pci[3,]<0.9], pci[3,][pci[3,]>0.9][1]))
  df <- cbind(df, pcan$x[,c(col_to_past)])
  return(df)
}

str(get_pca2(swiss))


#    Задача 5 - боремся с мультиколлинеарностью типа V1 = k*V2 + b

test_data51 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")

summary(lm(V1~V2, test_data51))$r.squared
summary(lm(V1~V3, test_data51))$r.squared
summary(lm(V1~V4, test_data51))$r.squared

test_data52 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_1.csv")
summary(lm(V1~V2, test_data52))$r.squared
summary(lm(V1~V3, test_data52))$r.squared
summary(lm(V1~V4, test_data52))$r.squared

task5 <- apply(test_data51[,-c(1)], 2, function(col) summary(lm(unlist(test_data51[,1]) ~ col, test_data51))$r.squared)

# мое решение 
is_multicol <- function(df) {
  mc <- c()
  for (i in c(1:(ncol(df)-1))) {
    for (j in c((i+1):ncol(df))) {
      rsq <- summary(lm(unlist(df[,i]) ~ unlist(df[,j]), df))$r.squared
      if (isTRUE(all.equal(rsq, 1))) {
        mc <- append(mc, c(i,j))
      }
    }
  }
  df1 <- df[,mc]
  if (length(mc) == 0) {
    return("There is no collinearity in the data")
  }
  else {
    return(colnames(df1))
  }
}

is_multicol(test_data52)
test_data53 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
is_multicol(test_data53)
?lower.tri()

# Правильное решение - анализ корреляционной матрицы
is_multicol <- function(d){    
  d <- abs(cor(d))    #строим модуль матрицы корреляции
  d[lower.tri(d)] <- 0    #зануляем нижний треугольник матрицы корреляции
  diag(d) <- 0    #зануляем диагональ матрицы корреляции
  index <- which((1-d) < 1e-10, arr.ind = T)    #заполняем вектор значениями индекса для единиц
  if (length(index) == 0){      
    return('There is no collinearity in the data')    
  } else {      
    return(rownames(d)[index])    #выводим названия строк)  
  }      
}


#    Задача 5 - в swiss выделить два кластера при помощи иерархической кластеризации

str(swiss)
#dist_matrix <- dist(test_data) # расчет матрицы расстояний
#fit <- hclust(dist_matrix) # иерархическая кластеризация 
#cluster <- cutree(fit, 3) # номер кластера для каждого наблюдения
#test_data$cluster <- as.factor(cluster) #записываем переменную в датафрейм

swiss$cluster <- as.factor(cutree((hclust(dist(swiss))), 2))
ggplot(swiss, aes(Education, Catholic, col = cluster))+
  geom_point()+
  geom_smooth(method = "lm")

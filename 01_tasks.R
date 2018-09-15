#задача 0 - если положения NA в двух векторах совпадают, выводит True, иначе - False

NA_position <- function(x, y) {
  all(is.na(x) == is.na(y))
}

NA_position(c(1, NA, 3), c(2, 3, NA))


#                 Задача 1 - smart_test

smart_test <- function(df) {
  df[,1] <- as.factor(df[,1]) #делаем первую переменную фактором
  df[,2] <- as.factor(df[,2]) #делаем вторую переменную фактором
  
  table11 <- table(df[,1], df[,2]) #составляем таблицу сопряженности
  
  if(any(table11 < 5)) { #если хотя бы одно значение в таблице меньше 5
    return(c(fisher.test(table11)$p.value)) #возвращаем вектор из 
    #р-значения для теста Фишера
  }
  else { #в противном случае 
    k <- chisq.test(table11) #делаем тест хи-квадрат
    return(c(k$statistic, k$parameter, k$p.value)) #и возвращаем вектор нужный
  }
}

smart_test(mtcars[1:20, c("am", "vs")])
smart_test(mtcars[,c("am","vs")])


#            Задача 2 - минимальный уровень р-значимости

most_significant <- function(df) {
  task21 <- vector()
  for(j in colnames(df)) {
    task21[j] <- chisq.test(table(df[, j]))$p.value
  }
  return(names(which(task21 == min(task21))))
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
most_significant(test_data)


#         Задача 3 - создаем новую факторную переменную в iris

iris$important_cases <- factor(apply(apply(iris[,1:4], 2, function(col) col>mean(col)), 1, 
                         function(row) sum(row)>=3), labels = c("No", "Yes"))
str(iris$important_cases)
table(iris$important_cases)

#Подробное описание подхода:
#во вложенном apply применяем функцию function(col) к столбцам(2)
#эта функция возвращает для каждого значения в столбце логическое выражение
#которое истинно, если значение больше среднего по столбцу
#во внешний apply мы вводим датафрейм, состоящий из таких столбцов
#и применяем к нему построчно(1) функцию function(row)
#эта функция возвращает истину, если сумма по строке (т.е количество истин)
#больше или равно 3




#         Задача 4 - обобщаем предыдущую на произвольное число 
#            количественных переменных и выводим датафрейм на печать

get_important_cases <- function(df) {
  df$important_cases <- factor(apply(apply(df, 2, function(col) col>mean(col)),
                              1, function(row) sum(row)>(ncol(df)/2)), labels = c("No", "Yes"))
  return(df)
}

test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))
get_important_cases(test_data)


#          Задача 5 - находим моду у числового вектора
#          если мод несколько - выводим все в виде числового вектора

stat_mode <- function(v) {
   freqdf <- data.frame(table(v)) #преобразуем вектор в таблицу частот
   freqdf$v <- as.character(freqdf$v) #избавляемся от фактора
   return(as.integer(freqdf$v[which(freqdf$Freq == max(freqdf$Freq))])) #выводим наиболее частые частоты
}

task51 <- c(1, 1, 1, 2, 3, 3, 3)
task52 <- c(1, 2, 3, 3, 3, 4, 5)
task53 <- c(13,17,14,15,9,15,6,19,5,2,18,13,14,3,9,15,19,19)

stat_mode(task51)
stat_mode(task52)
stat_mode(task53)


#       Задача 6 - максимальное значение стандартизованного остатка
#        функция должна возвращать вектор из названия строчки и столбца

max_resid <- function(df) {
  chires <- chisq.test(table(df))
  stdres1 <- as.data.frame.array(chires$stdres)
  where <- which(stdres1 == max(stdres1), arr.ind = TRUE)
  return(c(rownames(stdres1[where[1],]), colnames(stdres1[where[2]])))
}

task6_test <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
task61 <- chisq.test(table(task6_test))
task62 <- as.data.frame.array(task61$stdres)
position <- which(task62 == max(task62), arr.ind = TRUE)

rownames(task62[position[1],])
colnames(task62[position[2]])

max_resid(task6_test)


#         Задача 7 - построить график в ggplot2

library(ggplot2)
str(diamonds)

obj <- ggplot(diamonds, aes(color, fill = cut))+
  geom_bar(position = "dodge")
?ggplot

all_users <- read.csv("/Users/alena/shmya_final_version.csv", header = TRUE, sep =",", quote = "\"", dec = ".")
all_users_grouped_uid <- read.csv("/Users/alena/Y_data.csv", header = TRUE, sep =";", quote = "\"", dec = ".")

install.packages("MASS")
install.packages("aod")
install.packages("sandwich")
install.packages("car")
install.packages("caret")
install.packages("foreign")
install.packages('e1071', dependencies=TRUE)
install.packages("tidyverse")
install.packages("dplyr")
library("HSAUR")
library("dplyr")
library("ggplot2")
library("car")
library("MASS")
library("aod")
library("sandwich")
library("corrplot")
library("caret")
library("foreign")
library("tidyverse")

#Попробуем сгруппировать здесь сырые данные
all_users_grouped_r %>%
  group_by(all_users$uid) %>%
  summarise()

#Смотрим базовые описательные статистики
summary(all_users)
summary(all_users_grouped_uid)

#Проверим корреляцию между кол-вом столовых приборов и суммой чаевых
cor(all_users$cutlery, all_users$tips)
cor(all_users_grouped_uid$cutlery_grouped_uid, all_users_groped_uid$tips_grouped_uid)
#Коэффициент корреляции в сырых данных 0.16 -> связи практически нет
#в усредненных по uid данных - 0.4 -> эти данные интереснее для анализа

#Разобьём юзеров на сегменты
users_segment1 <- filter(all_users_grouped_uid, cutlery_grouped_uid <= 2)
users_segment2 <- filter(all_users_grouped_uid, cutlery_grouped_uid > 2)
cor(users_segment1$cutlery_grouped_uid, users_segment1$tips_grouped_uid)
cor(users_segment2$cutlery_grouped_uid, users_segment2$tips_grouped_uid)

#Посмотрим, есть ли графическая зависимость суммы чаевых от кол-ва приборов
qplot(users_segment1$cutlery_grouped_uid, users_segment1$tips_grouped_uid, main = "Зависимость суммы чаевых от кол-ва приборов 1 сегмент", xlab = "Количество приборов", ylab = "Сумма чаевых")
qplot(users_segment2$cutlery_grouped_uid, users_segment2$tips_grouped_uid, main = "Зависимость суммы чаевых от кол-ва приборов 2 сегмент", xlab = "Количество приборов", ylab = "Сумма чаевых")

#Проверим на нормальность визуально
#Посмотрим на распределения суммы чаевых по сегментам юзеров
qplot(users_segment1$tips_grouped_uid, main = "Распределение суммы чаевых 1 сегмент", xlab = "Количество пользователей", ylab = "Сумма чаевых")
qplot(users_segment2$tips_grouped_uid, main = "Распределение суммы чаевых 2 сегмент", xlab = "Количество пользователей", ylab = "Сумма чаевых")

library(ggpubr)
ggqqplot(users_segment1$tips_grouped_uid)
ggqqplot(users_segment2$tips_grouped_uid)

#И с помощью теста Шапиро-Уилка
shapiro.test(users_segment1$tips_grouped_uid)
shapiro.test(users_segment2$tips_grouped_uid)
#p-value для обоих выборок сильно меньше 0.05 -> чаевые распределены не нормально

#Для сравнения выборок, данные в которых распределены не нормально, будем использовать непараметрические критерии
#Так как наши выборки независимые (в каждой из них содержатся уникальных пользователи),
#будем использовать U-критерий Манна-Уитни
wilcox.test(users_segment2$tips_grouped_uid, users_segment1$tips_grouped_uid)
#p-value сильно меньше любого приемлемого уровня значимости -> на уровне значимости 0.01
#отвергается гипотеза об отсутствии различий в выборках -> действительно, берущие больше 2 комплектов приборов оставляют больше чаевых

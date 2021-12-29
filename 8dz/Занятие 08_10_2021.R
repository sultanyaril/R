# Файл с исходными данными
#data(sleep, package = "VIM")

# Файл с заполненными пропусками
load(file = "sleep_imp.Rdata")

# Раскрашенная корреляционная матрица
M <- cor(sleep_imp3)
library(corrplot)
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow",
                           "#7FFF7F", "cyan", "#007FFF", "blue","#00007F")) 
corrplot(M, method = "pie", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black", addCoef.col = "red")

# Сравните
corrplot(M, method = "color", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black", addCoef.col = "green")

corrplot(M, method = "number", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black")

# Фактор инфляции дисперсии (VIF). Изучить теоретические основы
library(car)
vif(lm(Sleep ~ BodyWgt + BrainWgt + Span + Gest + Pred + Exp + Danger,
       data = sleep_imp3))

# Диаграммы рассеяния
cars <- mtcars[, 1:7]
pairs(cars, panel = panel.smooth) #со сглаживающей кривой

# Наносим на график коэффциенты корреляции Спирмена
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "spearman"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep = "")
  # text(0.5, 0.5, txt)
  #Размер шрифта может зависеть от значения коэффициента корреляции
  if(missing(cex.cor))
    cex.cor <- 1.1/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Новый вариант вывода диаграммы
pairs(cars, panel = panel.smooth, lower.panel = panel.cor)

# Матричные диаграммы рассеяния
library(lattice)
splom(cars)

# Матричные диаграммы рассеяния с двумерными ядерными оценками плотностей
library(GGally)
ggpairs(cars, 
        upper = list(continuous = "density", combo = "box"), 
        lower = list(continuous = "points", combo = "dot"))

# Эффективные степени свободы (EDF)
library(mgcv)
summary(gam(mpg ~ s(hp) + s(qsec), data = cars)) #обобщенные аддитивные модели

# Выявление взаимосвязей для категориальных переменных
Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
# Выбираем необходимый комплект данных: воробьи с длиной крыла >= 65
I1 <- Sparrows$SpeciesCode == 1 & 
  Sparrows$Sex != "0" &
  Sparrows$wingcrd < 65
Wing1 <- Sparrows$wingcrd[I1]
Wei1 <- Sparrows$wt[I1]
Mon1 <- factor(Sparrows$Month[I1])
Sex1 <- factor(Sparrows$Sex[I1])

# Определим месяц и пол как категориальные переменные
fMonth1 <- factor(Mon1, levels = c(5, 6, 7, 8, 9),
                  labels = c("Май", "Июнь",
                             "Июль", "Август", "Сентябрь"))
fSex1 <- factor(Sex1, levels = c(4, 5),
                labels = c("Самцы", "Самки"))

# Вывод категориальной диаграммы

coplot(Wei1 ~ Wing1 | fMonth1 * fSex1,
       ylab = c("Вес (г)", "Пол"),
       xlab = c("Длина крыла (мм)", "Месяц"),
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

# Анализ данных с помощью автокорреляционной функции (АКФ): изучить теоретические основы

# Численность двух видов птиц
Waders <- read.table(file = "wader.txt", header = TRUE)
Time <- seq(1, 25)

layout(matrix(1:4, ncol = 2))
plot(Time, Waders$C.fuscicolis, type = "l", main="Бонапартов песочник", 
     xlab = "Время (2 недели)", ylab = "Численность")
acf(Waders$C.fuscicolis, main = "АКФ", xlab = "Лаг", ylab = "Значение")
plot(Time, Waders$L.dominicanus, type = "l", main="Доминиканская чайка",
     xlab = "Время (2 недели)", ylab = "Численность")
acf(Waders$L.dominicanus, main = "АКФ", xlab = "Лаг", ylab = "Значение")


# Дисперсионный анализ: изучить теоретические основы

# Данные о весе кустов томатов, которые выращивали при поливе водой, с удобрением
# и удобрением+гербицидом 
tomato <- data.frame(weight =
                       c(1.5, 1.9, 1.3, 1.5, 2.4, 1.5, # water
                         1.5, 1.2, 1.2, 2.1, 2.9, 1.6, # nutrient
                         1.9, 1.6, 0.8, 1.15, 0.9, 1.6), # nutrient+24D
                     trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                               c(6, 6, 6)))
is.factor(tomato$trt)
tomato$trt<-factor(tomato$trt,rep(c("Water", "Nutrient", "Nutrient+24D")))
is.factor(tomato$trt)
levels(tomato$trt)
tomato$trt <- relevel(tomato$trt, ref = "Water")

# Таблица со средними значениями
Means <- data.frame(weight = as.numeric(tapply(tomato$weight,tomato$trt, mean)),
                    trt = rep("Means", 3))
Means

# Добавляем Means в таблицу tomato
tomato <- rbind(tomato, Means)

layout(matrix(1:1, ncol = 1))

stripchart(weight ~ trt, data = tomato, pch = 19,
           col = c("blue", "red", "green", "black"),
           ylab = "Условия", xlab = "Вес, кг")

# Отмечаем, какое среднее значение к какой группе относится
points(x = Means$weight, y = c(4, 4, 4), pch = 19,
       col = c("blue", "red", "green"))

# Однофакторный дисперсионный анализ
summary(aov(weight ~ trt, data = tomato))

# Линейная модель: изучить параметры вывода
M <- lm(weight ~ trt, data = tomato)
summary(M)

# Таблица дисперсионного анализа, сравнить с результатами aov()
anova(M)

# Двухфакторный дисперсионный анализ
library(HSAUR2)
# Состав корма крыс отличается высоким и низким содержанием белка, который может
# быть растительным (cereal) и животным (beef)

# Изучается влияние типа питания на прирост веса крыс
data(weightgain)
str(weightgain)

library(ggplot2)
ggplot(data = weightgain, aes(x = type, y = weightgain)) + 
  geom_boxplot(aes(fill = source))

require(doBy)
summaryBy(weightgain ~ type + source, data = weightgain,
          FUN = c(mean, sd, length))

# График плана эксперимента
plot.design(weightgain)

# Порядок фактор не важен только в случае, если данные - сбалансированные
M <- aov(weightgain ~ source + type + source:type, 
          data = weightgain)
summary(M)

# Используем функцию для явной оценки линейной модели
M <- lm(weightgain ~ type*source, data = weightgain)
summary(M)
anova(M)

# Вернемся к данным про воробьев

# Удаляем данные за май и сентябрь
df <- data.frame(weight = Wei1, length = Wing1, sex = fSex1, month = fMonth1)
df1 <- df[df$month != "May" & df$month != "Sep", ]

# Трехфакторный
M <- lm(weight ~ length*month*sex, data = df1)
DT <- anova(M)
DT

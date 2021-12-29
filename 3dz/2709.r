# Генерация 250 псевдослучайных чисел из нормального распределения с заданными параметрами
X <- rnorm(n = 250, mean = 15, sd  = 5) 
density(X)
hist(X, breaks = 20, freq = FALSE, col = "lightblue",
     xlab = "Переменная X",
     ylab = "Плотность вероятности",
     main = paste("n =", i))
lines(density(X), col = "red", lwd = 2)
lines(density(X, bw = 0.8), col = "blue", lwd = 2)
# Изменяя размер выборки, проверьте, как изменяется качество подгонки гистограммы
# В том числе - для разных значений параметров density()

# Опция freq = FALSE позволяет рисовать гистограмму с нормированными стоблцами (площадь=1)
layout(matrix(1, ncol=1)) 
for (i in c(50,250,1000)) {
        X <- rnorm(n = i, mean = 15, sd  = 5) 
        hist(X, breaks = 20, freq = FALSE, col = "lightblue",
                xlab = "Переменная X",
                ylab = "Плотность вероятности",
                main = paste("n =", i))
        lines(density(X), col = "red", lwd = 2)
        lines(density(X, bw = 0.8), col = "blue", lwd = 2)
}

for (i in c(5, 15, 50)) {
        X <- rnorm(n = 1000, mean = i, sd  = 5) 
        hist(X, breaks = 20, freq = FALSE, col = "lightblue",
             xlab = "Переменная X",
             ylab = "Плотность вероятности",
             main = paste("mean =", i))
        lines(density(X), col = "red", lwd = 2)
        lines(density(X, bw = 0.8), col = "blue", lwd = 2)        
}

for (i in c(1, 5, 40)) {
        X <- rnorm(n = 1000, mean = 15, sd  = i) 
        hist(X, breaks = 20, freq = FALSE, col = "lightblue",
             xlab = "Переменная X",
             ylab = "Плотность вероятности",
             main = paste("sd =", i))
        lines(density(X), col = "red", lwd = 2)
        lines(density(X, bw = 0.8), col = "blue", lwd = 2)        
}



# Функция density() используется для отображения (ядерной) оценки плотности
# Изучите ее параметры

layout(matrix(1, ncol=1)) 

X <- rnorm(n = 250, mean = 15, sd  = 5)
hist(X, breaks = 20, freq = FALSE, col = "lightblue",
     xlab = "Переменная X",
     ylab = "Плотность вероятности",
     main = "Нормальное распределение"
)
lines(density(X), col = "red", lwd = 2)
lines(density(X, bw = 0.2), col = "blue", lwd = 2)
lines(density(X, bw = 2), col = "green", lwd = 2)
lines(density(X, bw = 0.8), col = "black", lwd = 2)
legend("topright", legend=c("1","0.2","2","0.8"), fill=c("red","blue","green","black"))


boxplot(X,
        ylab = "Инсектициды",
        xlab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов",
        col = "coral", horizontal = TRUE,
        data = InsectSprays)
# InsectSprays – данные, полученных в ходе эксперимента по изучению эффективности 6 видов инсектицидных средств.
# Каждым из них обработаны 12 растений, после чего подсчитано количество выживших насекомых.
# Встроенный набор данных
data(InsectSprays)

# В InsectSprays два столбца:
# count: результаты подсчета насекомых
# spray: коды инсектицидных средств от А до F

head(InsectSprays)
attach(InsectSprays)

# Установка пакета sm ("smoothing methods"), если его нет в системе
#install.packages("sm")

# Если библиотека уже есть
library(sm)

#Сравнение всех групп по кривым ядерной плотности
sm.density.compare(count, spray, lwd = 2, xlab = "Число насекомых", ylab = "Плотности")
# Специальная команда для заголовка
title(main = "Кривые ядерной плотности")
# Составляем вектор с кодами использованных цветов
Colfill <- c(2:(2 + length(levels(spray))))
# добавляем легенду туда, куда кликнем мышью
legend(locator(1), levels(spray), fill = Colfill)

# Двумерное распределение
# Indometh – еще один встроенный набор данных (скорости выведения препарата индометацин)
data(Indometh)
attach(Indometh)
library(MASS)

# Двумерная ядерная оценка для совместного распределения 
f <- kde2d(time, conc) 
# Создание окрашенной прямоугольной сетки
image(f, xlab="Время выведения", ylab="Концентрация индометацина")
# Добавляем на график изолинии
contour(f, add = TRUE)

# Примеры использования функции cdplot ("conditional density plot")
# Вывод на одном графике плотности вероятности для каждого уровня качественной переменной 

# Библиотека с медицинскими данными
library(HSAUR2)
data(plasma)
summary(plasma)

# Задание вывода нескольких графиков на одном листе
layout(matrix(1:2, ncol = 2)) 
# Первый аргумент позволяет задавать порядок вывода графиков вручную
#layout(matrix(2:1, ncol = 2)) 

# Cравните с par(mfrow) из прошлого занятия

# ESR - скорость оседания эритроцитов: важно не точное значение, а преодоление порога в 20 мм/ч
# Изучается зависимость количества того или иного белка и вероятности превышения порога
cdplot(ESR ~ fibrinogen, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 мм/ч", "> 20 мм/ч"), data = plasma)
cdplot(ESR ~ globulin, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 мм/ч", "> 20 мм/ч"), data = plasma)

# Есть неявный вызов density(), можно изменять ее параметры
cdplot(ESR ~ fibrinogen, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 мм/ч", "> 20 мм/ч"), bw = 0.9, data = plasma)
cdplot(ESR ~ globulin, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 мм/ч", "> 20 мм/ч"), bw = 0.9, data = plasma)

# Примеры box plot (диаграммы размахов / "ящики с усами")
# Точки или линию, соответствующую некоторой мере центральной тенденции в данных,
# окружает прямоугольник ("ящик"), длина которого соответствует определенному
# показателю разброса. Дополнительно от этого прямоугольника отходят "усы",
# также отражающие разброс в данных или, реже, точность оценки меры центральной тенденции
# + разница между двумя и более группами

# Слева от знака ~ указывается зависимая переменная, справа – предикторы
# Изучить параметры
boxplot(count ~ spray,
        xlab = "Инсектициды",
        ylab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов",
        col = "coral", data = InsectSprays)

# Горизонтальное расположение "ящиков"
boxplot(count ~ spray,
        ylab = "Инсектициды",
        xlab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов",
        col = "coral", horizontal = TRUE,
        data = InsectSprays)

boxplot(count ~ spray,
        ylab = "Инсектициды",
        xlab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов",
        col = "coral", horizontal = TRUE,
        data = InsectSprays,
        whisklty=2,  # тип усов (3=dashed)
        medlty=0,  # тип прямой на медиане (0 чтобы убрать)
        boxcol="green",  # цвет границы коробки
        staplecol = "violet", # цвет максимумов и минимумов
        outlwd = 15  # ширина выбросов
)

#Не забываем про следующую команду (при необходимости)
par(mfrow = c(1,1))

# Bag plot
library(aplpack)
bagplot(time, conc, xlab = "Время выведения",
        ylab = "Концентрация индометацина", main = "Мешок с усами")


# Примеры столбчатых диаграмм

# Пример применения tapply
Means <- tapply(count, spray, mean)
Means

# Изучить параметры функции
barplot(Means, col = "steelblue",
        xlab = "Инсектицид",
        ylab = "Количество выживших насекомых",
        border = "red", width = sqrt(Means))



#Горизонтальная версия 
barplot(Means, density = 20, angle = -45, space = 2,
        col = "red", horiz = TRUE, las = 1,
        ylab = "Инсектицид", 
        xlab = "Количество выживших насекомых")

# Столбчатые диаграммы для сгруппированных данных
library(MASS)
data(genotype)
# Лабораторные крысы четырех разных генотипов (A, B, I, J)
# Выводок (Litter), полученный самками (Mother) каждого генотипа, отдавался на вскармливание
# На 28-й день измерен вес у крысят (Wt) с целью установить влияние генотипа на этот показатель
head(genotype)
means = with(genotype, tapply(Wt, list(Litter, Mother), mean))
means

barplot(means, beside = TRUE,
       col = topo.colors(4),
       legend.text = rownames(means),
       xlab = "Выводок", ylab = "Вес, г",
       ylim = c(0, 100))

barplot(means, beside = F, # расположены рядом друг с другом
        angle=45,  # угол наклона полос в графике
        density=20,  # частота полос внутри графика
        space = 0.4,  # расстояние между блоками
        col = topo.colors(4),
        xlab = "Выводок", ylab = "Вес, г")

#Вычисление стандартных отклонений
sds = with(genotype, tapply(Wt, list(Litter, Mother), sd))
sds

# Диаграмма используется далее как самостоятельный объект
b <- barplot(means, ylim = c(min(pretty(means-sds)),
             max(pretty(means+sds))),
             col = topo.colors(4),
             beside = TRUE, xpd = FALSE,
             ylab = "Вес, г", xlab = "Выводок",
             legend.text=rownames(means))

# Отклонения от среднего на величину среднеквадратического отклонения
arrows(b, means + sds, b, means - sds, angle = 90, code = 3, length = 0.05)

# Точечные диаграммы Кливленда

# Данные журнала Motor Trend (1974)
data(mtcars)
dotchart(mtcars$mpg * 0.425144, labels = row.names(mtcars),
         main="Расстояние (в км), которое проезжает автомобиль на 1 литре",
         xlab="Значение", cex = 0.8)
# Перерисовать график для расхода топлива в литрах + км

# Для упрощения анализа сортируем данные
x <- mtcars[order(mtcars$mpg), ]

# Создание факторов вручную
x$cyl <- factor(x$cyl)

# Разные цвеа для отображения
x$color[x$cyl==4] <- 1
x$color[x$cyl==6] <- 2
x$color[x$cyl==8] <- 3
dotchart(x$mpg, labels = row.names(x),
         groups = x$cyl, gcolor = "blue", pch = 16,
         main="Экономичность двигателя у 32 моделей автомобилей",
         xlab="Миль/галлон", cex = 0.8, color = x$color)
?mtcars
# С помощью команды ?mtcars ознакомиться со структурой и описанием данных
# Провести аналогичный анализ для какого-либо другого столбца 
x <- mtcars[order(mtcars$hp), ]
x$carb <- factor(x$carb)
cnt = 1
for (i in levels(factor(x$carb))) {
  x$color[x$carb==i] <- cnt
  cnt = cnt + 1
}

dotchart(x$hp, labels = row.names(x),
         groups = x$carb, gcolor = "blue", pch = 16,
         main="Мощности у 32 моделей автомобилей",
         xlab="Лошадинных сил", cex = 0.8, color = x$color)

# Одномерные диаграммы рассеяния
data(InsectSprays)
names(InsectSprays)

stripchart(InsectSprays$count ~ InsectSprays$spray,
           ylab = "Количество выживших насекомых",
           xlab = "Инсектицид",
           vertical = TRUE,
           method = "jitter",
           jitter = 0.1,
           pch = 1, col = "purple")

# Параметр jitter используется для внесения случайных сдвигов в данные для удобства визуализации

# boxplot() изображает только выбросы, но в связке со stripchart() - все наблюдения

# Параметр outline определяет необходимость вывода выбросов
boxplot(count ~ spray, 
        outline = FALSE, xlab = "Инсектициды",
        ylab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов",
        col = "wheat2", data = InsectSprays)

# Параметр add позволяет нанести точки на уже готовый график
stripchart(count ~ spray, method="stack", 
           data = InsectSprays, add = TRUE,
           pch = 1, , lwd = 1.7, col = "purple", vertical = TRUE)

#Реализовать для собственных данных

# Оценка выборочных характеристик
data(mtcars)

# Среднее арифметическое
mean(mtcars$mpg)

# Медиана
median(mtcars$mpg)

# Дисперсия
var(mtcars$mpg)

# Стандартное отклонение
sd(mtcars$mpg)

# Минимальное значение
min(mtcars$mpg)

# Максимальное значение
max(mtcars$mpg)
max(mtcars$disp)

# Стандартная ошибка среднего - вручную
SEmpg = sd(mtcars$mpg)/sqrt(length(mtcars$mpg))

# Квантили
quantile(mtcars$mpg) #квартили
quantile(mtcars$mpg, p = seq(0, 1, 0.1))

# Интерквантильный размах
IQR(mtcars$mpg)

# Использование функций других пакетов
library(moments)  #загрузка пакета moments
kurtosis(mtcars$mpg, na.rm = TRUE)
skewness(mtcars$mpg, na.rm = TRUE)

# Вручную заносим отсутствующее значение
mtcars$mpg[3] <- NA

head(mtcars$mpg)
mean(mtcars$mpg)

# Чтобы считать корректно и в данной ситуации
mean(mtcars$mpg, na.rm = TRUE)

# Вывод полного размера с учетом NA
length(mtcars$mpg)
# А теперь - без
sum(!is.na(mtcars$mpg))

# Поиск максимума и минимума
which.min(mtcars$mpg)
which.max(mtcars$mpg)

# Определение их местоположения в данных (названия автомобилей)
rownames(mtcars)[which.min(mtcars$mpg)]
rownames(mtcars)[which.max(mtcars$mpg)]

# Использование функции apply:  средний объем двигателя у моделей с автоматической (0)
# и ручной (1) коробками передач 
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = mean)
# tapply(X = mtcars$disp, INDEX = list(mtcars$am, mtcars$vs), FUN = mean)
SE <- function(x) {sd(x)/sqrt(length(x))}
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = SE)

# Использование функции summary():
summary(mtcars)
summary(mtcars$mpg)

# vs и am - факторы, так их и определяем
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
is.factor(mtcars$vs)
is.factor(mtcars$am)
summary(mtcars)

# Пакет Hmisc, функция describe()
library("Hmisc")
describe(mtcars)

# Пакет pastecs, функция stat.desc():
library(pastecs)
stat.desc(mtcars)

# Пакет psych, функция describe.by() - расчет параметров
# описательной статистики для каждого уровня некоторого фактора
install.packages("psych")
library(psych)
describeBy(mtcars, mtcars$am)

# Пакет doBy, функция summaryBy()
library(doBy)
summaryBy(mpg + wt ~ cyl + vs, data = mtcars,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

# Идентификация выбросов в данных
install.packages("outliers")
library(outliers)
set.seed(1234)
x = rnorm(10)

grubbs.test(x) #единственный выброс, положение может задаваться автоматически или вручную
grubbs.test(x,type=20) #2 выброса на одном хвосте
grubbs.test(x,type=11) #2 выброса на разных хвосте

dixon.test(x)
dixon.test(x,opposite=TRUE)
dixon.test(x,type=10)

# Заполнение пропусков

# Разобраться с использованными функциями
#install.packages("VIM")
library(VIM)
data(sleep, package = "VIM")
head(sleep)

# Результаты наблюдений за процессом сна у 62 млекопитающих разных видов
# Зависимые переменные: продолжительность сна со сновидениями (Dream),
# сна без сновидений (NonD) и их сумму (sleep)
# Таксономические переменные: масса тела (BodyWgt), вес мозга (BrainWgt),
# продолжительность жизни (Span) и время беременности (Gest).
# Экологические переменные: 5-бальные оценки степени хищничества животных (Pred), меры защищенности их места для сна (Exp): от глубокой норы до полно  стью открытого пространства, и показателя риска (Danger) на основе  логической комбинации Pred и Exp

# Список строк, в которых нет пропущенных значений
sleep[complete.cases(sleep), ]

# Список строк, в которых есть хотя бы одно пропущенное значение
sleep[!complete.cases(sleep), ]
sum(is.na(sleep$Dream))

#Дополнительная статистическая информация
library(mice)
# Нули в таблице соответствуют недостающим значениям: в первой строке пропусков нет,
# последующие упорядочены по числу их появления
# Первый столбец указывает число случаев в каждой строке исходных данных
# Последний столбец – число переменных с отсутствующими значениями в каждой строке
md.pattern(sleep)

# Числовые данные масштабируются к интервалу [0, 1] и представлены уровнями яркости
# более темными цветами показаны большие значения
# Недостающие значения представлены красным цветом
matrixplot(sleep)
aggr(sleep)


#Определение корреляции между пропусками
# Формируем матрицу со значениями 1 в местах пропусков
x <- as.data.frame (abs (is.na(sleep)))
y <- x[, which(colSums(x) > 0)]
print(cor(y),4)
cor(sleep, y, use = "pairwise.complete.obs")

#Заполнение пропущенных значений, Multivariate Imputation by Chained Equations
imp <- mice(sleep, seed = 1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)

# Заполнение пропусков и сохранение результата для использования
sleep_imp3 <- complete(imp, action = 3)
head(sleep_imp3)
sleep[!complete.cases(sleep_imp3), ]
save(sleep_imp3, file = "sleep_imp.Rdata")


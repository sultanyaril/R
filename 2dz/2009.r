# Измерение длительности вычислений:
t1 <- proc.time()
for (x in 1:1000000) y <- atan(x)
time.result <- proc.time() - t1
time.result["elapsed"]
names(time.result)

# Объединение нескольких команд в одну последовательность
{aver <- mean(1:10); stdev <- sd(1:10); c(MEAN=aver, SD=stdev)}

# Пользовательские функции 
# my_func.R

# Работа с временными рядами

# Данные по рождаемости в Нью-Йорке с января 1946 г. по декабрь 1959 г.
birth <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
head(birth)
# Создание объекта - временного ряда
birth.ts <- ts(birth, start = c(1946, 1), frequency = 12)
birth.ts
# Проверим, действительно ли это временной ряд
is.ts(birth.ts)
# Установка параметров для графиков
par(bg="wheat2")
#par(bg=rgb(0.3,0.1,0.5))
plot(birth.ts, xlab = "", ylab = "Рождаемость, тыс. чел.",
     main="Данные по рождаемости в Нью-Йорке с января 1946 г. по декабрь 1959 г.",
     col.main="deepskyblue", type="o", lwd = 2, cex=1, lty=3522, pch = 2, col="violet") 
#green rgb(0, 1, 0) deepskyblue coral violet
#type="o" type="h" type="s"
# Тип символа pch
# Размер символа cex
# Ширина линии lwd
# Тип линии lty: "штрих-пробел-штрих-пробел" (или одно из 6 предустановленных значений)

#Вывод имен 675 доступных цветов
colors()

par(bg="white")
Speed <- cars$speed
Distance <- cars$dist

# Прикрепление переменной к поисковому пути
attach(cars)
Speed <- speed
Distance <- dist

plot(Speed, Distance, panel.first = grid(8, 8),
     pch = 0, cex = 1.2, col = "blue")
plot(Speed, Distance,
     panel.first = lines(stats::lowess(Speed, Distance), lty = "dashed"),
     pch = 0, cex = 1.2, col = "blue")

#Демонстрация различных настроек для графиков
x <- 0:12
y <- sin(pi/5 * x)
op <- par(mfrow = c(3,3), mar = .1+ c(2,2,3,1))
for (tp in c("p","l","b",  "c","o","h",  "s","S","n")) {
  plot(y ~ x, type = tp, main = paste0("plot(*, type = \"", tp, "\")"))
  if(tp == "S") {
    lines(x, y, type = "s", col = "red", lty = 2)
    mtext("lines(*, type = \"s\", ...)", col = "red", cex = 0.8)
  }
}
par(op)

#Логарифмические графики и пользовательские оси
lx <- seq(1, 5, length = 41)
yl <- expression(e^{-frac(1,2) * {log[10](x)}^2})
y <- exp(-.5*lx^2)
op <- par(mfrow = c(2,1), mar = par("mar")-c(1,0,2,0), mgp = c(2, .7, 0))
plot(10^lx, y, log = "xy", type = "l", col = "purple",
     main = "Log-Log plot", ylab = yl, xlab = "x")
plot(10^lx, y, log = "xy", type = "o", pch = ".", col = "forestgreen",
     main = "Log-Log plot with custom axes", ylab = yl, xlab = "x",
     axes = FALSE, frame.plot = TRUE)
my.at <- 10^(1:5)
axis(1, at = my.at, labels = formatC(my.at, format = "fg"))
e.y <- -5:-1 ; at.y <- 10^e.y
axis(2, at = at.y, col.axis = "red", las = 1,
     labels = as.expression(lapply(e.y, function(E) bquote(10^.(E)))))
par(op)

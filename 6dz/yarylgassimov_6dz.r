data("morley")
?morley
head(morley)
morley
attach(morley)
# будем работать по expt #1, 2 и #3, 4
morley[Expt==2,]$Expt = 1  # перегруппируем 
morley[Expt==3,]$Expt = 2
morley[Expt==4,]$Expt = 2
morley = morley[Expt != 5,]
attach(morley)
# чтобы применить тест Стьюеднта
# 1. не должны быть коррелирующими
first = morley[Expt == 1,]$Speed
second = morley[Expt == 2,]$Speed
cor.test(first, second)
# p-value > 0.05 -> не коррелируют
# 2. соответствуют нормальному распределению
shapiro.test(first)
shapiro.test(second)
# p-value > 0.05 -> нормальные
var.test(first, second)
# p-value > 0.05 -> не совпадают дисперсии
t.test(Speed ~ Expt, var.equal=F)
# p-value < 0.05 -> средние не равны
t.test(first, second, alternative = 'less')
# p-value > 0.05 -> разница в средних меньше 0
power.t.test(n = length(first), delta = mean(first) - mean(second), 
             sd = mean(sd(first), sd(second)), sig.level = 0.05)
# power == 0.7, что недостаточно много
# при каком объеме можно увеличить до 0.9?
power.t.test(delta = mean(first) - mean(second), 
             sd = mean(sd(first), sd(second)), sig.level = 0.05, power = 0.9)
# n = 69


data("randu")
?randu
attach(randu)
# будем работать по x и y
shapiro.test(x)
shapiro.test(y)
# p-value < 0.05 -> распределение не нормальное
cor.test(x, y)
# p-value > 0.05 -> корреляции нет
wilcox.test(x, y)
# p-value > 0.05 -> средние совпадают

library(car)
var.test(first, second) # Фишер
# p-value 0.15
leveneTest(first, second) # Левене
# p-value 0.15
bartlett.test(morley$Speed ~ morley$Expt, data=morley) # Бартлетт
# p-value 0.15
fligner.test(morley$Speed ~ morley$Expt) # Флигнер
# p-value 0.10



gds  # from last hw
gds=sort(gds)
hist(gds, freq=F)
lines(density(gds))

shapiro.test(gds)$p.value
# p-value < 0.05 -> не нормальное распределение
mean(gds)
# 0
wilcox.test(gds)
# p-value > 0.05 -> среднее близко к 0
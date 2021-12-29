match_results <- read.csv(file.choose())
match_results
data("women")
?women
attach(women)
# проверим нормальность
shapiro.test(height)
shapiro.test(weight)
# данные нормальны
# вычисляем коэф. корреляции Пирсона
cor.test(height, weight)
# cor = 1, значит есть прямая зависимость между величинами
cor.test(height, weight, method = "spearman", alternative="less")
# тест Спирмана с проверкой обратной зависимости выдает результат p-value = 1
# т.е. обратной зависимости точно нет
cor.test(height, weight, method = "kendall", alternative = "greater")
# тест Кендалла с гипотезой о том, что коэф. кор > 0
# p-value < 0.05 -> коэф. корреляции больше 1

data("mtcars")
mtcars
t <- table(mtcars$gear, mtcars$cyl)
t
chisq.test(t)




attach(match_results)
team_names <- c('Man United', 'West Ham')
home_mu = match_results[HomeTeam == 'Man United' & AwayTeam == 'West Ham', 
                        c("HomeTeam","AwayTeam","FTR")]
home_whu = match_results[HomeTeam == 'West Ham' & AwayTeam == 'Man United',
                           c('HomeTeam', 'AwayTeam', 'FTR')]
mu_res = as.vector(table(home_mu$FTR))
table(home_mu$FTR)
whu_res = as.vector(table(home_whu$FTR))
table(home_whu$FTR)
mu_res
whu_res
results.data <- matrix(c(mu_res, whu_res), nrow = 3, byrow=F,
                       dimnames = list(c('H', 'D', 'A'), team_names))
results.data

chisq <- chisq.test(results.data)
# p<0.05
chisq
contrib <- 100*chisq$residuals^2/chisq$statistic
library(corrplot)
corrplot(contrib, is.cor = F)

fisher.test(results.data)
# p < 0.05

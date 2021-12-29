match_results <- read.csv(file.choose())
attach(match_results)
num_cols = colnames(match_results)
non_num_cols = c(1:4, 7, 10, 11)
num_cols <- num_cols[-non_num_cols]  # non-numerical cols
numerical_results <- match_results[num_cols]  # get numerical cols
numerical_results <- na.omit(numerical_results)  # delete nans
M <- cor(numerical_results)
library(corrplot)
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow",
                           "#7FFF7F", "cyan", "#007FFF", "blue","#00007F")) 
corrplot(M, method = "pie", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black", addCoef.col = "red")

corrplot(M, method = "color", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black", addCoef.col = "red")
vif(lm(FTHG - FTAG ~ HTHG + HTAG + HS + AS + HST + AST + HC + AC + HF + AF + HY + 
         AY + HR + AR,
       data = match_results))

#
num_cols
numerical_results = numerical_results[num_cols[-c(8, 9)]]
# выкинем пару столбцов т.к. много слишком много столбцов
pairs(numerical_results, panel = panel.smooth) #со сглаживающей кривой

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

pairs(numerical_results, panel = panel.smooth, lower.panel = panel.cor)
library(lattice)
splom(cars)

library(GGally)
ggpairs(numerical_results, 
        upper = list(continuous = "density", combo = "box"), 
        lower = list(continuous = "points", combo = "dot"))


non_num_results <- match_results[non_num_cols]
non_num_results <- na.omit(non_num_results)
non_num_results
ftr <- non_num_results["FTR"]
htr <- non_num_results["HTR"]

ftr[ftr == 'H'] = 1
ftr[ftr == 'D'] = 0
ftr[ftr == 'A'] = -1

htr[htr == 'H'] = 1
htr[htr == 'D'] = 0
htr[htr == 'A'] = -1

coplot(ftr ~ htr)
coplot(ftr ~ htr,
       ylab = c("Y"),
       xlab = c("X",
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) }))

club = "Chelsea"
season = "2019-20"
Time = 38
club_matches = match_results[Season == season & (HomeTeam==club | AwayTeam==club),]
club_matches
gds <- c()
for (row in 1:nrow(club_matches)) {
  x = match_results[row, ]
  if (x$HomeTeam == club) {
    gds <- c(gds, x$FTHG - x$FTAG)
  } else {
    gds <- c(gds, x$FTAG - x$FTHG)
  }
}
gds

layout(matrix(1:2, ncol = 2))
plot(1:Time, gds, type = "l", main="Бонапартов песочник", 
     xlab = "Время (2 недели)", ylab = "Численность")
acf(gds, main = "АКФ", xlab = "Лаг", ylab = "Значение")




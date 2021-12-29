?RNGkind
graph_distr <- function(x, pc, pd, main_name = "")
{ 
  op <- par(mfrow = c(1, 1), pty = "s")
  par(mfrow = c(1, 2))
  mn <- paste(c("Эмпирическая ФР и ", main_name))
  plot(x,pc, type = "l", col = "red", lwd = 2, main = mn) 
  plot(ecdf(x), add = TRUE) 
  mn <- paste(c("Эмпирическая плотность и ", main_name))
  plot(density(x), lwd = 2, col = "blue", main = mn) 
  lines(x, pd, col = "red", lwd = 2)
  par(op)
}

library(car)
for (i in c(3, 9, 27)) {
  for (j in c(5, 10, 15)) {
    small = sort(rnorm(50, mean=i, sd=j))
    common_name=paste(50, i, j, sep='_')
    jpeg(paste("pics/small", common_name, 1,sep='_'))
    graph_distr(small, pnorm(small, mean = i, sd = j),
                dnorm(small, mean = i, sd = j),
                paste("нормальное распределение с параметрами", 50, i, j))
    dev.off()
    
    jpeg(paste("pics/small",common_name,2,sep='_'))
    #text(0.5,0.5,paste("Нормальное распределение с параметрами", 50, i, j),cex=2,font=2)
    par(mfrow = c(1,1))
    qqnorm(small, main = paste("Нормальное распределение с параметрами", 50, i, j))
    qqline(small)
    dev.off()
    
    jpeg(paste("pics/small",common_name,3,sep='_'))
    qqPlot(small, dist = "norm", col = palette()[1], pch = 19,
           xlab="Квантили нормального распределения", 
           ylab="Наблюдаемые квантили", 
           main=paste("Нормальное распределение с параметрами", 50, i, j))
    dev.off()
  }
}

for (i in c(10, 100, 1000)) {
  for (j in c(10, 100, 1000)) {
    common_name=paste(2000, i, j, sep='_')
    jpeg(paste("pics/big",common_name,1,sep='_'))
    big = sort(rnorm(2000, mean=i, sd=j))
    graph_distr(big, pnorm(big, mean = i, sd = j),
                dnorm(big, mean = i, sd = j),
                paste("нормальное распределение с параметрами", 2000, i, j))
    #text(0.5,0.5,paste("Нормальное распределение с параметрами", 2000, i, j),cex=2,font=2)
    dev.off()
    jpeg(paste("pics/big",common_name,2,sep='_'))
    par(mfrow = c(1,1))
    qqnorm(big)
    qqline(big)
    dev.off()
    
    jpeg(paste("pics/big",common_name,3,sep='_'))
    qqPlot(big, dist = "norm", col = palette()[1], pch = 19,
           xlab="Квантили нормального распределения", 
           ylab="Наблюдаемые квантили", 
           main=paste("Нормальное распределение с параметрами", 2000, i, j))
    dev.off()
  }
}

library(nortest)
for (k in c (50, 2000)) {
  for (i in c(3, 9, 27)) {
    for (j in c(5, 10, 15)) {
      x = rnorm(k, mean=i, sd=j)
      print(paste("Size:",k,"Mean",i,"Sd",j))
      print(paste("Shapiro", shapiro.test(x)$p.value))
      print(paste("Ad",ad.test(x)$p.value))
      print(paste("Cvm",cvm.test(x)$p.value))
      print(paste("Lillie",lillie.test(x)$p.value))
      print(paste("Sf",sf.test(x)$p.value))
      print("")
    }
  }
}

match_results = read.csv(file.choose())
gds_abs = abs(match_results$FTAG-match_results$FTHG)
hist(gds_abs, breaks=4)

gds  # from last hw
gds=sort(gds)
hist(gds, freq=F)
lines(density(gds))

shapiro.test(gds)$p.value  # very little p.value\
graph_distr(gds, pnorm(gds, mean = mean(gds), sd = sd(gds)),
            dnorm(gds, mean = mean(gds), sd = sd(gds)),
            "нормальное распределение")

par(mfrow = c(1,1))
qqnorm(gds)
qqline(gds)

qqPlot(gds, dist = "norm", col = palette()[1], pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main=paste("Нормальное распределение"))




x = sort(rgamma(1000, 2, (5 + 2*rpois(1000, 100))))
hist(x, freq=F)
lines(density(x))
pars = fitdistr(x, "normal")
ks.test(x, pnorm, mean=pars$estimate[1], sd=pars$estimate[2])
m = pars$estimate[1]
s = pars$estimate[2]
graph_distr(x, pnorm(x, mean = m, sd = s),
            dnorm(x, mean = m, sd = s),
            "нормальное распределение")

par(mfrow = c(1,1))
qqnorm(x)
qqline(x)

qqPlot(x, dist = "norm", col = palette()[1], pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main=paste("Нормальное распределение"))

shapiro.test(x)



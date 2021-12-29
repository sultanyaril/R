match_results = read.csv(file.choose())
attach(match_results)
big_six=c('Arsenal','Chelsea','Liverpool','Man City','Man United','Tottenham')

layout(matrix(1:1, ncol=1, nrow=1)) 
home_gd=FTHG[HomeTeam=='Man United'] - FTAG[HomeTeam=='Man United']
away_gd=FTAG[AwayTeam=='Man United'] - FTHG[AwayTeam=='Man United']
total_gd <- c(home_gd, away_gd)
hist(total_gd,
     freq=F,
     xlab = "Разница мячей",
     ylab = "Частота",
     main = paste("Man United", "'s GD",sep=''),
     col="lightblue"
)  # гистограмма разницы мячей МЮ, на нее будут ложиться все кривые
density(total_gd)
cols = c("red","blue","violet","skyblue","tomato", "grey")
# цвета выбранные для кривых
col_num = 1  # счетчик цветом для кривых
for (i in big_six) {
  home_gd=FTHG[HomeTeam==i] - FTAG[HomeTeam==i]  # домашние результаты
  away_gd=FTAG[AwayTeam==i] - FTHG[AwayTeam==i]  # результаты на выезде
  total_gd <- c(home_gd, away_gd)  # общие результаты
#  hist(total_gd,
#        freq=F,
#        xlab = "Разница мячей",
#        ylab = "Частота",
#        main = paste(i, "'s GD",sep=''),
#        col="lightblue"
#  )
  lines(density(total_gd, wd=0.45), col = cols[col_num], lwd = 2) 
  col_num = col_num + 1
}
legend("topright",big_six, fill=cols, cex=0.75)

layout(matrix(1:6, ncol=3, nrow=2)) 
for (i in big_six) {
  home_gd=FTHG[HomeTeam==i] - FTAG[HomeTeam==i]  # домашние результаты
  away_gd=FTAG[AwayTeam==i] - FTHG[AwayTeam==i]  # результаты на выезде
  game_results = FTR[HomeTeam==i]
  total_gd <- c(home_gd)  # общие результаты
  cdplot(FTR[HomeTeam==i]~FTAG[HomeTeam==i], col=c('red','yellow','green'),
         xlab="Goals Conceded",
         ylab="Game Result",
         main=paste(i,"'s at home when conceding",sep=''))
  legend("topleft",legend=c("win","draw","lose"),fill=c("green","yellow","red"),ncol=3,cex=0.4)
}

layout(matrix(1, ncol=1, nrow=1))
totals <- list()
for (i in big_six) {
  home_gd=FTHG[HomeTeam==i] - FTAG[HomeTeam==i]  # домашние результаты
  away_gd=FTAG[AwayTeam==i] - FTHG[AwayTeam==i]  # результаты на выезде
  total_gd <- c(home_gd, away_gd)
  totals[i]<-list(total_gd) # общие результаты
}
totals

cols = c("red","blue","violet","skyblue","tomato", "grey")
big_six_abbreviations=c("ARS", "CHE", "LIV","MCI","MUN","TOT")
boxplot(totals,
        ylim=c(-7,7),
        horizontal=T,
        names=big_six_abbreviations,
        xlab="Goals Difference",
        main=paste("Big six's goal difference",sep=''),
        col=cols,
        outline=F
)
axis(side=1,at=seq(-6,6))
legend("topright",legend=big_six_abbreviations,fill=cols,cex=0.75)


teams_to_compare=c("Man United", "Southampton")
totals <- list()
for (i in teams_to_compare) {
  home_gd=FTHG[HomeTeam==i] - FTAG[HomeTeam==i]  # домашние результаты
  away_gd=FTAG[AwayTeam==i] - FTHG[AwayTeam==i]  # результаты на выезде
  total_gd <- c(home_gd, away_gd)
  totals[i]<-list(total_gd) # общие результаты
}
totals

cols = c("red","blue")
boxplot(totals,
        ylim=c(-7,7),
        horizontal=T,
        names=teams_to_compare,
        xlab="Goals Difference",
        main=paste("Two team's goal difference",sep=''),
        col=cols,
        outline=F
)
axis(side=1,at=seq(-6,6))
legend("topright",legend=teams_to_compare,fill=cols,cex=0.75)

draw_pie_for <- function(team_name) {
  home_results=table(FTR[HomeTeam==team_name])
  away_results=table(FTR[AwayTeam==team_name])
  wins = home_results[names(home_results)=='H'] + away_results[names(away_results)=='A']
  draws = home_results[names(home_results)=='D'] + away_results[names(away_results)=='D']
  loses = home_results[names(home_results)=='A'] + away_results[names(away_results)=='H']
  pie(c(wins,draws,loses), 
      labels=c("", "", ""),
      col=c("green", "yellow","red"),
      main=paste(team_name,"'s all-time results",sep=""),
      radius=1)
  legend("topright",fill=c("green","yellow","red"),legend=c("win","draw","lose"))
}
draw_pie_for("Man United")

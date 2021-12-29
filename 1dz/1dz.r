mat <- matrix(seq(1, 16), nrow = 4, ncol = 4, byrow=TRUE)
colnames(mat) <- seq(10, 40, 10)
rownames(mat) <- c("A", "B", "C", "D")
mat
t(mat)

a <- c(1, 2, 3, 4)
b <- c("a", "b", "c", "d", "e")
c <- c("word", "letter", "text")
new_mat <- rbind(a, b, c)
new_mat
cbind(a, b, c)

class(new_mat[2, 2])
new_mat[2, 2] = 3
new_mat
if (class(new_mat[2, 2]) == "character") {
  "CHR"
} else if (class(new_mat[2, 2] == "integer")){
  "INT"
} else if (class(new_mat[2, 2] == "numeric")) {
  "NUM"
} else if (class(new_mat[2, 2]) == "logical") {
  "LOG"
} else if (new_mat[2, 2] == "NaN") {
  "Not-a-Number"
} else if (new_mat[2, 2] == "NA") {
  "Not applicable"
}


match_results <- read.csv(file=file.choose())
match_results <- read.table(file.choose(), header = TRUE, sep = ",", quote = "\"", dec = ".", 
                            fill = TRUE, comment.char = "")
str(match_results)
match_results[match_results$AwayTeam == "Man United", 1 : 23]
"LAST 10 MAN UNITED RESULTS:"
tail(match_results[match_results$AwayTeam == "Man United", 1 : 23], n = 10)
clubs <- c(levels(match_results$HomeTeam))
"CLUBS PLAYED IN EPL"
clubs

#max_goals <- max(match_results$FTHG)
#h <- hist(match_results$FTHG,
#     right=F,
#     breaks=max_goals,
#     main="Goals scored by Home Team", 
#     freq=F,
#     ylim=c(0, 0.5),
#     xlab="Number of goals")
#text(h$mids, h$counts,labels=h$counts, adj=c(0.5, -0.5))

barplot(table(match_results$FTHG)/nrow(match_results),
        main = "Number of Goals scored by home team",
        xlab = "Goals scored",
        density = 20,
        border="red",
        col="blue",
        ylim=c(0, 0.4))

plot(table(match_results$FTHG + match_results$FTAG)/nrow(match_results),
     ylim=c(0, 0.3),
     main = 'Total number of goals scored in one match',
     ylab = 'Goals scored',
     xlab = 'Density',
     type = 'l')

goals_table = data.frame(Season = unique(match_results$Season), Goals_scored = 0)
for(i in seq(1, nrow(match_results), 1)) {
  row <- match_results[i, ]
  factor(row$Season)
  row$Season
  goals_table$Goals_scored[goals_table$Season == row$Season]
  if (row$HomeTeam == 'Man United') {
    goals_table$Goals_scored[goals_table$Season == row$Season] <- goals_table$Goals_scored[goals_table$Season == row$Season] + row$FTHG
  } else if (row$AwayTeam == 'Man United') {
    goals_table$Goals_scored[goals_table$Season == row$Season] <- goals_table$Goals_scored[goals_table$Season == row$Season] + row$FTAG
  }
}
"Goals scored by Man United in different seasons"
goals_table        

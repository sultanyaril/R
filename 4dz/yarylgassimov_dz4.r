match_results = read.csv(file.choose())
attach(match_results)
match_results

get_goals_scored <- function(team_name, season) {
  get_goals_scored <- sum(subset(match_results, select=c(FTAG), 
                             subset=(AwayTeam==team_name & Season==season))) + 
                      sum(subset(match_results, select=c(FTHG), 
                             subset=(HomeTeam==team_name & Season==season)))
}

get_goals_conceded <- function(team_name, season) {
  get_goals_conceded <- sum(subset(match_results, select=c(FTHG), 
                                  subset=(AwayTeam==team_name & Season==season))) + 
                        sum(subset(match_results, select=c(FTAG), 
                                  subset=(HomeTeam==team_name & Season==season)))
}

get_points <- function(team_name, season) {
  get_points <- 3 * length(unlist(subset(match_results, select=c(FTR), 
                                  subset=(HomeTeam==team_name & FTR=='H' & Season==season)))) +
                3 * length(unlist(subset(match_results, select=c(FTR), 
                                  subset=(AwayTeam==team_name & FTR=='A' & Season==season)))) +
                1 * length(unlist(subset(match_results, select=c(FTR), 
                                  subset=((HomeTeam==team_name | AwayTeam==team_name) & FTR=='D' & Season==season))))
}

get_league_table <- function(season) {
  teams = unique(subset(match_results, select=c(AwayTeam), subset=(Season==season)))$AwayTeam
  league_table <- data.frame(
    team_name = teams,
    goals_scored = unlist(lapply(teams, season=season, get_goals_scored)),
    goals_conceded = unlist(lapply(teams, season=season, get_goals_conceded)),
    points = unlist(lapply(teams, season=season, get_points))
  )
  league_table <- league_table[order(league_table$points,
                                     league_table$goals_scored - league_table$goals_conceded,
                                     decreasing = T), ]
  # sort by points, then by goal difference (that's the rule)
  rownames(league_table) <- 1:nrow(league_table)
  get_league_table <- league_table
}

league_table <- get_league_table("2011-12")
league_table$color[1:4] <- 'blue'
league_table$color[5:6] <- 'orange'
league_table$color[7:17] <- 'black'
league_table$color[18:20] <- 'red'

dotchart(rev(league_table$points),
         labels=rev(league_table$team_name),
         main="2011-12 EPL table",
         xlab="Points",
         ylab="Team",
         color=rev(league_table$color)
         )  # revs for correct graph (top to bottom)
legend("bottomright",legend=c("CL", "EL", "Relegation"), fill=c("blue","orange","red"))

# let's see how particular team scores goals per season
seasons = unique(Season)
team_name='Leeds'
total_goals = sapply(seasons, team_name=team_name, get_goals_scored)
total_goals[total_goals==0] = NA  # team has skipped many seasons
total_goals  # here we see importance of NA
boxplot(total_goals, horizontal = T)
stripchart(total_goals, add=T)

# let's look at GD
home_gd=FTHG[HomeTeam==team_name & Season=='2003-04'] - 
        FTAG[HomeTeam==team_name & Season=='2003-04']  # home gds
away_gd=FTAG[AwayTeam==team_name & Season=='2003-04'] - 
        FTHG[AwayTeam==team_name & Season=='2003-04']  # away gds
total_gd <- c(home_gd, away_gd)  # all gds

boxplot(total_gd, 
        horizontal=T,
        outline=F,
        main="Leeds GD in 2003-04")
stripchart(total_gd,
           vertical=F,
           method='jitter',
           pch=21,
           jitter=0.1,
           add=T)

# let's get float values and see ratio of goals scored over goals conceded by team

team_name = 'Man United'
seasons = unique(Season)
scored_goals = sapply(seasons, team_name=team_name, get_goals_scored)
conceded_goals = sapply(seasons, team_name=team_name, get_goals_conceded)
ratio = scored_goals / conceded_goals
ratio
boxplot(ratio, 
        horizontal=T,
        outline=F,
        main="Man United all-time goal ratio",
        xlab="Scored/Conceded ration")
stripchart(ratio,
           vertical=F,
           method='jitter',
           pch=21,
           jitter=0.1,
           add=T)

# let's check grubbs for some data
seasons = unique(Season)
gds = c()
for (i in seasons) {
  teams = unique(match_results$AwayTeam[Season==i])
  gds <- c(gds,unlist(lapply(teams, season=i, get_goals_scored)) -
                   unlist(lapply(teams, season=i, get_goals_conceded)))
}
gds
density(gds)
hist(gds, freq=F,
     main="Goal differences",
     xlab="Goal differences")
lines(density(gds))
library(outliers)
stripchart(gds,
           method='jitter',
           pch=21,
           jitter=0.1)
grubbs.test(gds, opposite=T)  # check if outlier on the opposite tail (min)
grubbs.test(gds)  # check if max is outlier
grubbs.test(gds, type=11)  # check if outlier is on the both sides

gds <- c(gds, 95)  # add 95 as an outlier
grubbs.test(gds)
gds <- gds[gds!=95]

gds <-c(gds, -100)  # add -100 as an outlier
grubbs.test(gds)

gds <- c(gds, 95)
grubbs.test(gds, type=11)  # outliers on both sides
stripchart(gds,
           method='jitter',
           pch=21,
           jitter=0.1)
gds = gds[gds!=95]  # delete outliers
gds = gds[gds!=-100]
sort(gds)
# dixon test doesnt work with >= 30 samples
short_gds <- c(min(gds), max(gds), sample(gds, 28))
boxplot(short_gds, notch=T, horizontal = T)
dixon.test(short_gds)  # check max
dixon.test(short_gds, type=11)  # check 1 elements from both sides
dixon.test(short_gds, opposite = T)  # check min

# increase max from 79 to 95
dixon.test(c(short_gds[-1], 95))
# decrease min from -69 to -100
dixon.test(c(short_gds[-2], -100))

library(mice)
md.pattern(match_results)
imputed_Data <- mice(match_results, m=5, method = 'pmm')
summary(imputed_Data)
completeData <- complete(imputed_Data)
completeData
transcan(x=match_results)
attach(match_results)
attach(mu_liverpool)
mu_liverpool = rbind(match_results[(match_results$HomeTeam=='Man United' & match_results$AwayTeam=='Liverpool'),], 
              match_results[(match_results$AwayTeam=='Liverpool' & match_results$HomeTeam=='Man United'),])
mu_liverpool
actual=c()
space_positions=sample(1:nrow(mu_liverpool), 5)
for (i in space_positions) {
  actual<-append(actual, mu_liverpool$FTHG[i])
  mu_liverpool$FTHG[i] = NA
}
impute_arg <- aregImpute(formula=~FTHG + FTAG, n.impute=5, data=mu_liverpool, nk=0)
mu_liverpool
impute_arg
impute_arg$imputed$FTHG[,1]
actual


# first 5 tours of season
first_tour = match_results[match_results$Season=='2018-19',][0:100,]
set = subset(first_tour, select=c('FTAG','FTHG','HTAG','HTHG','FTR'))
str(match_results)
set
library(pan)
actual = c()
space_positions=sample(1:nrow(set), 5)
for (i in space_positions) {
  actual<-c(actual, set[i,]$FTAG, set[i,]$FTHG)
  set[i,]$FTAG = NA
  set[i,]$FTHG = NA
}


# Lilliefors (Kolmogorov-Smirnov) test for normality
library(nortest)
gds  # back to gds
gds
lillie.test(gds)
# hope you are not looking into my code
# because i figured out that all data i was using
# is not normally distributed :(((

library(EnvStats)
rosnerTest(gds, k=3)
rosnerTest(c(gds, 100), k=2)

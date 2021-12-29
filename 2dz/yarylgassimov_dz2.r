# additional func to know month from date in dataset
get_month <- function(date) {
  new_date = as.Date(date, '%Y-%m-%dT%H:%M:%SZ')
  get_month = as.numeric(format(new_date, "%m")) 
  # get_month = as.integer(substr(as.character(date), 6, 7))
}

multiplot <- function(data, team_name, season) {
  par(mfcol=c(2,2))
  # plot of cumulative points
  plot(
    cumsum(data$points) ~ data$month,
    type='l',
    main=paste(team_name, "'s points in ", season, sep=''),
    xlab="Months",
    ylab="Points",
    lty = 2,
    lwd = 3,
    font = 3,
    font.lab = 2,
    col = "blue",
    col.axis = 'chocolate',
    las = 2
  )
  # barplot of points per month
  barplot(
    t(as.matrix(data$points)),
    names.arg=format(data$month, "%b '%y"),
    xlab="Months",
    ylab="Points",
    col = "blue",
    font = 3,
    font.lab = 2,
    col.axis = 'chocolate',
    las = 2,
    density = 10,
    border = 'red'
  )
  #barplot of average points in the month
  barplot (
    t(as.matrix(data$points/data$games)),
    names.arg=format(data$month, "%b '%y"),
    xlab="Months",
    ylab="Points per game",
    col = "blue",
    font = 3,
    font.lab = 2,
    col.axis = 'chocolate',
    las = 2,
    density=10,
    border='red'
  )
  # barplot made by plot
  plot (
    data$games ~ data$month,
    type='h',
    lwd=10,
    xlab="Months",
    ylab="Matches",
    col = "blue",
    font = 3,
    font.lab = 2,
    col.axis = 'chocolate',
    las = 2,
  )
}

plot_stats <- function(team_name, season) {
  match_results <- read.csv(file="/media/sultanyaril/989E6E129E6DE968/Users/Султан/Desktop/Third year/Практикум на ЭВМ/dataset/results.csv")
  # str(match_results)
  points.ts <- ts(rep(0, times = 12), start = c(as.numeric(substr(season, 1, 4)), 8), frequency=12)
  points_accumulation=data.frame(
    month = seq(as.Date(paste(substr(season,1,4),"-8-1",sep='')), by="month", length.out=12),
    points=0,
    games=0
  )
  # frame with Months from August to July and points, that we'll acumulate
  month_count = 1
  # this on we need for points.ts because season start in August and it's indexed 1, not 8
  current_month = 0
  # initially its 0 but it's only needed to know when new month starts
  total_points = 0
  # accumulator
  total_games = 0
  for(i in seq(1, nrow(match_results), 1)) {
    row <- match_results[i, ]
    if (season != row$Season) {
      next;
    }
    if (row$HomeTeam == team_name) {
      # first we check if month ended
      # if true then update ts and frame
      if (current_month != get_month(row$DateTime)) {
        points.ts[month_count] = total_points
        points_accumulation$points[get_month(points_accumulation$month)==current_month] = total_points
        points_accumulation$games[get_month(points_accumulation$month)==current_month] = total_games
        # decrements by 1 month to correctly assign
        total_points = 0
        total_games = 0
        month_count = month_count + 1
        current_month = get_month(row$DateTime)
      }
      total_games = total_games + 1
      if (row$FTHG > row$FTAG)
          total_points = total_points + 3 # win
      else if (row$FTHG == row$FTAG)
          total_points = total_points + 1 # draw
    } else if (row$AwayTeam == team_name) {
      if (current_month != get_month(row$DateTime)) {
        points.ts[month_count] = total_points
        points_accumulation$points[get_month(points_accumulation$month)==current_month] = total_points
        points_accumulation$games[get_month(points_accumulation$month)==current_month] = total_games
        total_points = 0
        total_games = 0
        month_count = month_count + 1
        current_month = get_month(row$DateTime)
      }
      total_games = total_games + 1
      if (row$FTHG < row$FTAG)
        total_points = total_points + 3
      else if (row$FTHG == row$FTAG)
        total_points = total_points + 1
    }
  }
  # record last month
  points.ts[month_count] = total_points
  points_accumulation$points[get_month(points_accumulation$month)==current_month] = total_points
  points_accumulation$games[get_month(points_accumulation$month)==current_month] = total_games
  print(team_name)
  print(points.ts)
  multiplot(points_accumulation, team_name, season)
  plot_stats = sum(points_accumulation$points)
}

dataset = read.csv(file.choose())
str(dataset)

plot_stats('Arsenal', '2003-04')

teams = levels(dataset$HomeTeam)
results = sapply(teams, season='2011-12', plot_stats)
tournament_table = sort(results[results != 0], T)
tournament_table

seasons = levels(dataset$Season)
chelsea_results = sapply(seasons, team_name='Chelsea', plot_stats)
chelsea_results

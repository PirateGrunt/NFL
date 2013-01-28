library(TTR)

LimitScores = function(Scores, Limit)
{
  Limit = abs(Limit)
  Scores = ifelse(Scores > Limit, Limit, Scores)
  Scores = ifelse(Scores < -Limit, -Limit, Scores)
  
  return (Scores)
}

ExponentialSmoothedAverage = function(x, N = 16, Smoother = 0.9)
{
  number.of.rows = length(x)
  if (N > number.of.rows)
  {
    warning("N exceeds the number of rows")
  }
  ExpSmooth = rep(0, number.of.rows)
  ExpSmooth[1:N-1] = NA
  for (i in 1:N)
  {
    ExpSmooth[N:number.of.rows] = (Smoother^i) * x[(N-i+1):(number.of.rows-i+1)] + ExpSmooth[N:number.of.rows]
  }
  
  ExpSmooth[N:number.of.rows] = ExpSmooth[N:number.of.rows] / N
  
  return(ExpSmooth)
}

GetLimitedScores = function(dfTeamData, ColumnToAdjust, Limit)
{
  dfTeamData$ReplaceMe = LimitScores(dfTeamData[, ColumnToAdjust], Limit)
  
  names(dfTeamData) = gsub("^ReplaceMe$", paste("Limited", ColumnToAdjust, sep=""), names(dfTeamData))
  
  return(dfTeamData)
}

CalculateExponentialSmoothedAverage = function(dfTeamData, ColumnToAdjust, NumberOfGames = 16, Smoother = 0.9)
{
  EMA = ExponentialSmoothedAverage(dfTeamData[,ColumnToAdjust], NumberOfGames, Smoother)
  dfTeamData$ReplaceMe = EMA
  names(dfTeamData) = gsub("^ReplaceMe$", paste(ColumnToAdjust, "EMAPost", sep=""), names(dfTeamData))
  
  rowIndices = 1:length(EMA)
  dfTeamData$ReplaceMe = rowIndices
  dfTeamData$ReplaceMe = NA
  dfTeamData$ReplaceMe[2:max(rowIndices)] = EMA[1:max(rowIndices)-1]
  names(dfTeamData) = gsub("^ReplaceMe$", paste(ColumnToAdjust, "EMAPre", sep=""), names(dfTeamData))
  
  return (dfTeamData)
}

CalculateSimpleMovingAverage = function(dfTeamData, ColumnToAdjust, NumberOfGames = 16)
{
  require(TTR)
  SMA = SMA(dfTeamData[, ColumnToAdjust], NumberOfGames)
  dfTeamData$ReplaceMe = SMA
  names(dfTeamData) = gsub("^ReplaceMe$", paste(ColumnToAdjust, "SMAPost", sep=""), names(dfTeamData))
                           
  rowIndices = 1:length(SMA)
  dfTeamData$ReplaceMe = rowIndices
  dfTeamData$ReplaceMe = NA
  dfTeamData$ReplaceMe[2:max(rowIndices)] = SMA[1:max(rowIndices)-1]
  names(dfTeamData) = gsub("^ReplaceMe$", paste(ColumnToAdjust, "SMAPre", sep=""), names(dfTeamData))
  
  return (dfTeamData)
}

AdjustAColumn = function(dfTeams, ColumnToAdjust, NumberOfGames = 10, Smoother = 0.5, ScoreLimit = 999)
{
  TeamsList = split(dfTeams, dfTeams$ThisTeam)
  TeamsList = lapply(TeamsList, GetLimitedScores, "ScoreDifference", ScoreLimit)
  TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ScoreDifference", NumberOfGames)
  TeamsList = lapply(TeamsList, CalculateExponentialSmoothedAverage, "ScoreDifference", NumberOfGames, Smoother)
  TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "LimitedScoreDifference", NumberOfGames)
  TeamsList = lapply(TeamsList, CalculateExponentialSmoothedAverage, "LimitedScoreDifference", NumberOfGames, Smoother)
  dfTeamData = do.call(rbind, TeamsList)
  
  return(dfTeamData)
}

AdjustAllTeamsData = function(dfTeams, NumberOfGames = 10, Smoother = 0.5, ScoreLimit = 999)
{
  TeamsList = split(dfTeams, dfTeams$ThisTeam)
  TeamsList = lapply(TeamsList, GetLimitedScores, "ScoreDifference", ScoreLimit)
  TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ScoreDifference", NumberOfGames)
  TeamsList = lapply(TeamsList, CalculateExponentialSmoothedAverage, "ScoreDifference", NumberOfGames, Smoother)
  TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "LimitedScoreDifference", NumberOfGames)
  TeamsList = lapply(TeamsList, CalculateExponentialSmoothedAverage, "LimitedScoreDifference", NumberOfGames, Smoother)
  dfTeamData = do.call(rbind, TeamsList)
  
  return(dfTeamData)
}


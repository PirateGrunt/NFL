GetPredictionRate = function(dfGamesData, dfTeamData, Predictor)
{
  
  #dfTeamData = GetTeamData(dfGamesData, NumberOfGames, Smoother)
  #============================
  # Pick up the list of winning teams
  MergedGamesData = merge(dfGamesData, dfTeamData, by.x=c("GameDate", "Winner"), by.y=c("GameDate", "ThisTeam"), all.x=T)
  retained.columns = c("GameDate", "Winner", "Loser", "WinnerPoints", "LoserPoints", Predictor)
  MergedGamesData = MergedGamesData[,retained.columns]
  colnames(MergedGamesData) = c("GameDate", "Winner", "Loser", "WinnerPoints", "LoserPoints", "Predictor.Winner")
  
  #============================
  # Pick up the list of losing teams
  MergedGamesData = merge(MergedGamesData, dfTeamData, by.x=c("GameDate", "Loser"), by.y=c("GameDate", "ThisTeam"), all.x=T)
  retained.columns = c("GameDate", "Winner", "Loser", "WinnerPoints", "LoserPoints", "Predictor.Winner", Predictor)
  MergedGamesData = MergedGamesData[,retained.columns]
  colnames(MergedGamesData) = c("GameDate", "Winner", "Loser", "WinnerPoints", "LoserPoints", "Predictor.Winner", "Predictor.Loser")
  
  rm(retained.columns)
  
  #===================================
  # Filter out the NAs
  MergedGamesData = subset(MergedGamesData, !is.na(Predictor.Winner))
  
  #==================================
  # Predict the winner
  MergedGamesData$PredictedWinner = ifelse(MergedGamesData$Predictor.Winner > MergedGamesData$Predictor.Loser, MergedGamesData$Winner, MergedGamesData$Loser)
  MergedGamesData$AccuratePrediction = ifelse(MergedGamesData$PredictedWinner == MergedGamesData$Winner, 1, 0)
  mojo = subset(MergedGamesData, !is.na(AccuratePrediction))
  
  PredictionRate = sum(mojo$AccuratePrediction) / dim(mojo)[1]
  
  return (PredictionRate)
}

#================================================================================================
# The script
dfGamesData = GetGamesHistory(1985:2012)
dfTeamData = GetAllTeamsData(dfGamesData)
dfTeamData = AdjustAllTeamsData(dfTeamData, 14, 1, 100)

write.csv(dfTeamData, paste(filepath, "December27.csv", sep=""))

dfThisSeason = GetGamesHistory(2011:2012)
dfThisSeasonData = GetAllTeamsData(dfThisSeason)
dfThisSeasonData = AdjustAllTeamsData(dfThisSeasonData, 14, 1, 100)
write.csv(dfThisSeasonData, paste(filepath, "December13.csv", sep=""))

print(GetPredictionRate(dfGamesData, dfTeamData, "LimitedScoreDifferenceSMAPre"))
print(GetPredictionRate(dfGamesData, dfTeamData, "ScoreDifferenceSMAPre"))

for (i in 10:16)
{
  #   for (limit in seq(1, 21, by=3))
  #   {
  #     dfTeamData = GetAllTeamsData(dfGamesData)
  #     dfTeamData = AdjustAllTeamsData(dfTeamData, i, .95, limit)
  #     print(paste("smoother = .95, i = ",i,"Limit =", limit,"Prediction rate = ", GetPredictionRate(dfGamesData, dfTeamData, "LimitedScoreDifferenceSMAPre")))
  #     dfTeamData = AdjustAllTeamsData(dfTeamData, i, 1, limit)
  #     print(paste("smoother = 1, i = ",i,"Limit =", limit,"Prediction rate = ", GetPredictionRate(dfGamesData, dfTeamData, "LimitedScoreDifferenceSMAPre")))
  #   }
  dfTeamData = GetAllTeamsData(dfGamesData)
  dfTeamData = AdjustAllTeamsData(dfTeamData, i, 1, 100)
  print(paste("i = ",i,"No limit", "Prediction rate = ", GetPredictionRate(dfGamesData, dfTeamData, "ScoreDifferenceSMAPre")))
  for( smooth in seq(80, 100, by=5))
  {
    dfTeamData = GetAllTeamsData(dfGamesData)
    dfTeamData = AdjustAllTeamsData(dfTeamData, i, smooth/100, 100)
    print(paste("i = ",i,"Smoother =", smooth/100,"Prediction rate = ", GetPredictionRate(dfGamesData, dfTeamData, "LimitedScoreDifferenceEMAPre")))
  }
}

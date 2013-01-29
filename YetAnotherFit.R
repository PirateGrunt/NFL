
#==============================================================================
# Get regular season games and compute moving averages
dfRegularSeason = dfGames[dfGames$Week %in% 1:18,]

dfRegularSeason$ThisTeamTotalYardsAllowed = dfRegularSeason$OpponentTotalYards
dfRegularSeason$ThisTeamPassYardsAllowed = dfRegularSeason$OpponentPassYards
dfRegularSeason$ThisTeamRushYardsAllowed = dfRegularSeason$OpponentRushYards
dfRegularSeason$ThisTeam1DAllowed = dfRegularSeason$Opponent1D

dfRegularSeason$OpponentTotalYardsAllowed = dfRegularSeason$ThisTeamTotalYards
dfRegularSeason$OpponentPassYardsAllowed = dfRegularSeason$ThisTeamPassYards
dfRegularSeason$OpponentRushYardsAllowed = dfRegularSeason$ThisTeamRushYards
dfRegularSeason$Opponent1DAllowed = dfRegularSeason$ThisTeam1D

TeamsList = split(dfRegularSeason, dfRegularSeason$ThisTeam)
TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ThisTeamTotalYards", 10)
TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ScoreDifference", 10)
TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ThisTeamTotalYardsAllowed", 10)
TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ThisTeamPassYardsAllowed", 10)
TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ThisTeamRushYardsAllowed", 10)
TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ThisTeam1DAllowed", 10)
dfAdjustedData = do.call(rbind, TeamsList)
rm(TeamsList)

dfTeamTranslator = GetTeamTranslator()
dfAdjustedData$ThisTeam = sapply(dfAdjustedData$ThisTeam, vlookup, dfTeamTranslator,"Abbreviation", "TeamName")

#==============================================================================
# Prepare the game data set and clean it up
dfHomeGames = dfRegularSeason[dfRegularSeason$Home,]
rm(dfRegularSeason)

dfGameSet = with(dfHomeGames, data.frame(GameDate = GameDate
                                         , OT = OT
                                         , Home = ThisTeam
                                         , Visitor = Opponent
                                         , HomeScore = ThisTeamScore
                                         , VisitorScore = OpponentScore
                                         , Home1D = ThisTeam1D
                                         , HomeTotalYards = ThisTeamTotalYards
                                         , HomePassYards = ThisTeamPassYards
                                         , HomeRushYards = ThisTeamRushYards
                                         , Visitor1D = Opponent1D
                                         , VisitorTotalYards = OpponentTotalYards
                                         , VisitorPassYards = OpponentPassYards
                                         , VisitorRushYards = OpponentRushYards
                                         , stringsAsFactors = FALSE))

rm(dfHomeGames)

dfTeamTranslator = GetTeamNameCleanup()
dfGameSet$AdjustVisitorName = sapply(dfGameSet$Visitor, vlookup, dfTeamTranslator, "OldName", "CurrentName")
dfGameSet$Visitor = ifelse(is.na(dfGameSet$AdjustVisitorName), dfGameSet$Visitor, dfGameSet$AdjustVisitorName)
dfGameSet$AdjustVisitorName = NULL

dfTeamTranslator = GetTeamTranslator()
dfGameSet$Home = sapply(dfGameSet$Home, vlookup, dfTeamTranslator,"Abbreviation", "TeamName")

dfGameSet$HomeWin = with(dfGameSet, ifelse(HomeScore >= VisitorScore, 1, 0))

#==========================================================
# Add moving average figures
LookupWhat = dfGameSet[,c("GameDate", "Home")]
colnames(LookupWhat)[2] = "ThisTeam"

dfGameSet$HomeTotalYardsSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ThisTeamTotalYardsSMAPre")
dfGameSet$HomeScoreDifferenceSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ScoreDifferenceSMAPre")
dfGameSet$HomeTotalYardsAllowedSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ThisTeamTotalYardsAllowedSMAPre")
dfGameSet$HomePassYardsAllowedSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ThisTeamPassYardsAllowedSMAPre")
dfGameSet$HomeRushYardsAllowedSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ThisTeamRushYardsAllowedSMAPre")
dfGameSet$Home1DSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ThisTeam1DAllowedSMAPre")

LookupWhat = dfGameSet[,c("GameDate", "Visitor")]
colnames(LookupWhat)[2] = "ThisTeam"

dfGameSet$VisitorTotalYardsSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ThisTeamTotalYardsSMAPre")
dfGameSet$VisitorScoreDifferenceSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ScoreDifferenceSMAPre")
dfGameSet$VisitorTotalYardsAllowedSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ThisTeamTotalYardsAllowedSMAPre")
dfGameSet$VisitorPassYardsAllowedSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ThisTeamPassYardsAllowedSMAPre")
dfGameSet$VisitorRushYardsAllowedSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ThisTeamRushYardsAllowedSMAPre")
dfGameSet$Visitor1DSMAPre = multiColumnLookup (LookupWhat, dfAdjustedData, "ThisTeam1DAllowedSMAPre")

rm(LookupWhat)
rm(dfTeamTranslator)
rm(dfAdjustedData)

#===========================================================
# Start predicting
dfPredictionSet = dfGameSet[!is.na(dfGameSet$HomeScoreDifferenceSMAPre),]
rm(dfGameSet)
dfPredictionSet = dfPredictionSet[!is.na(dfPredictionSet$VisitorScoreDifferenceSMAPre),]

dfPredictionSet$HomeScoreAdvantage = with(dfPredictionSet, HomeScoreDifferenceSMAPre - VisitorScoreDifferenceSMAPre)
dfPredictionSet$YardAdvantage = with(dfPredictionSet, HomeTotalYardsSMAPre + VisitorTotalYardsAllowedSMAPre)
dfPredictionSet$TotalAdvantage = with(dfPredictionSet, inv.logit(HomeScoreAdvantage) + inv.logit(YardAdvantage))

DrawPrediction = function(df, WhatPredictor, WhatResponse)
{
  plot(jitter(df[,WhatPredictor]), jitter(df[,WhatResponse], factor=0.5), xlab=WhatPredictor, ylab = WhatResponse)
  fit = glm(df[,WhatResponse] ~ df[,WhatPredictor], family=binomial(link="logit"))
  curve( inv.logit(coef(fit)[1] + coef(fit)[2]*x), add=T)
  abline(0.5, 0)
  xIntercept = (logit(0.5) - coef(fit)[1]) / coef(fit)[2]
  abline(v = xIntercept)
  xPoints = c(min(df[,WhatPredictor]), min(df[,WhatPredictor]), xIntercept, xIntercept)
  yPoints = c(.5,1.5,1.5,.5)
  polygon(xPoints, yPoints, col=alpha("red", 0.5), border=NA, density = )
  xPoints = c(max(df[,WhatPredictor]), max(df[,WhatPredictor]), xIntercept, xIntercept)
  yPoints = c(-.5,.5,.5,-.5)
  polygon(xPoints, yPoints, col=alpha("red", 0.5), border=NA, density = )
  
  mojo = as.data.frame(predict(fit))
  colnames(mojo)[1] = "Pr"
  mojo$LogitPr = inv.logit(mojo$Pr)
  mojo$PredictedWin = ifelse(mojo$LogitPr > .5,1,0)
  mojo$ActualWin =dfPredictionSet$HomeWin
  
  mojo$Success = with(mojo, PredictedWin == ActualWin)
  
  rate = sum(mojo$Success) / length(mojo$Success)
  print(rate)
}

DrawPrediction(dfPredictionSet, "HomeScoreAdvantage", "HomeWin")
DrawPrediction(dfPredictionSet, "YardAdvantage", "HomeWin")
DrawPrediction(dfPredictionSet, "TotalAdvantage", "HomeWin")


#plot(jitter(df[,WhatPredictor]), jitter(df[,WhatResponse], factor=0.5), xlab=WhatPredictor, ylab = WhatResponse)
fit = glm(HomeWin ~ HomeScoreAdvantage + YardAdvantage, family=binomial(link="logit"), data=dfPredictionSet)
curve( inv.logit(coef(fit)[1] + coef(fit)[2]*x), add=T)
abline(0.5, 0)
xIntercept = (logit(0.5) - coef(fit)[1]) / coef(fit)[2]
abline(v = xIntercept)
xPoints = c(min(df[,WhatPredictor]), min(df[,WhatPredictor]), xIntercept, xIntercept)
yPoints = c(.5,1.5,1.5,.5)
polygon(xPoints, yPoints, col=alpha("red", 0.5), border=NA, density = )
xPoints = c(max(df[,WhatPredictor]), max(df[,WhatPredictor]), xIntercept, xIntercept)
yPoints = c(-.5,.5,.5,-.5)
polygon(xPoints, yPoints, col=alpha("red", 0.5), border=NA, density = )

mojo = as.data.frame(predict(fit))
colnames(mojo)[1] = "Pr"
mojo$LogitPr = inv.logit(mojo$Pr)
mojo$PredictedWin = ifelse(mojo$LogitPr > .5,1,0)
mojo$ActualWin =dfPredictionSet$HomeWin

mojo$Success = with(mojo, PredictedWin == ActualWin)

rate = sum(mojo$Success) / length(mojo$Success)
rate

HomeWinPct = sum(dfGameSet$HomeWin) / length(dfGameSet$HomeWin)
HomeWinPct

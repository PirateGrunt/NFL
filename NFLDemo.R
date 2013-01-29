source("https://raw.github.com/PirateGrunt/NFL/master/GetNFLData.R")
source("https://raw.github.com/PirateGrunt/NFL/master/AdjustData.R")

df = GetTeamSeasonResults (2000:2011, "nyj")
dfGames = GetNFLData(years=1985:2011)
dfGames$ScoreDifference = with(dfGames, ThisTeamScore - OpponentScore)

dfRegularSeason = dfGames[dfGames$Week %in% 1:18,]
dfPostSeason = dfGames[!(dfGames$Week %in% 1:18),]

# Note that there are several tie games in the data set, so the total 
# number of wins will be less than the total number of games played.
homeWins = with(dfRegularSeason, sum(dfRegularSeason[Home == TRUE, "Win"]))
awayWins = with(dfRegularSeason, sum(dfRegularSeason[Home != TRUE, "Win"]))

homeWinPct = homeWins / (homeWins + awayWins)
homeWinPct



# Adjust the data
dfAdjustedData = AdjustAllTeamsData(dfGames, NumberOfGames = 10, Smoother = 0.5, ScoreLimit = 999)

# Average yardage vs win
HomeColor = ifelse(dfGames$Home == TRUE, "blue", "red")
with (dfGames, plot(jitter(ThisTeamTotalYards), jitter(Win, factor=0.5), xlab="Offensive Yards", ylab = "Wins", col=HomeColor))
fit = glm(Win ~ ThisTeamTotalYards, family=binomial(link="logit"), data = dfGames)
curve( inv.logit(coef(fit)[1] + coef(fit)[2]*x), add=T)

with (dfGames, plot(jitter(ThisTeamPassYards), jitter(Win, factor=0.5), xlab="Passing Yards", ylab = "Wins", col=HomeColor))
fit = glm(Win ~ ThisTeamPassYards, family=binomial(link="logit"), data = dfGames)
curve( inv.logit(coef(fit)[1] + coef(fit)[2]*x), add=T)

with (dfGames, plot(jitter(ThisTeamRushYards), jitter(Win, factor=0.5), xlab="Rushing Yards", ylab = "Wins", col=HomeColor))
fit = glm(Win ~ ThisTeamRushYards, family=binomial(link="logit"), data = dfGames)
curve( inv.logit(coef(fit)[1] + coef(fit)[2]*x), add=T)

dfGames$PassYardPct = with(dfGames, ThisTeamPassYards / ThisTeamTotalYards)

with (dfGames, plot(jitter(PassYardPct), jitter(Win, factor=0.5), xlab="% Passing Yards", ylab = "Wins", col=HomeColor))
fit = glm(Win ~ PassYardPct, family=binomial(link="logit"), data = dfGames)
curve( inv.logit(coef(fit)[1] + coef(fit)[2]*x), add=T)

with (dfGames, plot(jitter(ThisTeamRushYards), jitter(Win, factor=0.5), xlab="Rushing Yards", ylab = "Wins", col=HomeColor))
fit = glm(Win ~ ThisTeamRushYards:Home, family=binomial(link="logit"), data = dfGames)
curve( inv.logit(coef(fit)[1] + coef(fit)[2]*x), add=T)

df = dfAdjustedData[!is.na(dfAdjustedData$ScoreDifferenceSMAPre),]
with (df, plot(jitter(ScoreDifferenceSMAPre), jitter(Win, factor=0.5), xlab="SMA Pre", ylab = "Wins", col=HomeColor))
fit = glm(Win ~ ScoreDifferenceSMAPre, family=binomial(link="logit"), data = df)
curve( inv.logit(coef(fit)[1] + coef(fit)[2]*x), add=T)


TeamsList = split(dfGames, dfGames$ThisTeam)
TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ThisTeamTotalYards", 10)
TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ScoreDifference", 10)
dfAdjustedData = do.call(rbind, TeamsList)
rm(TeamsList)

dfAdjustedData = dfAdjustedData[!is.na(dfAdjustedData$ThisTeamTotalYardsSMAPre),]

with (dfAdjustedData, plot(jitter(ThisTeamTotalYardsSMAPre), jitter(Win, factor=0.5), xlab="Historical Avg Yards", ylab = "Wins", col=HomeColor))
fitAvgYard = glm(Win ~ ThisTeamTotalYardsSMAPre, family=binomial(link="logit"), data = dfAdjustedData)
curve( inv.logit(coef(fitAvgYard)[1] + coef(fitAvgYard)[2]*x), add=T)

with (dfAdjustedData, plot(jitter(ScoreDifferenceSMAPre), jitter(Win, factor=0.5), xlab="Historical Avg ScoreDiff", ylab = "Wins", col=HomeColor))
fitAvgScore = glm(Win ~ ScoreDifferenceSMAPre, family=binomial(link="logit"), data = dfAdjustedData)
curve( inv.logit(coef(fitAvgScore)[1] + coef(fitAvgScore)[2]*x), add=T)

dfAdjustedData$HomeCat = ifelse(dfAdjustedData$Home, 1, 0)

with (dfAdjustedData, plot(jitter(ScoreDifferenceSMAPre), jitter(Win, factor=0.5), xlab="Historical Avg ScoreDiff", ylab = "Wins", col=HomeColor))
fitAvgScoreHome = glm(Win ~ ScoreDifferenceSMAPre + HomeCat, family=binomial(link="logit"), data = dfAdjustedData)
curve( inv.logit(coef(fitAvgScoreHome)[1] + coef(fitAvgScoreHome)[2]*x + coef(fitAvgScoreHome)[3]), add=T)
curve( inv.logit(coef(fitAvgScoreHome)[1] + coef(fitAvgScoreHome)[2]*x), add=T)

mojo = as.data.frame(predict(fitAvgScoreHome))
colnames(mojo)[1] = "Pr"
mojo$LogitPr = inv.logit(mojo$Pr)
mojo$PredictedWin = ifelse(mojo$LogitPr > .5,1,0)
mojo$ActualWin = dfAdjustedData$Win

mojo$Success = with(mojo, PredictedWin == ActualWin)

rate = sum(mojo$Success) / length(mojo$Success)

summary(fitAvgYard)
summary(fitAvgScore)
summary(fitAvgScoreHome)
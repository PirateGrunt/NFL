source("https://raw.github.com/PirateGrunt/NFL/master/GetNFLData.R")
source("https://raw.github.com/PirateGrunt/NFL/master/AdjustNFLData.R")

df = GetTeamSeasonResults (2000:2011, "nyj")
dfGames = GetNFLData(years=2000:2011)
dfGames$ScoreDifference = with(dfGames, ThisTeamScore - OpponentScore)

dfRegularSeason = dfGames[dfGames$Week %in% 1:17,]

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

TeamsList = split(dfGames, dfGames$ThisTeam)
TeamsList = lapply(TeamsList, CalculateSimpleMovingAverage, "ScoreDifference", NumberOfGames)

df = dfAdjustedData[!is.na(dfAdjustedData$ScoreDifferenceSMAPre),]
with (df, plot(jitter(ScoreDifferenceSMAPre), jitter(Win, factor=0.5), xlab="SMA Pre", ylab = "Wins", col=HomeColor))
fit = glm(Win ~ ScoreDifferenceSMAPre, family=binomial(link="logit"), data = df)
curve( inv.logit(coef(fit)[1] + coef(fit)[2]*x), add=T)
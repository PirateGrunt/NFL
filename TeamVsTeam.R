source("https://raw.github.com/PirateGrunt/NFL/master/GetNFLData.R")

dfGames = GetNFLData(years=2000:2011)

dfTeamVsTeam = dfGames[,c("Opponent", "ThisTeam", "ThisTeam1D", "ThisTeamTotalYards", "ThisTeamPassYards", "ThisTeamRushYards")]
dfTeamVsTeam = aggregate(dfTeamVsTeam, by = list(dfTeamVsTeam$Opponent, dfTeamVsTeam$ThisTeam), mean)
colnames(dfTeamVsTeam)[1] = "Opponent"
colnames(dfTeamVsTeam)[2] = "ThisTeam"
dfTeamVsTeam[,3] = NULL
dfTeamVsTeam[,3] = NULL

library(ggplot2)
p = ggplot(dfTeamVsTeam, aes(x = as.factor(ThisTeam), y = as.factor(Opponent), fill = ThisTeamTotalYards))
p = p + geom_tile() +  scale_fill_gradient(low="red", high="blue")
p = p + labs(title = "Total yardage", x = "Offense", y = "Defense")
p = p + theme(panel.grid.major = element_blank())
print(p)

p = ggplot(dfTeamVsTeam, aes(x = as.factor(ThisTeam), y = as.factor(Opponent), fill = ThisTeamPassYards))
p = p + geom_tile() +  scale_fill_gradient(low="red", high="blue")
p = p + labs(title = "Pass yards", x = "Offense", y = "Defense")
p = p + theme(panel.grid.major = element_blank())
print(p)

p = ggplot(dfTeamVsTeam, aes(x = as.factor(ThisTeam), y = as.factor(Opponent), fill = ThisTeamRushYards))
p = p + geom_tile() +  scale_fill_gradient(low="red", high="blue")
p = p + labs(title = "Rush yards", x = "Offense", y = "Defense")
p = p + theme(panel.grid.major = element_blank())
print(p)
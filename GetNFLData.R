require(gtools)

GetTeamSeasonResults = function(year, team)
{
  
  require(XML)
  require(lubridate)

  games.URL.stem = "http://www.pro-football-reference.com/teams/"
  URL = paste(games.URL.stem, team, "/", year, "_games.htm", sep="")
  
  colClasses = c(rep("character",9), rep("integer", 12), rep("numeric",3))
  games = readHTMLTable(URL, colClasses = colClasses, stringsAsFactors = FALSE)
  
  if (length(games) == 0) {
    return (NULL)
  }
  
  df = games[[1]]
  
  df = df[,1:21]
  
  # Clean up the df
  df[,4] = NULL
  
  emptyRow = which(df[,2] == "")
  if(length(emptyRow) > 0 )
  {
    df = df[-emptyRow,]
    row.names(df) = seq(nrow(df))
  }
  
  colnames(df) = c("Week", "Day", "Date", "Outcome", "OT", "Record","Home", "Opponent", "ThisTeamScore", "OpponentScore"
                   , "ThisTeam1D", "ThisTeamTotalYards", "ThisTeamPassYards", "ThisTeamRushYards", "ThisTeamTO"
                   , "Opponent1D", "OpponentTotalYards", "OpponentPassYards", "OpponentRushYards", "OpponentTO")

  whichCells = is.na(df)
  df[whichCells] = 0
  
  df$GameDate = mdy(paste(df$Date, year), quiet=T)
  year(df$GameDate) = with(df, ifelse(month(GameDate) <=6, year(GameDate)+1, year(GameDate)))
  
  df$Date = as.character(df$Date)
  df$Home = with(df, ifelse(Home == "@",F,T))
  df$TODiff = with(df, as.integer(OpponentTO) - as.integer(ThisTeamTO))
  df$Win = ifelse(df$Outcome =="W", 1, 0)
  
  df$ThisTeam = team
  df$ScoreDifference = with(df, ThisTeamScore - OpponentScore)
  
  return(df)
}

GetNFLData = function(years = 2011)
{
  #TODO: Sort out some way to handle teams which relocate.
  teams = c("nyj",  "mia", "nwe", "buf"
            , "rav", "cin", "pit", "cle"
            , "htx", "clt", "oti", "jax"
            , "den", "sdg", "rai", "kan"
            , "dal", "nyg", "was", "phi"
            , "gnb", "chi", "min", "det"
            , "atl", "tam", "nor", "car"
            , "sfo", "sea", "ram", "crd")
  
  df = expand.grid(years = years, teams = teams)  
  aList = mapply(GetTeamSeasonResults, df$years, df$teams, SIMPLIFY = F)
  df = as.data.frame(do.call("rbind", aList))
  
  return(df)
}

GetTeamTranslator = function()
{
  teams = c("nyj",  "mia", "nwe", "buf"
            , "rav", "cin", "pit", "cle"
            , "htx", "clt", "oti", "jax"
            , "den", "sdg", "rai", "kan"
            , "dal", "nyg", "was", "phi"
            , "gnb", "chi", "min", "det"
            , "atl", "tam", "nor", "car"
            , "sfo", "sea", "ram", "crd")
  
  teamNames  =c("New York Jets", "Miami Dolphins", "New England Patriots", "Buffalo Bills"
                , "Baltimore Ravens", "Cincinnati Bengals", "Pittsburgh Steelers", "Cleveland Browns"
                , "Houston Texans", "Indianapolis Colts", "Tennessee Titans", "Jacksonville Jaguars"
                , "Denver Broncos", "San Diego Chargers", "Oakland Raiders", "Kansas City Chiefs"
                , "Dallas Cowboys", "New York Giants", "Washington Redskins", "Philadelphia Eagles"
                , "Green Bay Packers", "Chicago Bears", "Minnesota Vikings", "Detroit Lions"
                , "Atlanta Falcons", "Tampa Bay Buccaneers", "New Orleans Saints", "Carolina Panthers"
                , "San Francisco 49ers", "Seattle Seahawks", "St. Louis Rams", "Arizona Cardinals")
  
  df = data.frame(Abbreviation = teams, TeamName = teamNames)
  
  return (df)
}


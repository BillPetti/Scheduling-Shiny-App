##code for querying the FanGraphs pitch by pitch database for daily updates for Edge% numbers and updating a Shiny app to display and filter the data
##Bill Petti 
## Updated September 2016 to run on a Mac

#load RMySQL and dplyr packagea for fetching and manipulating the data, 

require(methods)
require(RMySQL)
require(dplyr)
require(readr)
require(baseballr)

# custom Batter function for edge splits

edge_frequency_custom <- function (df, group = NULL, stand = stand) 
{
  if (is.null(group)) {
    grouped <- filter(df, !is.na(px), !is.na(pz)) %>% summarise(All_pitches = n(), 
                                                                All_calls = sum(called_pitch), Called_Strike = sum(called_strike), 
                                                                Called_strike_rate = round(sum(called_strike)/sum(called_pitch), 
                                                                                           3), Upper_Edge = sum(Upper_Edge)/All_pitches, 
                                                                Lower_Edge = sum(Lower_Edge)/All_pitches, Inside_Edge = sum(Inside_Edge)/All_pitches, 
                                                                Outside_Edge = sum(Outside_Edge)/All_pitches, Heart = sum(Heart)/All_pitches, 
                                                                Out_of_Zone = sum(OutOfZone)/All_pitches) %>% mutate(Total_Edge = Upper_Edge + 
                                                                                                                       Lower_Edge + Inside_Edge + Outside_Edge)
    grouped
  }
  else {
    grouped <- filter(df, !is.na(px), !is.na(pz)) %>% group_by_(group, stand) %>% 
      summarise(All_pitches = n(), All_calls = sum(called_pitch), 
                Called_Strike = sum(called_strike), Called_strike_rate = round(sum(called_strike)/sum(called_pitch), 
                                                                               3), Upper_Edge = sum(Upper_Edge)/All_pitches, 
                Lower_Edge = sum(Lower_Edge)/All_pitches, Inside_Edge = sum(Inside_Edge)/All_pitches, 
                Outside_Edge = sum(Outside_Edge)/All_pitches, 
                Heart = sum(Heart)/All_pitches, Out_of_Zone = sum(OutOfZone)/All_pitches) %>% 
      mutate(Total_Edge = Upper_Edge + Lower_Edge + Inside_Edge + 
               Outside_Edge)
    grouped
  }
}

edge_frequency_team_pitchers <- function(df, group = NULL) 
{
    grouped <- filter(df, !is.na(px), !is.na(pz)) %>% group_by_(group) %>% summarise(All_pitches = n(), 
                                                                All_calls = sum(called_pitch), Called_Strike = sum(called_strike), 
                                                                Called_strike_rate = round(sum(called_strike)/sum(called_pitch), 
                                                                                           3), Upper_Edge = sum(Upper_Edge)/All_pitches, 
                                                                Lower_Edge = sum(Lower_Edge)/All_pitches, Inside_Edge = sum(Inside_Edge)/All_pitches, 
                                                                Outside_Edge = sum(Outside_Edge)/All_pitches, Heart = sum(Heart)/All_pitches, 
                                                                Out_of_Zone = sum(OutOfZone)/All_pitches) %>% mutate(Total_Edge = Upper_Edge + 
                                                                                                                       Lower_Edge + Inside_Edge + Outside_Edge)
    grouped
}

edge_frequency_team_batters <- function(df, group = NULL, stands = stand) 
{
  grouped <- filter(df, !is.na(px), !is.na(pz)) %>% group_by_(group, stands) %>% summarise(All_pitches = n(), 
                                                                                   All_calls = sum(called_pitch), Called_Strike = sum(called_strike), 
                                                                                   Called_strike_rate = round(sum(called_strike)/sum(called_pitch), 
                                                                                                              3), Upper_Edge = sum(Upper_Edge)/All_pitches, 
                                                                                   Lower_Edge = sum(Lower_Edge)/All_pitches, Inside_Edge = sum(Inside_Edge)/All_pitches, 
                                                                                   Outside_Edge = sum(Outside_Edge)/All_pitches, Heart = sum(Heart)/All_pitches, 
                                                                                   Out_of_Zone = sum(OutOfZone)/All_pitches) %>% mutate(Total_Edge = Upper_Edge + 
                                                                                                                                          Lower_Edge + Inside_Edge + Outside_Edge)
  grouped
}

edge_frequency_umpires <- function (df) {
    grouped <- filter(df, !is.na(px), !is.na(pz)) %>%
      group_by(ump_name, stand, p_throws) %>%
      summarise(All_pitches = n(), 
                                                                All_calls = sum(called_pitch), Called_Strike = sum(called_strike), 
                                                                Called_strike_rate = round(sum(called_strike)/sum(called_pitch), 
                                                                                           3), Upper_Edge = sum(Upper_Edge)/All_pitches, 
                                                                Lower_Edge = sum(Lower_Edge)/All_pitches, Inside_Edge = sum(Inside_Edge)/All_pitches, 
                                                                Outside_Edge = sum(Outside_Edge)/All_pitches, Heart = sum(Heart)/All_pitches, 
                                                                Out_of_Zone = sum(OutOfZone)/All_pitches) %>% mutate(Total_Edge = Upper_Edge + 
                                                                                                                       Lower_Edge + Inside_Edge + Outside_Edge)
    grouped
}

# import existing data files

Pitchers_old <- read_csv("/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Pitchers_Edge.csv", col_types = cols(Pitcher = col_character()))

Batters_old <- read_csv("/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Batters_Edge.csv", col_types = cols(Batter = col_character()))

Team_P_old <- read_csv("/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Team_P_Edge.csv", col_types = cols(Team = col_character()))

Team_B_old <- read_csv("/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Team_B_Edge.csv", col_types = cols(Team = col_character()))

ump_location_old <- read_csv("/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/ump_location.csv", col_types = cols(Umpire = col_character(), `Pitch Location` = col_character()))

# current year

current_year <- as.numeric(substr(Sys.Date(),1,4))
current_date <- paste0("'", Sys.Date(), "'")
first_date <- paste0("'", "2016-03-31", "'")

# remove current year stats

Pitchers_old <- filter(Pitchers_old, Year < current_year)

Batters_old <- filter(Batters_old, Year < current_year)

Team_P_old <- filter(Team_P_old, Year < current_year)

Team_B_old <- filter(Team_B_old, Year < current_year)

ump_location_old <- filter(ump_location_old, Year < current_year)

## connect to FanGraphs database

# load FanGraphs database authorization 

fg_database_values <- c("/Users/williampetti/bpAuthsCons/fg_dbname.rda", "/Users/williampetti/bpAuthsCons/fg_username.rda", "/Users/williampetti/bpAuthsCons/fg_password.rda", "/Users/williampetti/bpAuthsCons/fg_host.rda")

lapply(fg_database_values, load, .GlobalEnv)

con <- dbConnect(RMySQL::MySQL(), dbname = fg_dbname, username = fg_username, password = fg_password, host = fg_host, port = 3306)

# query pfx for 2016

current_year <- paste0("'", substr(Sys.Date(),1,4), "'")

pfx_query <- paste0("select GameDate, HomeTeamId, AwayTeamId, InningHalf, batter, pitcher, p.px, p.pz, des2, stand, p_throws, b_height, DH from gd_pitch p where gamedate between ", first_date, " and ", current_date, " and pitch_type <> 'XC'")

pfx <- dbGetQuery(con, pfx_query)

pfx <- edge_code(pfx)

gc()

# get player lookup table

player_info <- paste("SELECT * FROM player_info a LEFT JOIN playerid_lookup b ON a.PlayerId = b.playerid", sep = "")

player_table <- dbGetQuery(con, player_info)

player_table <- player_table %>%
  select(FirstName, LastName, mlbamid) %>%
  mutate(Name = paste0(FirstName, " ", LastName)) %>%
  select(Name, mlbamid) %>%
  unique()

# get team lookup table

team_info <- paste("SELECT TeamId, ShortName FROM season_team WHERE Season = 2016", sep = "")

team_table <- dbGetQuery(con, team_info)

# Pitchers

Pitchers <- edge_frequency(pfx, group = "pitcher") %>%
  left_join(player_table, by = c("pitcher" = "mlbamid")) %>%
  mutate(Total_Horizontal_Edge = Outside_Edge + Inside_Edge, Year = 2016) %>%
  ungroup()

# Batters

Batters <- edge_frequency_custom(pfx, group = "batter", stand = "stand") %>%
  left_join(player_table, by = c("batter" = "mlbamid")) %>%
  mutate(Total_Horizontal_Edge = Outside_Edge + Inside_Edge, Year = 2016) %>% 
  ungroup()

# Teams (note: InninfHalf of 0 = top, 1 = bottom)

pfx <- pfx %>%
  left_join(team_table, by = c("HomeTeamId" = "TeamId")) %>% 
  mutate(HomeTeam = ShortName) %>%
  select(-ShortName)

pfx <- pfx %>%
  left_join(team_table, by = c("AwayTeamId" = "TeamId")) %>% 
  mutate(AwayTeam = ShortName) %>%
  select(-ShortName)

pfx$pitching_team <- with(pfx, ifelse(InningHalf == 0, HomeTeam, AwayTeam))
pfx$batting_team <- with(pfx, ifelse(InningHalf == 0, AwayTeam, HomeTeam))

Team_P <- edge_frequency_team_pitchers(pfx, group = "pitching_team") %>%
  mutate(Total_Horizontal_Edge = Outside_Edge + Inside_Edge, Year = 2016)

Team_B <- edge_frequency_team_batters(pfx, group = "batting_team", stands = "stand") %>%
  mutate(Total_Horizontal_Edge = Outside_Edge + Inside_Edge, Year = 2016)

## query umps

# get ump id's

system.time(umps <- read_csv("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"))

umps <- filter(umps, !is.na(mlb_umpired_first)) %>%
  select(key_mlbam, name_first, name_last) %>%
  mutate(ump_name = paste0(name_first, " ", name_last)) %>%
  select(key_mlbam, ump_name)

max_date <- Sys.Date()

dbDisconnect(con)

con <- dbConnect(RMySQL::MySQL(), dbname = fg_dbname, username = fg_username, password = fg_password, host = fg_host, port = 3306)

ump_query <- paste0("select a.game_date, b.hometeamid, b.awayteamid, b.dh, a.umpire from gd_savant a left join gd_gameinfo b on (a.game_date = b.gamedate AND a.game_pk = b.game_pk) where a.game_year = '2016' and a.game_pk <> 0 group by a.game_pk")

umps_games <- dbGetQuery(con, ump_query)
unique_umps <- unique(umps_games$umpire)
join_umps <- umps %>% filter(key_mlbam %in% unique_umps)

pfx <- pfx %>% left_join(umps_games, by = c("GameDate" = "game_date", "DH" = "dh", "HomeTeamId" = "hometeamid", "AwayTeamId" = "awayteamid"))

pfx <- pfx %>% left_join(join_umps, by = c("umpire" = "key_mlbam"))

gc()

umpires <- edge_frequency_umpires(pfx)

lgAve <- pfx %>% group_by(location, stand, p_throws) %>% summarise(Called_Strike = sum(called_strike), All_Calls = sum(called_pitch), Lg_Called_Strike_Rate = round(sum(called_strike/sum(called_pitch)),3)) 

ump_location <- pfx %>% group_by(ump_name, location, stand, p_throws) %>% summarise(Called_Strike = sum(called_strike), All_Calls = sum(called_pitch), Called_Strike_Rate = round(sum(called_strike/sum(called_pitch)),3)) %>% mutate(Year = 2016)

ump_location <- ump_location %>% left_join(lgAve[, c(1:4, 6)], by = c("location" = "location", "stand" = "stand", "p_throws" = "p_throws")) %>% .[,c(1:4, 6:7, 8, 10)] %>% mutate(`Called Strike % +` = round(Called_Strike_Rate/Lg_Called_Strike_Rate, 2)) %>% filter(!is.na(ump_name)) %>% .[,-8]

ump_location <- select(ump_location, ump_name, Year, location, stand, p_throws, All_Calls, Called_Strike_Rate, `Called Strike % +`)

names(ump_location) <- c("Umpire", "Year", "Pitch Location", "Batter Stands", "Pitcher Throws", "Called Pitches", "Called Strike %", "Called Strike % +")

ump_location <- ungroup(ump_location)

## add column for Edge/Heart ratio to new data, restructure data frames, and rename variables to match old data frames

Pitchers <- select(Pitchers, Name, Year, All_pitches, Inside_Edge, Outside_Edge, Total_Horizontal_Edge, Upper_Edge, Lower_Edge, Total_Edge, Heart, Out_of_Zone)

Pitchers$`Edge/Heart Ratio` <- round(Pitchers$Total_Edge/Pitchers$Heart,1)

names(Pitchers) <- c('Pitcher','Year','# of total pitches','Inside Edge%','Outside Edge%','Total Horizontal Edge%','Top Edge%','Bottom Edge%','Total Edge%','Heart%','Out of Zone%','Edge/Heart Ratio')

Pitchers <- Pitchers %>% mutate_each(funs(round(., 4)), -c(1:3, 12))

Batters <- select(Batters, Name, Year, stand, All_pitches, Inside_Edge, Outside_Edge, Total_Horizontal_Edge, Upper_Edge, Lower_Edge, Total_Edge, Heart, Out_of_Zone)

Batters$`Edge/Heart Ratio` <- round(Batters$Total_Edge/Batters$Heart,1)

Batters <- Batters %>% mutate_each(funs(round(., 4)), -c(1:4, 13))

names(Batters) <- c('Batter','Year','Handedness', '# of total pitches','Inside Edge%','Outside Edge%','Total Horizontal Edge%','Top Edge%','Bottom Edge%','Total Edge%','Heart%','Out of Zone%','Edge/Heart Ratio')

Team_P$`Edge/Heart Ratio` <- round(Team_P$Total_Edge/Team_P$Heart,1)

Team_P <- Team_P %>% select(pitching_team, Year, everything())

Team_P <- Team_P[,-c(4:6)]

names(Team_P) <- c('Team','Year', '# of total pitches','Inside Edge%','Outside Edge%','Total Horizontal Edge%','Top Edge%','Bottom Edge%','Total Edge%','Heart%','Out of Zone%','Edge/Heart Ratio')

Team_P <- Team_P %>% mutate_each(funs(round(., 4)), -c(1:3, 12))

Team_B$`Edge/Heart Ratio` <- round(Team_B$Total_Edge/Team_B$Heart,1)

Team_B <- Team_B[,-c(4:6)]

Team_B <- Team_B %>% select(batting_team, Year, everything())

names(Team_B) <- c('Team','Year','Handedness', '# of total pitches','Inside Edge%','Outside Edge%','Total Horizontal Edge%','Top Edge%','Bottom Edge%','Total Edge%','Heart%','Out of Zone%','Edge/Heart Ratio')

Team_B <- Team_B %>% mutate_each(funs(round(., 4)), -c(1:4, 13))

Team_B <- ungroup(Team_B)

## combine new data to existing data files

Pitchers <- rbind(Pitchers_old, Pitchers)

Batters <- rbind(Batters_old, Batters)

Team_P <- rbind(Team_P_old, Team_P)

Team_B <- rbind(Team_B_old, Team_B)

ump_location <- rbind(ump_location_old, ump_location)

## sort updated data

Pitchers <- arrange(Pitchers, desc(`Total Edge%`))

Batters <- arrange(Batters, desc(`Total Edge%`))

Team_P <- arrange(Team_P, desc(`Total Edge%`))

Team_B <- arrange(Team_B, desc(`Total Edge%`))

ump_location <- arrange(ump_location, desc(`Umpire`))

## create Dates dataframe for Shiny filter

Dates <- as.data.frame(pfx$GameDate) %>% unique()
names(Dates) <- "Dates"
Dates$Dates <- as.Date(Dates$Dates, "%Y-%m-%d")
Dates <- filter(Dates, Dates == max(Dates))

gc()

## export the updated data sets to a folder - export as csv

write.csv(Pitchers, file="/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Pitchers_Edge.csv", row.names=FALSE, col.names=TRUE, na="")

write.csv(Batters, file="/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Batters_Edge.csv", row.names=FALSE, col.names=TRUE, na="")

write.csv(Team_P, file="/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Team_P_Edge.csv", row.names=FALSE, col.names=TRUE, na="")

write.csv(Team_B, file="/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Team_B_Edge.csv", row.names=FALSE, col.names=TRUE, na="")

write.csv(ump_location, file="/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/ump_location.csv", row.names=FALSE, col.names=TRUE, na="")

write.csv(Dates, file="/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Dates.csv", row.names=FALSE, col.names=TRUE, na="")

## update and redeploy shiny app

library(shiny)
library(shinyapps)
library(rsconnect)

rsconnect::setAccountInfo(name='billpetti', token='AF62AA0CE06A6A349CF93399D6188A06', secret='ZuEDq2JwtJImW189zj/SuxZCpaAIf/jqVtfet+26')

shinyapps::deployApp('/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny')

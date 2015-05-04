##code for querying the FanGraphs pitch by pitch database for daily updates for Edge% numbers and updating a Shiny app to display and filter the data
##Bill Petti 

#load RMySQL and dplyr packagea for fetching and manipulating the data, 

require(methods)
require(RMySQL)
require(dplyr)

#import existing data files

Pitchers_old<-read.csv("C:/Users/Petti/Box Sync/Default Sync Folder/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Pitchers_Edge.csv", header=TRUE, check.names = FALSE)

Batters_old<-read.csv("C:/Users/Petti/Box Sync/Default Sync Folder/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Batters_Edge.csv", header=TRUE, check.names = FALSE)

#remove current year stats

Pitchers_old<-filter(Pitchers_old, Year<2015)

Batters_old<-filter(Batters_old, Year<2015)

#connect to database

con<-dbConnect(RMySQL::MySQL(), dbname="tht", username = "bill.petti", password = "rt81csB8", host = "db.fangraphs.com", port = 3306)

#query pitchers

Pitchers<-dbGetQuery(con, "select concat(pl_p.FirstName,' ',pl_p.LastName) as Pitcher, pl_p.playerid as MLBAM_ID, substr(gamedate,1,4) as 'Year',

sum(if(px is not null or pz is not null or pl_b.Height is not null, 1, 0)) as '# of total pitches',

sum(if(px is not null and pz is not null, ((stand = 'R' and (((px > -1.03 and px < -.43 ) or (px > .7 and px < 1.00 )) and (pz > (.92 + pl_b.Height/12 *.136) and pz < (2.60 +  pl_b.height/12 *.136))))
	or
	(stand = 'L' and (((px > -1.20 and px < -.9) or (px > .21 and px < .81)) and (pz > (.35 + pl_b.height/12 *.229) and pz < (2.0 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) as 'Horizontal Edge%',


sum(if(px is not null and pz is not null, ((stand = 'R' and ((px >= -.43 and px <= .7) and (pz > (2.3 + pl_b.Height/12 *.136) and pz < (2.6 +  pl_b.height/12 *.136))))
	or 
	(stand = 'L' and ((px >= -.9 and px <= .21) and (pz > (1.7 + pl_b.height/12 *.229) and pz < (2 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) as 'Top Edge%',

sum(if(px is not null and pz is not null, ((stand = 'R' and ((px >= -.43 and px <= .7) and (pz > (.92 + pl_b.Height/12 *.136) and pz < (1.22 + pl_b.Height/12 *.136))))
	or
	(stand = 'L' and ((px >= -.9 and px <= .21) and (pz > (.35 + pl_b.height/12 *.229) and pz < (.65 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) as 'Bottom Edge%',


(sum(if(px is not null and pz is not null, ((stand = 'R' and (((px > -1.03 and px < -.43 ) or (px > .7 and px < 1.00 )) and (pz > (.92 + pl_b.Height/12 *.136) and pz < (2.60 +  pl_b.height/12 *.136))))
  or
	(stand = 'L' and (((px > -1.20 and px < -.9) or (px > .21 and px < .81)) and (pz > (.35 + pl_b.height/12 *.229) and pz < (2.0 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) 

+

sum(if(px is not null and pz is not null, ((stand = 'R' and ((px >= -.43 and px <= .7) and (pz > (2.3 + pl_b.Height/12 *.136) and pz < (2.6 +  pl_b.height/12 *.136))))
	or 
	(stand = 'L' and ((px >= -.9 and px <= .21) and (pz > (1.7 + pl_b.height/12 *.229) and pz < (2 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0))

+

sum(if(px is not null and pz is not null, ((stand = 'R' and ((px >= -.43 and px <= .7) and (pz > (.92 + pl_b.Height/12 *.136) and pz < (1.22 + pl_b.Height/12 *.136))))
	or
	(stand = 'L' and ((px >= -.9 and px <= .21) and (pz > (.35 + pl_b.height/12 *.229) and pz < (.65 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0))) as 'Total Edge%',

sum(if(px is not null and pz is not null, ((stand = 'R' and ((px >= -.43 and px <= .7) and (pz >= (1.22 + pl_b.Height/12 *.136) and pz <= (2.30 +  pl_b.height/12 *.136))))
or
(stand = 'L' and ((px >= -.9 and px <= .21) and (pz >= (.65 + pl_b.height/12 *.229) and pz <= (1.7 + pl_b.height/12 *.229))))), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) as 'Heart%',

sum(if(px is not null and pz is not null, ((stand = 'R' and (((px <= -1.03 ) or(px >= 1.00 ))
or pz <= (.92 + pl_b.Height/12 *.136)
or pz >= (2.60 +  pl_b.height/12 *.136))) or
(stand = 'L' and (((px <= -1.20) or (px >= .81))
or pz <= (.35 + pl_b.height/12 *.229)
or pz >= (2.0 + pl_b.height/12 *.229)))), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) as 'Out of Zone%'

from gd_pitch  p
join playerid_lookup lu_b on lu_b.mlbamid = p.batter
join player_info pl_b on pl_b.playerid=lu_b.playerid
join playerid_lookup lu_p on lu_p.mlbamid = p.pitcher
join player_info pl_p on pl_p.playerid=lu_p.playerid

where

substr(gamedate,1,4) = 2015

-- and pl_p.playerID='3580'
-- pitcher = '112526'
and pitch_type<> 'XC'
group by pitcher, substr(gamedate,1,4)
order by substr(gamedate,1,4) DESC, sum(if(px is not null or pz is not null or pl_b.Height is not null, 1, 0)) DESC")

#query batters

Batters<-dbGetQuery(con, "select concat(pl_b.FirstName,' ',pl_b.LastName) as Batter, pl_b.playerid as MLBAM_ID, substr(gamedate,1,4) as 'Year', p.stand as 'Handedness',

sum(if(px is not null or pz is not null or pl_b.Height is not null, 1, 0)) as '# of total pitches',

sum(if(px is not null and pz is not null, ((stand = 'R' and (((px > -1.03 and px < -.43 ) or (px > .7 and px < 1.00 )) and (pz > (.92 + pl_b.Height/12 *.136) and pz < (2.60 +  pl_b.height/12 *.136))))
	or
	(stand = 'L' and (((px > -1.20 and px < -.9) or (px > .21 and px < .81)) and (pz > (.35 + pl_b.height/12 *.229) and pz < (2.0 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) as 'Horizontal Edge%',


sum(if(px is not null and pz is not null, ((stand = 'R' and ((px >= -.43 and px <= .7) and (pz > (2.3 + pl_b.Height/12 *.136) and pz < (2.6 +  pl_b.height/12 *.136))))
	or 
	(stand = 'L' and ((px >= -.9 and px <= .21) and (pz > (1.7 + pl_b.height/12 *.229) and pz < (2 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) as 'Top Edge%',

sum(if(px is not null and pz is not null, ((stand = 'R' and ((px >= -.43 and px <= .7) and (pz > (.92 + pl_b.Height/12 *.136) and pz < (1.22 + pl_b.Height/12 *.136))))
	or
	(stand = 'L' and ((px >= -.9 and px <= .21) and (pz > (.35 + pl_b.height/12 *.229) and pz < (.65 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) as 'Bottom Edge%',

(sum(if(px is not null and pz is not null, ((stand = 'R' and (((px > -1.03 and px < -.43 ) or (px > .7 and px < 1.00 )) and (pz > (.92 + pl_b.Height/12 *.136) and pz < (2.60 +  pl_b.height/12 *.136))))
  or
  (stand = 'L' and (((px > -1.20 and px < -.9) or (px > .21 and px < .81)) and (pz > (.35 + pl_b.height/12 *.229) and pz < (2.0 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) 

+

sum(if(px is not null and pz is not null, ((stand = 'R' and ((px >= -.43 and px <= .7) and (pz > (2.3 + pl_b.Height/12 *.136) and pz < (2.6 +  pl_b.height/12 *.136))))
	or 
	(stand = 'L' and ((px >= -.9 and px <= .21) and (pz > (1.7 + pl_b.height/12 *.229) and pz < (2 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0))

+

sum(if(px is not null and pz is not null, ((stand = 'R' and ((px >= -.43 and px <= .7) and (pz > (.92 + pl_b.Height/12 *.136) and pz < (1.22 + pl_b.Height/12 *.136))))
	or
	(stand = 'L' and ((px >= -.9 and px <= .21) and (pz > (.35 + pl_b.height/12 *.229) and pz < (.65 + pl_b.height/12 *.229))))
	), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0))) as 'Total Edge%',

sum(if(px is not null and pz is not null, ((stand = 'R' and ((px >= -.43 and px <= .7) and (pz >= (1.22 + pl_b.Height/12 *.136) and pz <= (2.30 +  pl_b.height/12 *.136))))
or
(stand = 'L' and ((px >= -.9 and px <= .21) and (pz >= (.65 + pl_b.height/12 *.229) and pz <= (1.7 + pl_b.height/12 *.229))))), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) as 'Heart%',

sum(if(px is not null and pz is not null, ((stand = 'R' and (((px <= -1.03 ) or(px >= 1.00 ))
or pz <= (.92 + pl_b.Height/12 *.136)
or pz >= (2.60 +  pl_b.height/12 *.136))) or
(stand = 'L' and (((px <= -1.20) or (px >= .81))
or pz <= (.35 + pl_b.height/12 *.229)
or pz >= (2.0 + pl_b.height/12 *.229)))), 0))
/
sum(if(px is not null and pz is not null, pitch_type <> 'XC', 0)) as 'Out of Zone%'

from gd_pitch  p
join playerid_lookup lu_b on lu_b.mlbamid = p.batter
join player_info pl_b on pl_b.playerid=lu_b.playerid
join playerid_lookup lu_p on lu_p.mlbamid = p.pitcher
join player_info pl_p on pl_p.playerid=lu_p.playerid

where

substr(gamedate,1,4) = 2015

-- and pl_p.playerID='3580'
-- pitcher = '112526'
and pitch_type<> 'XC'
group by batter, stand, substr(gamedate,1,4)
order by substr(gamedate,1,4) DESC, sum(if(px is not null or pz is not null or pl_b.Height is not null, 1, 0)) DESC")

#combine new data to existing data files

Pitchers<-rbind(Pitchers_old, Pitchers)

Batters<-rbind(Batters_old, Batters)

#add column for Edge/Heart ratio

Pitchers$`Edge/Heart Ratio`<-round(Pitchers$`Total Edge%`/Pitchers$`Heart%`,1)

Batters$`Edge/Heart Ratio`<-round(Batters$`Total Edge%`/Batters$`Heart%`,1)

#sort updated data by year

Pitchers<-arrange(Pitchers, desc(Year))

Batters<-arrange(Batters, desc(Year))

#export the updated data sets to a folder - export as csv

write.csv(Pitchers, file="C:/Users/Petti/Box Sync/Default Sync Folder/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Pitchers_Edge.csv", row.names=FALSE, col.names=TRUE, na="")

write.csv(Batters, file="C:/Users/Petti/Box Sync/Default Sync Folder/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny/data/Batters_Edge.csv", row.names=FALSE, col.names=TRUE, na="")

#update and redeploy shiny app

library(shinyapps)

shinyapps::deployApp('C:/Users/Petti/Box Sync/Default Sync Folder/Baseball Data/Baseball Data/Edge_Percent/edge_percent/edge_shiny')
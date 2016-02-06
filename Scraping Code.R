## Code by Frank Firke (@clownhypothesis), 1/2016
## Scrapes pre-2009 Fans Scouting Report Data and standardizes/combines all years
## Also adds FanGraphs ID for forward compatibility
## Data is NOT mine; it's maintained and made publicly available by TangoTiger
## Links can be found here: http://www.tangotiger.net/scout/

rm(list=ls())

require(XML)
require(readr)
library(rvest)
library(stringr)
library(doParallel)
library(dplyr)
library(magrittr)

top <- "http://www.tangotiger.net/scouting/"

htms <- read_html("http://www.tangotiger.net/scouting/scoutResults2005.html")

# List of Team Abbreviations

teamlist05 <- htms %>% 
  html_nodes("a") %>%
  html_attr("href") %>% grep("scoutR",.,value=T) %>% str_sub(18,20)

options(stringsAsFactors = F)

# This is for parallelization

cl <- makeCluster(2)
registerDoParallel(cl)

# Pulls Main 2004 data
# If not parallelizing, can use %do% instead of %dopar%

data04 <- foreach(n = 1:length(teamlist05),.combine=rbind) %dopar% {
  library(dplyr)
  require(XML)
  library(rvest)
  library(stringr)
  library(magrittr)
  temp <- top %>% paste0("scoutResults_",teamlist05[n],".html") %>% 
    readHTMLTable(colClasses=c("character",rep("numeric",12)))
  temp2 <- temp[[1]] %>% mutate(Team = teamlist05[n],Season=2004)
  temp2
}

data04 %<>% rename(Overall = `OverallPosition-Neutral`,Agreement=AgreementLevel) %>% mutate(Pos=NA)

# In 2004, removes duplicate records for players that were traded

tempsupp <- top %>% paste0("scoutResults_xxx.html") %>% 
  readHTMLTable(colClasses=c("character","character",rep("numeric",11)))

doubledplayers <- tempsupp[[1]] %>% group_by(Player) %>% 
  mutate(MaxGames = max(GP)) %>% 
  filter(GP != MaxGames |  (Player == "Gonzalez, Alex" & Age == "MON")) %>% select(Player,Age)

data04 %<>% anti_join(doubledplayers,by=c("Player","Team" = "Age"))

cl <- makeCluster(2)
registerDoParallel(cl)

# Not relevant until the merge, but 2005 uses first initial and last name, not full name

data05 <- foreach(n = 1:length(teamlist05),.combine=rbind) %dopar% {
  library(dplyr)
  require(XML)
  library(rvest)
  library(stringr)
  library(magrittr)
  temp <- top %>% paste0("scoutResults2005_",teamlist05[n],".html") %>% 
    readHTMLTable(colClasses=c("character",rep("numeric",10)))
  temp2 <- temp[[1]] %>% mutate(Team = teamlist05[n],Season=2005)
  temp2
}

# Renames and removes double counted individuals

data05 %<>% rename(Agreement=AgreementLevel,Overall = `OverallPosition-Neutral`) %>% 
  mutate(Age=NA,Pos=NA,GP=NA) %>% group_by(Player,Overall) %>% mutate(MaxTeam = max(Team)) %>% 
  filter(Team==MaxTeam) %>% ungroup() %>% select(-MaxTeam) %>% mutate(Ballots=ifelse(Player=="E Garabito",1,Ballots))

scrape0608 <- function(team,year) {
  tempURL <- top %>% paste0("scoutResults",year,"_",team,".html")
  
  if (year!=2008) {
    cc <- c("character","character",rep("numeric",10))
  } else {
    cc <- c("character","character",rep("numeric",11))
  }
  
  tempURL %>% readHTMLTable(colClasses = cc) -> templist
  
  # For 2006-2008, there's a unique/persistent player ID that can be pulled out of the links
  # It's not necessary once we've added in the FG ID, but I've left it in

  ids <- tempURL %>% read_html() %>%
    html_nodes("a") %>%
    html_attr("href") %>% grep("sim",.,value=T) %>% str_sub(9,12)

    
  templist[[1]] %>% mutate(tangoid = ids,Season = year,Team = ifelse(team=="MON","WAS",team)) -> out
  
  if (year!=2008) {
    out %<>% mutate(`Position-Specific` = NA)
  }
  out
}

# Makes a list of teams and corrects for the Washington/Montreal Change

ifelse(teamlist05=="MON","WAS",teamlist05) -> teamlist08

loopframe <- data_frame(Team=sort(rep(teamlist08,3)),Season=rep(2006:2008,30)) %>% 
  mutate(Team = ifelse(Team=="WAS" & Season %in% c(2006,2007),"MON",Team))

cl <- makeCluster(2)
registerDoParallel(cl)

prelimdata0608 <- foreach(n = 1:nrow(loopframe),.combine=rbind) %dopar% {
  library(dplyr)
  require(XML)
  library(rvest)
  library(stringr)
  library(magrittr)
  scrape0608(loopframe$Team[n],loopframe$Season[n])
}

# Renames variables and removes players counted twice in 2006-2008 data

data0608 <- prelimdata0608 %>% rename(Agreement=AgreementLevel,Overall = `OverallPosition-Neutral`) %>% 
  mutate(Age=NA,GP=NA) %>% group_by(tangoid,Season) %>% mutate(MaxTeam = max(Team)) %>% 
  filter(Team==MaxTeam) %>% select(-MaxTeam) %>% ungroup()

# Pulls 2002/2003 data, which is fairly different form

test03 <- readHTMLTable("http://www.tangotiger.net/scouting/scout_Results.html")[[6]]
test03 %<>% filter(Player != "Player") %>% select(-UZR162,-FanRuns)
for (i in 2:ncol(test03)) {
  test03[,i] <- as.numeric(test03[,i])
}
names(test03)[10:12] <- c("Agreement","Ballots","GP")

poshelp <- data_frame(Pos = 2:9,CharPos = c("C","1B","2B",'3B','SS','LF','CF','RF'))

test03 %>% inner_join(poshelp,by="Pos") %>% select(-Pos) %>% rename(Pos=CharPos,FirstStep=Acceleration) %>%
  mutate(Season=2003,Team=NA,Age=NA,Overall=NA) %>% filter(!(Player == "Phillips, Jason" & Pos == "C")) -> data03

# Stacks all 2003-2008 data together

pre06 <- rbind_list(data03,data04,data05) %>% mutate(PositionSpec=NA,tangoid=NA)
data0608 %>% rename(PositionSpec = `Position-Specific`) %>% rbind_list(pre06) %>%
  rename(ArmStrength = Strength,ArmAccuracy = Accuracy,Votes=Ballots) -> combined

# Changes names in FSR data to make FanGraphs data merge

namestofix <- c('Hairston Jr., Jerry','Pena, Wily Mo','Morales, Kendry',
                'Castillo, Alberto','Nunez, Abraham O.','Upton, B.J.','Belliard, Ron',
                'Molina, Ben','Johnson, Mark L.','Ross, Dave','Choi, Hee Seop',
                'Hocking, Dennis','Cruz Jr., Jose','Davanon, Jeff','Cuddyer, Mike','Pena Jr., Tony',
                'Wise, Dewayne','Gonzalez, Luis A.','Matsui, Kazuo','Rios, Alexis',
                'Young, Chris B.','Young, Mike','Perry, Herb','Matthews, Gary',
                'Raines, Tim','Branyan, Russ','LeCroy, Matt','Restovich, Michael','Closser, J.D.',
                'K Griffey','W Pena','A Castillo','H Choi','G Matthews','S Alomar')

fixednames <- c('Hairston, Jerry','Mo Pena, Wily','Morales, Kendrys',
                'T. Castillo, Alberto','Nunez, Abraham','Upton, Melvin','Belliard, Ronnie',
                'Molina, Bengie','Johnson, Mark','Ross, David','Seop Choi, Hee',
                'Hocking, Denny','Cruz, Jose','DaVanon, Jeff','Cuddyer, Michael','Pena, Tony',
                'Wise, DeWayne','A. Gonzalez, Luis','Matsui, Kaz','Rios, Alex',
                'Young, Chris','Young, Michael','Perry, Herbert','Matthews Jr., Gary',
                'Raines Jr., Tim','Branyan, Russell','LeCroy, Matthew','Restovich, Mike','Closser, JD',
                'K Griffey Jr.','W Mo Pena','A T. Castillo','H Seop Choi','G Matthews Jr.','S Alomar Jr.')

namefixdf <- data_frame(Player=namestofix,NewName = fixednames)

# One-off adjustment for the other Luis Gonzalez in 2005

combined %<>% left_join(namefixdf,by="Player") %>% mutate(Player = ifelse(!is.na(NewName),NewName,Player)) %>%
  select(-NewName) %>% mutate(Player = ifelse(Team=="COL" & Player=="L Gonzalez","L A. Gonzalez",Player))

# Imports FG data downloaded from URL below then adds some variables for the merge (by name)
# http://www.fangraphs.com/leaders.aspx?pos=np&stats=bat&lg=all&qual=0&type=8&season=2009&month=0&season1=2000&ind=1&team=0&rost=0&age=0&filter=&players=0

fgdata <- read_csv("C:/Users/Frank/Documents/GitHub/fansreport/FanGraphs Database.csv")[,c(1:3,23)]
names(fgdata)[1] <- "Season"
fgdata %<>% mutate(Name = ifelse(playerid == 1860,"Luis A. Gonzalez",Name),firstname = word(Name),
                  commaname = paste0(substr(Name,nchar(firstname)+2,nchar(Name)),", ",firstname),
         Name2005 = paste0(substr(firstname,1,1)," ",substr(Name,nchar(firstname)+2,nchar(Name))))

# First pass at the merge

merged <- combined %>% left_join(fgdata %>% rename(FGTeam=Team) %>% select(-firstname),
                                 by=c("Season"="Season","Player" = "commaname"))

# Second pass at merge -- for players in the 2003 data because they played in 2002 that we haven't
# already matched, we merge on 2002 data

merged %>% mutate(Year02 = ifelse(is.na(playerid) & Season==2003,2002,2003)) %>% 
  left_join(fgdata %>% rename(FGTeam=Team) %>% filter(Season==2002) %>% select(-firstname),
            by=c("Year02"="Season","Player" = "commaname")) %>% 
  mutate(playerid = ifelse(!is.na(playerid.y),playerid.y,playerid.x),
         FGName = ifelse(!is.na(playerid.y),Name.y,Name.x),
         FGTeam = ifelse(!is.na(playerid.y),FGTeam.y,FGTeam.x)) %>% 
  select(-playerid.x,-playerid.y,-Year02,-Name.y,-Name.x,-FGTeam.x,-FGTeam.y) -> merged2

# Third pass at merge, taking care of 2005 (which is a separate key, due to the different naming conventions)

merged2 %>% left_join(fgdata %>% select(-firstname) %>% filter(Season==2005) %>% rename(FGTeam=Team),
                      by=c("Season","Player" = "Name2005")) %>% 
  mutate(playerid = ifelse(!is.na(playerid.y),playerid.y,playerid.x),
         FGName = ifelse(!is.na(playerid.y),Name,FGName),
         FGTeam = ifelse(!is.na(playerid.y),FGTeam.y,FGTeam.x)) %>% 
  select(-playerid.x,-playerid.y,-Name,-FGTeam.x,-FGTeam.y) -> merged3

# Creates dataframe of rows with multiple matches that need to be deleted
# For example, two Ramon Castros, or two Alex Gonzalezes
# These are more common in 2005 due to the naming system

teamdups <- c("FLO", NA, "CHN","FLO","BOS","SDN","ARI","ARI","BAL","KCA","PIT","NYN","ARI",
              "FLO","TBA","NYA","OAK","OAK","PHI","TBA","CIN","LAN","BAL","SEA","LAN","CIN",
              "PIT","FLO","SDN","KCA","CHN","TOR","CHA","PHI")
yeardups <- c(2004, 2003, 2004, 2004,2006,2008,2004,2006,2004,2004,2004,2006,2008,
              2005,2005,2005,2005,2005,2005,2005,2005,2005,2005,2005,2005,2005,
              2005,2005,2005,2005,2005,2005,2005,2005)
playeriddups <- c(7026, 520, 520,281,281,1727,1860,1860,1901,1420,531,2797,3196,
                  281,520,1818,1722,768,906,400,1296,463,3114,103,759,256,
                  2102,4022,634,1408,869,1216,965,891)
  
dupstoremove <- data_frame(Team = teamdups,Season = yeardups,playerid = playeriddups)

# This merges on and removes the duplicates (there's a one-off filter to save some typing)

merged3 %>% filter(Player != 'A Gonzalez' | FGName != 'Adrian Gonzalez') %>% 
  anti_join(dupstoremove,by=c("Team","Season","playerid")) %>% 
  select(-Name2005.x,-Name2005.y,-commaname) -> merged4

# Variable names in final output that might not be clear:
# # PositionSpec - average of tools relative to position average (missing in most sets)
# # GP - games played (often missing)

write_csv(merged4,"C:/Users/Frank/Documents/GitHub/fansreport/CombinedFSRData.csv")
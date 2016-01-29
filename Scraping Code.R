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

# In 2004, players that were traded got separate results; this collapses them together

tempsupp <- top %>% paste0("scoutResults_xxx.html") %>% 
  readHTMLTable(colClasses=c("character","character",rep("numeric",11)))

tempsupp[[1]] %>% group_by(Player) %>% 
  summarize(GP=sum(GP),`OverallPosition-Neutral` = weighted.mean(`OverallPosition-Neutral`,Ballots),
            Instincts=weighted.mean(Instincts,Ballots),FirstStep=weighted.mean(FirstStep,Ballots),
            Speed = weighted.mean(Speed,Ballots),Hands=weighted.mean(Hands,Ballots),
            Release=weighted.mean(Release,Ballots),Strength=weighted.mean(Strength,Ballots),
            Accuracy = weighted.mean(Accuracy,Ballots),AgreementLevel = weighted.mean(AgreementLevel,Ballots),
            Ballots=sum(Ballots)) %>% ungroup() %>%
  mutate(Team = "---",Season = 2004) -> supp04

supp04 %<>% rename(Agreement=AgreementLevel,Overall = `OverallPosition-Neutral`) %>% 
  mutate(Age=NA,Pos=NA)

cl <- makeCluster(2)
registerDoParallel(cl)

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

data05 %<>% rename(Agreement=AgreementLevel,Overall = `OverallPosition-Neutral`) %>% 
  mutate(Age=NA,Pos=NA,GP=NA)

scrape0608 <- function(team,year) {
  tempURL <- top %>% paste0("scoutResults",year,"_",team,".html")
  
  if (year!=2008) {
    cc <- c("character","character",rep("numeric",10))
  } else {
    cc <- c("character","character",rep("numeric",11))
  }
  
  tempURL %>% readHTMLTable(colClasses = cc) -> templist
  
  # For 2006-2008, there's a unique/persistent player ID that can be pulled out of the links

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

data0608 <- foreach(n = 1:nrow(loopframe),.combine=rbind) %dopar% {
  library(dplyr)
  require(XML)
  library(rvest)
  library(stringr)
  library(magrittr)
  scrape0608(loopframe$Team[n],loopframe$Season[n])
}

data0608 %<>% rename(Agreement=AgreementLevel,Overall = `OverallPosition-Neutral`) %>% 
  mutate(Age=NA,GP=NA)

# Pulls 2003 data, which is fairly different form

test03 <- readHTMLTable("http://www.tangotiger.net/scouting/scout_Results.html")[[6]]
test03 %<>% filter(Player != "Player") %>% select(-UZR162,-FanRuns)
for (i in 2:ncol(test03)) {
  test03[,i] <- as.numeric(test03[,i])
}
names(test03)[10:12] <- c("Agreement","Ballots","GP")

poshelp <- data_frame(Pos = 2:9,CharPos = c("C","1B","2B",'3B','SS','LF','CF','RF'))

test03 %>% inner_join(poshelp,by="Pos") %>% select(-Pos) %>% rename(Pos=CharPos,FirstStep=Acceleration) %>%
  mutate(Season=2003,Team=NA,Age=NA,Overall=NA) -> data03

pre06 <- rbind_list(data03,data04,supp04,data05) %>% mutate(PositionSpec=NA,tangoid=NA)
data0608 %>% rename(PositionSpec = `Position-Specific`) %>% rbind_list(pre06) -> combined

# Variable names in combined that might not be clear:
# # tangoid - unique id; only applicable from 2006-8
# # PositionSpec - average of tools relative to position average (missing in most sets)
# # GP - games player (often missing)

write_csv(combined,"C:/Users/Frank/Documents/GitHub/fansreport/CombinedFSRData.csv")
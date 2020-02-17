#2019 Nationals Stats Questions

library(tidyverse)
library(purrr)
library(rvest)
library(readr)

#Teams pages that have statistics recorded (those that don't: Texas, Virginia)
  #BYU and CU were calculated on another project - copying over.

Pittweb <- 'https://www.wcla.club/stats/team_instance/4134466?subseason=569703&tab=team_instance_player_stats&tool=3453565'
Washingtonweb <- 'https://www.wcla.club/stats/team_instance/4134300?subseason=569703&tab=team_instance_player_stats&tool=3453565'
Northeasternweb <- 'https://www.wcla.club/stats/team_instance/4134249?subseason=569703&tab=team_instance_player_stats&tool=3453565'
CalPolyweb <- 'https://www.wcla.club/stats/team_instance/4134526?subseason=569703&tab=team_instance_player_stats&tool=3453565'
Michiganweb <- 'https://www.wcla.club/stats/team_instance/4134478?subseason=569703&tab=team_instance_player_stats&tool=3453565'
SDSUweb <- 'https://www.wcla.club/stats/team_instance/4134570?subseason=569703&tab=team_instance_player_stats&tool=3453565'
BCweb <- 'https://www.wcla.club/stats/team_instance/4134214?subseason=569703&tab=team_instance_player_stats&tool=3453565'
UCLAweb <- 'https://www.wcla.club/stats/team_instance/4134530?subseason=569703&tab=team_instance_player_stats&tool=3453565'
UDelweb <- 'https://www.wcla.club/stats/team_instance/4134172?subseason=569703&tab=team_instance_player_stats&tool=3453565'
Georgiaweb <- "https://www.wcla.club/stats/team_instance/4134375?subseason=569703&tab=team_instance_player_stats&tool=3453565"
SClaraweb <- 'https://www.wcla.club/stats/team_instance/4134544?subseason=569703&tab=team_instance_player_stats&tool=3453565'
VTweb <- 'https://www.wcla.club/stats/team_instance/4134179?subseason=569703&tab=team_instance_player_stats&tool=3453565'
CUweb <- 'https://www.wcla.club/stats/team_instance/4134318?subseason=569703&tab=team_instance_player_stats&tool=3453565'
BYUweb <- 'https://www.wcla.club/stats/team_instance/4134315?subseason=569703&tab=team_instance_player_stats&tool=3453565'


#Function to turn into data frame
TeamData <- function(school){
  school.table <- read_html(school)
  schooltbls <- html_nodes(school.table, "table")
  school19 <- school.table %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  school2019 <- data.frame(school19)
}

#Adding in Washington data frame even though they don't have position or school year- possible summation questions
Washington.stats <- data.frame(TeamData(Washingtonweb))


#Add in positions
Pittroster <- 'https://www.wcla.club/roster/show/4631060?subseason=569703'
Washingtonroster <- #No roster positions or school years for Washington, excluding from this analysis
Northeasternroster <- 'https://www.wcla.club/roster/show/4630921?subseason=569703'
CalPolyroster <- 'https://www.wcla.club/roster/show/4631110?subseason=569703'
Michiganroster <- 'https://www.wcla.club/roster/show/4631069?subseason=569703'
SDSUroster <- 'https://www.wcla.club/roster/show/4631142?subseason=569703'
BCroster <- 'https://www.wcla.club/roster/show/4630880?subseason=569703'
UCLAroster <- 'https://www.wcla.club/roster/show/4631113?subseason=569703'
UDelroster <- 'https://www.wcla.club/roster/show/4630861?subseason=569703'
Georgiaroster <- 'https://www.wcla.club/roster/show/4631006?subseason=569703'
SClararoster <- 'https://www.wcla.club/roster/show/4631126?subseason=569703'
VTroster <- 'https://www.wcla.club/roster/show/4630866?subseason=569703'
CUroster <- 'https://www.wcla.club/roster/show/4630961?subseason=569703'
BYUroster <- 'https://www.wcla.club/roster/show/4630955?subseason=569703'

#Function to create Positions data set
Positions <- function(position){
  positions.table <- read_html(position)
  postables <- html_nodes(positions.table, "table")
  roster <- positions.table %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  Ros <- data.frame(roster)
  Ros <- Ros[,-c(1,2)]
}

##Separate function for BYU years
BYUyears <- function(position){
  positions.table <- read_html(position)
  postables <- html_nodes(positions.table, "table")
  roster <- positions.table %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  Ros <- data.frame(roster)
  Ros <- Ros[,-c(1,2,4)]
}


#Try merging two different functions
Pitt <- merge(data.frame(TeamData(Pittweb)), Positions(Pittroster), by.x = "Name", by.y = "Name")
Northeastern <- merge(data.frame(TeamData(Northeasternweb)), Positions(Northeasternroster), by.x = "Name", by.y = "Name")
CalPoly <- merge(data.frame(TeamData(CalPolyweb)), Positions(CalPolyroster), by.x = "Name", by.y = "Name")
Michigan <- merge(data.frame(TeamData(Michiganweb)), Positions(Michiganroster), by.x = "Name", by.y = "Name")
SDSU <- merge(data.frame(TeamData(SDSUweb)), Positions(SDSUroster), by.x = "Name", by.y = "Name")
BC <- merge(data.frame(TeamData(BCweb)), Positions(BCroster), by.x = "Name", by.y = "Name")
UCLA <- merge(data.frame(TeamData(UCLAweb)), Positions(UCLAroster), by.x = "Name", by.y = "Name")
Delaware <- merge(data.frame(TeamData(UDelweb)), Positions(UDelroster), by.x = "Name", by.y = "Name")
Georgia <- merge(data.frame(TeamData(Georgiaweb)), Positions(Georgiaroster), by.x = "Name", by.y = "Name")
SClara <- merge(data.frame(TeamData(SClaraweb)), Positions(SClararoster), by.x = "Name", by.y = "Name")
VT <- merge(data.frame(TeamData(VTweb)), Positions(VTroster), by.x = "Name", by.y = "Name")
CU <- merge(data.frame(TeamData(CUweb)), Positions(CUroster), by.x = "Name", by.y = "Name")

#BYU separate, writing down known positions
BYU <- merge(data.frame(TeamData(BYUweb)), Positions(BYUroster), by.x = "Name", by.y = "Name")
BYU$Pos. <- c("A", "D", "A", "M", "D", "M", "M",
              "M", "D", "M", "M", "A","M", "M", "A", "A",
              "M", "D", "A", "A", "D", "M")

#Add in school for Nationals analysis
Pitt$School <- "Pitt"
Northeastern$School <- "Northeastern"
CalPoly$School <- "CalPoly"
Michigan$School <- "Michigan"
SDSU$School <- "SDSU"
BC$School <- "BC"
UCLA$School <- "UCLA"
Delaware$School <- "Delaware"
Georgia$School <- "Georgia"
SClara$School <- "SClara"
VT$School <- "VT"
CU$School <- "CU"
BYU$School <- "BYU"

#Reorganize columns function
reorganize <- function(school){
  school[, c(1,2,12,11,10,9,2,3,4,5,6,7,8)]
}
Pitt <- reorganize(Pitt)
Northeastern <- reorganize(Northeastern)
CalPoly <- reorganize(CalPoly)
Michigan <- reorganize(Michigan)
SDSU <- reorganize(SDSU)
BC <- reorganize(BC)
UCLA <- reorganize(UCLA)
Delaware <- reorganize(Delaware)
Georgia <- reorganize(Georgia)
SClara <- reorganize(SClara)
VT <- reorganize(VT)
CU <- reorganize(CU)
BYU <- reorganize(BYU)

#Create CSV's for all data sets as permanent (just in case)
write_csv(Pitt, "Pitt2019.csv")
write_csv(Northeastern, "Northeastern2019,csv")
write_csv(CalPoly, "CalPoly2019.csv")
write_csv(Michigan, "Michigan2019.csv")
write_csv(SDSU, "SDSU2019.csv")
write_csv(BC, "BC2019.csv")
write_csv(UCLA, "UCLA2019.csv")
write_csv(Delaware, "Delaware2019.csv")
write_csv(Georgia, "Georgia2019.csv")
write_csv(SClara, "SClara2019.csv")
write_csv(VT, "VT2019.csv")
write_csv(CU, "CU19.csv")
write_csv(BYU, "BYU19.csv")


#Combine to do nationals analysis
Nationals2019 <- rbind(Pitt, Northeastern, CalPoly, Michigan, SDSU, BC, UCLA, Delaware, Georgia, SClara, VT, CU, BYU)



#Which positions scored the most goals amongst the national teams?

#Remove teams with no positions
N2019positions <- Nationals2019 %>%
  drop_na()

#Removing Northeastern because positions are empty
N2019positions2 <- N2019positions[!(N2019positions$School == "Northeastern"),]
N2019positions2 <- N2019positions2 %>%
  #Removing Defense and Goalies as, even though defenders do score, they don't score enough to worry about their effect.
  filter(Pos. != "D") %>%
  filter(Pos. != "G")

#Graph
ggplot(data = N2019positions2) +
  aes(x = School, y = A, fill = Pos.) +
  geom_bar(stat = "identity", position = "dodge")



#BYU is 1 of 2 schools (SDSU) that their midfield scored more goals than their offense. Which of these teams had more midfielders that scored?
SDSUcheck <- SDSU %>%
  filter(G > 0 & Pos. == "M")

BYUcheck <- BYU %>%
  filter(G > 0 & Pos. == "M")


##11/5/19
#What schools scored the most goals?
goaltotals <- function(school) {
  sum(school$G)
}

VTgoals <- data.frame(goaltotals(VT))
BCgoals <- data.frame(goaltotals(BC))
BYUgoals <- data.frame(goaltotals(BYU))
CalPolygoals <- data.frame(goaltotals(CalPoly))
Delgoals <- data.frame(goaltotals(Delaware))
Georgiagoals <- data.frame(goaltotals(Georgia))
Michigangoals <- data.frame(goaltotals(Michigan))
Northeasterngoals <- data.frame(goaltotals(Northeastern))
Pittgoals <- data.frame(goaltotals(Pitt))
SClaragoals <- data.frame(goaltotals(SClara))
SDSUgoals <- data.frame(goaltotals(SDSU))
UCLAgoals <- data.frame(goaltotals(UCLA))
Washingtongoals <- data.frame(goaltotals(Washington.stats))
CUgoals <- data.frame(goaltotals(CU))

#Combine to make a nationals goal totals
NationalsGoalTotals <- rbind(VTgoals,BCgoals,BYUgoals,CalPolygoals,Delgoals,Georgiagoals,Michigangoals,Northeasterngoals,Pittgoals,SClaragoals,SDSUgoals,UCLAgoals,Washingtongoals,CUgoals)

 
#Which nationals teams have the highest turnover of seniors?

#Change abbreviations to full words



#How many seniors competed at nationals?

##Change school year abbreviations to match

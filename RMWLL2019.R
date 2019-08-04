install.packages('rvest')
library(rvest)


#RMWLL
BYUweb <- 'https://www.wcla.club/stats/team_instance/4134315?subseason=569703&tab=team_instance_player_stats&tool=3453565'
CUweb <- 'https://www.wcla.club/stats/team_instance/4134318?subseason=569703&tab=team_instance_player_stats&tool=3453565'
CSUweb <- 'https://www.wcla.club/stats/team_instance/4134316?subseason=569703&tab=team_instance_player_stats&tool=3453565'
Utahweb <- 'https://www.wcla.club/stats/team_instance/4134324?subseason=569703&tab=team_instance_player_stats&tool=3453565'

#Function to grab team stats
TeamData <- function(school){
  school.table <- read_html(school)
  schooltbls <- html_nodes(school.table, "table")
  school19 <- school.table %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  school2019 <- data.frame(school19)
}

#RMWLL player stats
Utah.stats <- data.frame(TeamData(Utahweb))
BYU.stats <- data.frame(TeamData(BYUweb))
CSU.stats <- data.frame(TeamData(CSUweb))
CU.stats <- data.frame(TeamData(CUweb))

#Which team had the highest average DC per game?
Utahdc <- mean(Utah.stats$DC)
BYUdc <- mean(BYU.stats$DC)
CUdc <- mean(CU.stats$DC)
CSUdc <- mean(CSU.stats$DC)

Utahgoals <- mean(Utah.stats$G)
BYUgoals <- mean(BYU.stats$G)
CUgoals <- mean(CU.stats$G)
CSUgoals <- mean(CSU.stats$G)


UtahDC <- Utah.stats$DC / Utah.stats$GP
Utah.stats$DCgame <- UtahDC


BYUDC <- BYU.stats$DC / BYU.stats$GP
BYU.stats$DCgame <- BYUDC 

CUDC <- CU.stats$DC / CU.stats$GP
CU.stats$DCgame <- CUDC

CSUDC <- CSU.stats$DC / CSU.stats$GP
CSU.stats$DCgame <- CSUDC



#Function to grab player positions
Utahroster <- 'https://www.wcla.club/roster/show/4630963?subseason=569703'


Positions <- function(position){
  positions.table <- read_html(position)
  postables <- html_nodes(positions.table, "table")
  roster <- positions.table %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  Ros <- data.frame(roster)
  Ros <- Ros[,-c(1,2,5)]
  
}

#Final Data sets w/ positions
Utah <- merge(Utah.stats, Positions(Utahroster), by.x = "Name", by.y="Name")
Utah <- Utah[,c(1,2,10,3,4,5,6,7,8,9)]
names(Utah)[names(Utah)=="X."] <- 'Number'

BYU <- merge(BYU.stats, Positions(BYUroster), by.x = "Name", by.y="Name")
BYU <- BYU[,c(1,2,10,3,4,5,6,7,8,9)]
names(BYU)[names(BYU)=="X."] <- 'Number'

CSU <- merge(CSU.stats, Positions(CSUroster), by.x = "Name", by.y="Name")
CSU <- CSU[,c(1,2,10,3,4,5,6,7,8,9)]
names(CSU)[names(CSU)=="X."] <- 'Number'

CU <- merge(CU.stats, Positions(CUroster), by.x = "Name", by.y="Name")
CU <- CU[,c(1,2,10,3,4,5,6,7,8,9)]
names(CU)[names(CU)=="X."] <- 'Number'
#Adding PPG as a column
PPG <- BYUtest$PTS / BYUtest$GP
BYUtest <- cbind(BYUtest, PPG)
max(BYUtest$PPG)

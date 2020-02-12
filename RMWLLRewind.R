#Goal Patterns over the last few years

library(tidyverse)
library(reshape2)
library(purrr)
library(rvest)
library(readr)
library(ggrepel)

#Data tables
BYUweb <- 'https://www.wcla.club/stats/team_instance/4134315?subseason=569703&tab=team_instance_player_stats&tool=3453565'
CUweb <- 'https://www.wcla.club/stats/team_instance/4134318?subseason=569703&tab=team_instance_player_stats&tool=3453565'
CSUweb <- 'https://www.wcla.club/stats/team_instance/4134316?subseason=569703&tab=team_instance_player_stats&tool=3453565'
Utahweb <- 'https://www.wcla.club/stats/team_instance/4134324?subseason=569703&tab=team_instance_player_stats&tool=3453565'
USUweb <- 'https://www.wcla.club/stats/team_instance/4257380?subseason=569703&tab=team_instance_player_stats&tool=3453565'


BYU2018 <-"https://www.wcla.club/stats/team_instance/3062597?subseason=444209&tab=team_instance_player_stats&tool=2727872"
CU2018 <- "https://www.wcla.club/stats/team_instance/3062598?subseason=444209&tab=team_instance_player_stats&tool=2727872"
CSU2018 <- "https://www.wcla.club/stats/team_instance/3062599?subseason=444209&tab=team_instance_player_stats&tool=2727872"
Utah2018 <- "https://www.wcla.club/stats/team_instance/3062875?subseason=444218&tab=team_instance_player_stats&tool=2727922"
USU2018 <- "https://www.wcla.club/stats/team_instance/3062876?subseason=444218&tab=team_instance_player_stats&tool=2727922"

BYU2017 <- "https://www.wcla.club/stats/team_instance/2254703?subseason=340731&tab=team_instance_player_stats&tool=2134437"
Utah2017 <- "https://www.wcla.club/stats/team_instance/2256550?subseason=340994&tab=team_instance_player_stats&tool=2136007"
USU2017 <- "https://www.wcla.club/stats/team_instance/2256551?subseason=340994&tab=team_instance_player_stats&tool=2136007"
CU2017 <- "https://www.wcla.club/stats/team_instance/2254704?subseason=340731&tab=team_instance_player_stats&tool=2134437"
CSU2017 <-"https://www.wcla.club/stats/team_instance/2254705?subseason=340731&tab=team_instance_player_stats&tool=2134437"


BYU2016 <- "https://www.wcla.club/stats/team_instance/1554243?subseason=255831&tab=team_instance_player_stats&tool=1637934"
CU2016 <- "https://www.wcla.club/stats/team_instance/1554244?subseason=255831&tab=team_instance_player_stats&tool=1637934"
CSU2016 <- "https://www.wcla.club/stats/team_instance/1554245?subseason=255831&tab=team_instance_player_stats&tool=1637934"

#Function to scrape data tables
oldseasons <- function(school){
  school.table <- read_html(school)
  schooltbls <- html_nodes(school.table, "table")
  school19 <- school.table %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  school2019 <- data.frame(school19)
}

#BYU
BYU19 <- data.frame(oldseasons(BYUweb))
BYU18 <- data.frame(oldseasons(BYU2018))
BYU17 <- data.frame(oldseasons(BYU2017))
BYU16 <- data.frame(oldseasons(BYU2016))

#Utah
Utah19 <- data.frame(oldseasons(Utahweb))
Utah18 <- data.frame(oldseasons(Utah2018))
Utah17 <- data.frame(oldseasons(Utah2017))

#CU
CU19 <- data.frame(oldseasons(CUweb))
CU18 <- data.frame(oldseasons(CU2018))
CU17 <- data.frame(oldseasons(CU2017))
CU16 <- data.frame(oldseasons(CU2016))

#CSU
CSU19 <- data.frame(oldseasons(CSUweb))
CSU18 <- data.frame(oldseasons(CSU2018))
CSU17 <- data.frame(oldseasons(CSU2017))
CSU16 <- data.frame(oldseasons(CSU2016))

#USU
USU19 <- data.frame(oldseasons(USUweb))
USU18 <- data.frame(oldseasons(USU2018))
USU17 <- data.frame(oldseasons(USU2017))


#Sums of goals
BYUg2019 <- sum(BYU19$G)
BYUg2018 <- sum(BYU18$G)
BYUg2017 <- sum(BYU17$G)
BYUg2016 <- sum(BYU16$G)

BYUgs <- data.frame(
  "Goals" = rbind(BYUg2019, BYUg2018, BYUg2017, BYUg2016),
  "date" = c("2019", "2018", "2017", "2016"),
  "school" = "BYU"
)

CUg2019 <- sum(CU19$G)
CUg2018 <- sum(CU18$G)
CUg2017 <- sum(CU17$G)
CUg2016 <- sum(CU16$G)

CUgs <- data.frame(
  "Goals" = rbind(CUg2019, CUg2018, CUg2017, CUg2016),
  "date" = c("2019", "2018", "2017", "2016"),
  "school" = "CU"
)

CSUg2019 <- sum(CSU19$G)
CSUg2018 <- sum(CSU18$G)
CSUg2017 <- sum(CSU17$G)
CSUg2016 <- sum(CSU16$G)

CSUgs <- data.frame(
  "Goals" = rbind(CSUg2019, CSUg2018, CSUg2017, CSUg2016),
  "date" = c("2019", "2018", "2017", "2016"),
  "school" = "CSU"
)

Utahg2019 <- sum(Utah19$G)
Utahg2018 <- sum(Utah18$G)
Utahg2017 <- sum(Utah17$G)

Utahgs <- data.frame(
  "Goals" = rbind(Utahg2019, Utahg2018, Utahg2017),
  "date" = c("2019", "2018", "2017"),
  "school" = "Utah"
)

USUg2019 <- sum(USU19$G)
USUg2018 <- sum(USU18$G)
USUg2017 <- sum(USU17$G)

USUgs <- data.frame(
  "Goals" = rbind(USUg2019, USUg2018, USUg2017),
  "date" = c("2019", "2018", "2017"),
  "school" = "USU"
)

#Try to mend everything together 
fouryears <- rbind(BYUgs, CUgs, CSUgs)
fouryears$Year <- as.numeric(as.character(fouryears$date))


#Four years
ggplot(data = fouryears,
  aes(x = Year, y = Goals, color = school)) +
  geom_line() +
  geom_label_repel(show.legend = FALSE, aes(label = Goals),
                   box.padding   = 0.35, 
                   point.padding = 0,
                   segment.color = 'grey50') +
  theme_linedraw() +
  ggtitle("4-Year RMWLL Goal Totals")

#3 years - all in
threeyears <- rbind(BYUgs, CUgs, CSUgs, USUgs, Utahgs)
threeyears$date <- as.numeric(as.character(threeyears$date))

ggplot(data = threeyears,
       aes(x = date, y = goals, color = school)) +
    geom_line() +
  geom_label_repel(show.legend = FALSE, aes(label = goals),
                   box.padding   = 0.35, 
                   point.padding = 0,
                   segment.color = 'grey50') +
  theme_linedraw()


# Goals per game to take into account nationals appearances, more scheduled games, etc.
byu2019gpg <- sum(BYUg2019) / BYU19$GP
byu2018gpg <- sum(BYUg2018) / BYU18$GP
byu2017gpg <- sum(BYUg2017) / BYU17$GP
byu2016gpg <- sum(BYUg2016) / BYU16$GP

byugpg <- data.frame(
  'date' = c("2019", "2018", "2017", "2016"),
  "goals" = rbind(byu2019gpg, byu2018gpg, byu2017gpg, byu2016gpg),
  "school" = "BYU"
)

byugpg <- byugpg[,c(1:2)]
byugpg$school <- "BYU"

cu2019gpg <- sum(CUg2019) / CU19$GP
cu2018gpg <- sum(CUg2018) / CU18$GP
cu2017gpg <- sum(CUg2017) / CU17$GP
cu2016gpg <- sum(CUg2016) / CU16$GP

cugpg <- data.frame(
  'date' = c("2019", "2018", "2017", "2016"),
  "goals" = rbind(cu2019gpg, cu2018gpg, cu2017gpg, cu2016gpg),
  "school" = "CU"
)

cugpg <- cugpg[,c(1:2)]
cugpg$school <- "CU"

csu2019gpg <- sum(CSUg2019) / CSU19$GP
csu2018gpg <- sum(CSUg2018) / CSU18$GP
csu2017gpg <- sum(CSUg2017) / CSU17$GP
csu2016gpg <- sum(CSUg2016) / CSU16$GP

csugpg <- data.frame(
  'date' = c("2019", "2018", "2017", "2016"),
  "goals" = rbind(csu2019gpg, csu2018gpg, csu2017gpg,csu2016gpg),
  "school" = "CSU"
)

csugpg <- csugpg[,c(1:2)]
csugpg$school <- "CSU"

utah2019gpg <- sum(Utahg2019) / Utah19$GP
utah2018gpg <- sum(Utahg2018) / Utah18$GP
utah2017gpg <- sum(Utahg2017) / Utah17$GP

utahgpg <- data.frame(
  'date' = c("2019", "2018", "2017"),
  "goals" = rbind(utah2019gpg,utah2018gpg,utah2017gpg),
  "school" = "Utah"
)

utahgpg <- utahgpg[,c(1:2)]
utahgpg$school <- "Utah"

usu2019gpg <- sum(USUg2019) / USU19$GP
usu2018gpg <- sum(USUg2018) / USU18$GP
usu2017gpg <- sum(USUg2017) / USU17$GP

usugpg <- data.frame(
  'date' = c("2019", "2018", "2017"),
  "goals" = rbind(usu2019gpg, usu2018gpg, usu2017gpg),
)

usugpg <- usugpg[,c(1:2)]
usugpg$school <- "USU"

#Four years gpg
fouryearsgpg <- rbind(byugpg, cugpg, csugpg)
names(fouryearsgpg) <- c("date", "gpg", "school")
fouryearsgpg$date <- as.numeric(as.character(fouryearsgpg$date))

ggplot(data = fouryearsgpg,
       aes(x = date, y = gpg, color = school)) +
  geom_line()

#Three years gpg
threeyearsgpg <- rbind(byugpg, cugpg, csugpg, usugpg, utahgpg)
names(threeyearsgpg) <- c("date", "gpg", "school")
threeyearsgpg$date <- as.numeric(as.character(threeyearsgpg$date))

ggplot(data = threeyearsgpg,
       aes(x = date, y = gpg, color = school)) +
  geom_line()

#GOALS AGAINST PER GAME
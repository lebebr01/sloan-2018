# Packages
library(readr)
library(dplyr)
library(XLConnect)
library(readxl)

#### Read in Data ####
yby <- read_csv("data/year_by_year_to2015.csv", na = 'N/A')
coaches <- read_csv("data/coaches_to2015.csv") %>%
  select(Team,Year,Coach)

# earliestgame <- yby %>% group_by(Team) %>% summarize(earliest=min(Year))

#### Joining files ####
yby_coach <- yby %>% left_join(coaches,by=c("Team","Year"))

## Obtaining names of coaches to include in dataset
CoachNames05to15 <- unique(coaches[coaches$Year %in% c(2005:2015),]$Coach)

## Keeping only games were both coaches are included in the above list of names
yby_coach_05to15 <- yby_coach %>%
  filter(Coach %in% CoachNames05to15) %>%
  arrange(Team,Year)

#### Export file to excel ####
# ybycxl <- loadWorkbook ("year_by_year_to2015_withcoaches_export.xlsx", create = TRUE)
# createSheet(ybycxl,"yby_coach_05to15")
# writeWorksheet(ybycxl,yby_coach_05to15,"yby_coach_05to15")
# saveWorkbook(ybycxl)

## Team-Years to look for possible duplicates
MultipleCoaches <- read_csv("data/coaches_to2015.csv") %>%
  filter(Coach %in% CoachNames05to15) %>%
  group_by(Team,Year) %>% mutate(n=n()) %>%
  filter(n>1)

#### Reading new data and adding opponent coach ####
yby_coach_new <- read_excel("data/year_by_year_to2015_withcoaches_interim.xlsx")

## Checking for any remaining duplicate games
dupgames <- yby_coach_new %>% group_by(Team,Year,Date) %>%
  mutate(n=n()) %>% filter(n>1)

## Adding Opponent coach from original coach data
opponent1 <- yby_coach_new %>% select(Opponent=Team,Year,Date,Opponent_Coach1=Coach)
yby_coach_withOPcoach <- yby_coach_new %>%
  left_join(opponent1,by=c("Opponent","Year","Date"))

## Checking for any remaining duplicate games
dupgamesOPC1 <- yby_coach_withOPcoach %>% group_by(Team,Year,Date) %>%
  mutate(n=n()) %>% filter(n>1)

## Getting Opponent_Coach for games that currently have NA in the variable
opponent2 <- coaches %>% select(Opponent=Team,Year,Opponent_Coach2=Coach)
OPCNA <- yby_coach_withOPcoach %>% filter(is.na(Opponent_Coach1)) %>%
  left_join(opponent2,by=c("Opponent","Year"))
# write.csv(OPCNA,"OPCNA.csv")

## Team-Years to look for possible duplicates to remove
MultipleOppCoach<- OPCNA %>% group_by(Team,Year,Date) %>%
  mutate(n=n()) %>% filter(n>1)

## Reading in data after duplicates have been removed
# dropping Date column because I'm having trouble converting it to character to match the other data
OPCNA_new <- read_excel("OPCNA.xlsx",na="NA") %>% select(-Date)

# ## Checking for any remaining duplicate games
# dupgamesOPC2<- OPCNA_new %>% group_by(Team,Year,Date) %>%
#   mutate(n=n()) %>% filter(n>1)

## Adding OPC2 column and coalescing with OPC1
yby_coach_withOPcoach <- yby_coach_withOPcoach %>% left_join(OPCNA_new) %>%
  mutate(Opponent_Coach = coalesce(Opponent_Coach1,Opponent_Coach2)) %>%
  select(-Opponent_Coach1,-Opponent_Coach2)


#### Export file to excel ####
ybywithOPxl <- loadWorkbook ("year_by_year_to2015_withcoaches.xlsx", create = TRUE)
createSheet(ybywithOPxl,"yby_coach_05to15")
writeWorksheet(ybywithOPxl,yby_coach_withOPcoach,"yby_coach_05to15")
saveWorkbook(ybywithOPxl)

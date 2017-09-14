# Packages
library(readr)
library(dplyr)
library(XLConnect)
library(readxl)

#### Read in Data ####
yby <- read_csv("data/year_by_year_to2015.csv", na = 'N/A')
coaches <- read_csv("data/coaches_to2015.csv") %>%
  select(Team,Year,Coach)

#### Joining files ####
yby_coach <- yby %>% left_join(coaches,by=c("Team","Year"))

## Obtaining names of coaches to include in dataset
CoachNames05to15 <- unique(coaches[coaches$Year %in% c(2005:2015),]$Coach)

## Keeping only games were both coaches are included in the above list of names
yby_coach_05to15 <- yby_coach %>%
  filter(Coach %in% CoachNames05to15) %>%
  arrange(Team,Year)

#### Export file to excel ####
ybycxl <- loadWorkbook ("year_by_year_to2015_withcoaches_export.xlsx", create = TRUE)
createSheet(ybycxl,"yby_coach_05to15")
writeWorksheet(ybycxl,yby_coach_05to15,"yby_coach_05to15")
saveWorkbook(ybycxl)

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

## Adding Opponent coach
opponent <- yby_coach_new %>% select(Opponent=Team,Year,Date,Opponent_Coach=Coach)
yby_coach_withOPcoach <- yby_coach_new %>%
  left_join(opponent,by=c("Opponent","Year","Date")) %>%
  filter(!is.na(Opponent_Coach))

#### Export file to excel ####
ybywithOPxl <- loadWorkbook ("year_by_year_to2015_withcoaches.xlsx", create = TRUE)
createSheet(ybywithOPxl,"yby_coach_05to15")
writeWorksheet(ybywithOPxl,yby_coach_withOPcoach,"yby_coach_05to15")
saveWorkbook(ybywithOPxl)

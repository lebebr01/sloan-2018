library(readr)
library(dplyr)

## Only 2005 to present game data
yby05 <- read_csv("data/year_by_year_to2015.csv") %>% filter(Year>=2005)

## List of all locations (sites) and most common for each team (homes)
sites <- yby05 %>% count(Team,Location) %>%
  group_by(Team) %>% mutate(max=max(n))
homes <- sites %>% filter(n==max)

## Creating Home-Away-Neutral Designation
sites <- sites %>% mutate(HAN = ifelse(n==max,"Home",ifelse(Location %in% homes$Location,"Away","Neutral")))

## Adding back to game data
yby05 <- left_join(yby05,sites[c("Team","Location","HAN")],by=c("Team","Location"))

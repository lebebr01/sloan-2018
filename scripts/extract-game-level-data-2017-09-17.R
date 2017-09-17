##################################################
### Load libraries
##################################################

library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(PlayerRatings)



##################################################
### Read in data
##################################################

yby = read_excel("data/year_by_year_to2015_withcoaches.xlsx")
yby2 = read_excel("data/year_by_year_to2015_withcoaches_interim.xlsx")
coaches = read_csv("data/coaches_to2015.csv")



##################################################
### Create ELO-ready data
##################################################

# Access winners
winners = yby %>%
  filter(WL == "W") %>%
  select(date = Date, coach1 = Coach, coach2 = Opponent_Coach) %>%
  mutate(outcome = 1)

# Access losers
losers = yby %>%
  filter(WL == "L") %>%
  select(date = Date, coach1 = Coach, coach2 = Opponent_Coach) %>%
  mutate(outcome = 0)

ties = yby %>%
  filter(WL == "T") %>%
  select(date = Date, coach1 = Coach, coach2 = Opponent_Coach) %>%
  mutate(outcome = 0.5)


all = rbind(winners, losers, ties)



##################################################
### Get only unique games
##################################################

all$new = NA

for(i in 1:nrow(all)){
  all$new[i] = base::paste(sort(c(all$coach1[i], all$coach2[i], all$date[i])), collapse = "_")
}

cr2 = all %>% 
  distinct(new, .keep_all = TRUE) %>% 
  mutate(
    date = mdy(date),
    date2 = as.numeric(date),
    year = year(date)
  ) %>%
  arrange(date)


cr2 = cr2[complete.cases(cr2), ] %>% 
  filter(coach != "Deleted") %>% 
  filter(coach2 != "Deleted")  

cr2





# Create num_games variable for coaches
coaches = coaches %>%
  group_by(Team, Year, Coach) %>%
  mutate(num_games = sum(Win, Loss, Tie))

# filter by year, and ensure one row per year, team, coach, combo 
# join coaches and year by year  ----
game_results = yby %>%
  select(Date, Year, Team, Opponent, PF, PA) 


table(yby$WL)





# Join winners and losers
coach_results = game_results %>% 
  left_join(winners, by = c("Year", "Team", "Opponent")) %>%
  left_join(losers, by = c("Year", "Team", "Opponent"))






##################################################
### Write data to file
##################################################

write.csv(cr2, file = "data/elo-game-data-2017-09-08.csv", row.names = FALSE)

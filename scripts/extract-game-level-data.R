## ----setup, message = FALSE, warning = FALSE, error = FALSE--------------
# Read in data
library(readr)
library(dplyr)

# Read in Data ----
yby = read_csv("data/year_by_year_to2015.csv") %>% filter(Year >= 2005)
coaches = read_csv("data/coaches_to2015.csv") %>% filter(Year >= 2005)


#################################
## Add home, neutral, away
#################################


## List of all locations (sites) and most common for each team (homes)
sites = yby %>% 
  count(Team, Location) %>%
  group_by(Team) %>% mutate(max = max(n))

homes = sites %>% filter(n == max)

## Creating Home-Away-Neutral Designation
sites = sites %>% 
  mutate(HAN = ifelse(n == max, "Home", ifelse(Location %in% homes$Location, "Away", "Neutral")))

## Adding back to game data
yby = left_join(yby, sites[c("Team", "Location", "HAN")], by = c("Team", "Location"))



#################################
## Create ELO-ready data
#################################

# Create num_games variable for coaches
coaches = coaches %>%
  group_by(Team, Year, Coach) %>%
  mutate(num_games = sum(Win, Loss, Tie))

# filter by year, and ensure one row per year, team, coach, combo 
# join coaches and year by year  ----
game_results = yby %>%
  select(Date, Year, Team, Opponent, PF, PA, HAN) 


winners = game_results %>%
  left_join(coaches, by = c("Year", "Team")) %>%
  select(Team, Year, Opponent, Coach, han_winner = HAN)

losers = game_results %>%
  select(Team2 = Team, Team = Opponent, Year) %>%
  left_join(coaches, by = c("Year", "Team")) %>%
  select(Opponent = Team, Year, coach2 = Coach, Team = Team2)

coach_results = game_results %>% 
  left_join(winners, by = c("Year", "Team", "Opponent")) %>%
  left_join(losers, by = c("Year", "Team", "Opponent"))


# # Get only unique games
# myTest = coach_results %>% filter(Date == "9/5/1998" & ( Team == "Air Force (CO)" | Team == "Wake Forest (NC)"))
# myTest


coach_results$new = NA

for(i in 1:nrow(coach_results)){
  coach_results$new[i] = base::paste(sort(c(coach_results$Coach[i], coach_results$coach2[i], coach_results$Date[i])), collapse = "_")
}

cr2 = coach_results %>% 
  distinct(new, .keep_all = TRUE) %>% 
  mutate(
    coach_win = ifelse(PF > PA, 1, 0),
    coach_margin = PF - PA,
    date = as.numeric(lubridate::mdy(Date)) 
  ) %>%
  arrange(date) %>%
  select(date, coach = Coach, coach2, coach_win, coach_margin, year = Year, han_winner)

cr2 = cr2[complete.cases(cr2), ] %>% 
  filter(coach != "Deleted") %>% 
  filter(coach2 != "Deleted")  %>%
  mutate(han = ifelse(coach_win == 0 & han_winner == "Away", "Home", 
                       ifelse(coach_win == 0 & han_winner == "Home", "Away", han_winner)
                )
    ) %>%
  select(-han_winner)

# coach_win == 1                    ---> han2 = han
# coach_win == 0 & han == "Neutral" ---> han2 = "Neutral"
# coach_win == 0 & han == "Away"    ---> han2 = "Home"
# coach_win == 0 & han == "Home"    ---> han2 = "Away"

cr2





write.csv(cr2, file = "data/elo-game-data.csv", row.names = FALSE)

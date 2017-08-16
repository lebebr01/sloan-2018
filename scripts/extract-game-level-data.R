## ----setup, message = FALSE, warning = FALSE, error = FALSE--------------
# Read in data
library(readr)
library(dplyr)

# Read in Data ----
yby = read_csv("~/Dropbox/Football-Data/Data/year_by_year.csv", na = 'N/A')
coaches = read_csv("~/Dropbox/Football-Data/Data/coaches.csv", na = 'N/A')

# yby <- read_csv("C:/Users/bleb/Dropbox/Football-Data/Data/year_by_year.csv", na = 'N/A')
# coaches <- read_csv("C:/Users/bleb/Dropbox/Football-Data/Data/coaches.csv", na = 'N/A')
# curr_coaches <- read_csv("Data/current_coaches.csv")

# Create num_games variable for coaches
coaches = coaches %>%
  group_by(Team, Year, coach) %>%
  mutate(num_games = sum(Win, Loss, Tie))

# filter by year, and ensure one row per year, team, coach, combo 
# join coaches and year by year  ----
game_results = yby %>%
  select(Date, Year, Team, Opponent, PF, PA) %>%
  filter(Year >= 2005) 


winners = game_results %>%
  left_join(coaches, by = c("Year", "Team")) %>%
  select(Team, Year, Opponent, coach)

losers = game_results %>%
  select(Team2 = Team, Team = Opponent, Year) %>%
  left_join(coaches, by = c("Year", "Team")) %>%
  select(Opponent = Team, Year, coach2 = coach, Team = Team2)

coach_results = game_results %>% 
  left_join(winners, by = c("Year", "Team", "Opponent")) %>%
  left_join(losers, by = c("Year", "Team", "Opponent"))


# # Get only unique games
# myTest = coach_results %>% filter(Date == "9/5/1998" & ( Team == "Air Force (CO)" | Team == "Wake Forest (NC)"))
# myTest


coach_results$new = NA
for(i in 1:nrow(coach_results)){
  coach_results$new[i] = base::paste(sort(c(coach_results$coach[i], coach_results$coach2[i], coach_results$Date[i])), collapse = "_")
}

cr2 = coach_results %>% 
  distinct(new, .keep_all = TRUE) %>% 
  mutate(
    coach_win = ifelse(PF > PA, 1, 0),
    coach_margin = PF - PA,
    date = as.numeric(lubridate::mdy(Date)) 
  ) %>%
  arrange(date) %>%
  select(date, coach, coach2, coach_win, coach_margin, year = Year)

cr2 = cr2[complete.cases(cr2), ] %>% filter(coach != "Deleted") %>% filter(coach2 != "Deleted")


write.csv(cr2, file = "~/Documents/github/sloan-conference/data/elo-game-data.csv", row.names = FALSE)

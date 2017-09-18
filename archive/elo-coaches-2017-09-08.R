##################################################
### Load libraries
##################################################

library(corrr)
library(dplyr)
library(PlayerRatings)
library(readr)



##################################################
### Read in data
##################################################

coaches = read_csv("data/coaches_to2015.csv")
head(coaches)

games = read_csv(file = "data/elo-game-data-2017-09-08.csv")
head(games)



##################################################
### Estimate ELO ratings using 2005 data only
##################################################

games_2005 = games %>% filter(year == 2005) %>% select(date, coach, coach2, coach_win)
head(games_2005)

# Use k = 20
elo_20 = elo(games_2005, init = 1500, kfac = 20, history = TRUE)
elo20 = elo_20$ratings %>% 
  mutate(rank20 = 1:nrow(.)) %>%
  select(player = Player, rating20 = Rating, rank20)

# Use k = 25
elo_25 = elo(games_2005, init = 1500, kfac = 25, history = TRUE)
elo25 = elo_25$ratings %>% 
  mutate(rank25 = 1:nrow(.)) %>%
  select(player = Player, rating25 = Rating, rank25)

# Check correlation between rankings
elo20 %>% 
  left_join(elo25, by = "player") %>%
  select(rank20, rank25) %>%
  correlate(method = "spearman")

# Gut check Glen Mason
elo20 %>% filter(player == "Glen Mason")
elo25 %>% filter(player == "Glen Mason")

head(elo20, 20)
tail(elo20, 20)



##################################################
### Loop to estimate ELO for ALL years
##################################################

my_elo = list(NULL)
status = NULL

for(i in unique(games$year)){
  # Get one year's data
  games_year = games %>% 
    filter(year == i) %>% 
    select(date, coach, coach2, coach_win)

  # Fit ELO model
  elo_20 = elo(games_year, init = 1500, kfac = 20, history = TRUE, status = status)
  
  # Write output to list element
  my_elo[[i-1887]] = elo_20$ratings %>% 
    mutate(Rank = 1:nrow(.)) %>%
    select(Player, Rating, Rank) %>%
    mutate(Year = i)
  
  # Update initial status for following year
  status = elo_20$ratings %>%
    mutate(Rating = (Rating * .75 + 1500 * .25)) %>%
    select(Player, Rating)
  
}


# Put into a dataframe
ELO = do.call("rbind", my_elo)
head(ELO)
tail(ELO)
nrow(ELO)



##################################################
### Eliminate years where coach didn't actually coach
##################################################

ELO = ELO %>% select(Coach = Player, Year, Rating, Rank)
head(ELO %>% arrange(Coach, Year))

ELO2 = ELO %>% 
  arrange(Coach, Year) %>%
  left_join(coaches, by = c("Coach", "Year")) %>%
  select(Coach, Year, Team, Rating, Rank) %>%
  na.omit(Team)



##################################################
### Write data to file
##################################################

write.csv(ELO2, file = "data/elo-ratings-2017-09-08.csv", row.names = FALSE)



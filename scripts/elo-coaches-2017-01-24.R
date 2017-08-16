library(PlayerRatings)
library(dplyr)
library(readr)
library(corrr)

games = read_csv(file = "~/Documents/github/sloan-2018/data/elo-game-data.csv")
head(games)



# Use 2005 data to estimate ELO ratings
games_2005 = games %>% filter(year == 2005) %>% select(date, coach, coach2, coach_win)
head(games_2005)

elo_20 = elo(games_2005, init = 1300, kfac = 20, history = TRUE)
elo20 = elo_20$ratings %>% 
  mutate(rank20 = 1:nrow(.)) %>%
  select(player = Player, rating20 = Rating, rank20)

elo_25 = elo(games_2005, init = 1300, kfac = 25, history = TRUE)
elo25 = elo_25$ratings %>% 
  mutate(rank25 = 1:nrow(.)) %>%
  select(player = Player, rating25 = Rating, rank25)

elo20 %>% 
  left_join(elo25, by = "player") %>%
  select(rank20, rank25) %>%
  correlate(method = "spearman")


elo %>% filter(Player == "Glen Mason")

head(elo20, 20)
tail(elo20, 20)



myGlicko = glicko(cr2, init = c(0, 300), history = TRUE)
glicko = myGlicko$ratings
glicko$rank = 1:nrow(glicko)

glicko %>% filter(Player == "Glen Mason")
glicko %>% filter(Player == "Jerry Kill")
glicko %>% filter(Player == "Tracy Claeys")



library(PlayerRatings)
library(dplyr)
library(readr)
library(corrr)
library(ggplot2)

coaches = read_csv("data/coaches_to2015.csv") %>% filter(Year >= 2005)
games = read_csv(file = "data/elo-game-data.csv")
head(games)



# Use 2005 data to estimate ELO ratings
games_2005 = games %>% filter(year == 2005) %>% select(date, coach, coach2, coach_win)
head(games_2005)

elo_20 = elo(games_2005, init = 1500, kfac = 20, history = TRUE)
elo20 = elo_20$ratings %>% 
  mutate(rank20 = 1:nrow(.)) %>%
  select(player = Player, rating20 = Rating, rank20)

g = games %>% 
  filter(year == 2005) %>%
  mutate(my_gamma = ifelse(han == "Home", 65, ifelse(han == "Away", -65, 0)))

nrow(games_2005)
length(g$my_gamma)

elo_20_g = elo(games_2005, init = 1500, kfac = 20, history = TRUE, gamma = g$my_gamma)
elo20g = elo_20_g$ratings %>% 
  mutate(rankg = 1:nrow(.)) %>%
  select(player = Player, rating_g = Rating, rankg)

glicko_20 = glicko(games_2005, init = c(1500, 100), kfac = 20, history = TRUE)
glicko20 = glicko_20$ratings %>% 
  mutate(rank_glicko = 1:nrow(.)) %>%
  select(player = Player, rating_glicko = Rating, rank_glicko)



elo_25 = elo(games_2005, init = 1500, kfac = 25, history = TRUE)
elo25 = elo_25$ratings %>% 
  mutate(rank25 = 1:nrow(.)) %>%
  select(player = Player, rating25 = Rating, rank25)

elo20 %>% 
  left_join(elo25, by = "player") %>%
  select(rank20, rank25) %>%
  correlate(method = "spearman")

elo20 %>% 
  left_join(elo20g, by = "player") %>%
  left_join(glicko20, by = "player") %>%
  select(rank20, rankg, rank_glicko) %>%
  correlate(method = "spearman")


elo20 %>% filter(player == "Glen Mason")
elo25 %>% filter(player == "Glen Mason")

head(elo20, 20)
tail(elo20, 20)


# Loop to compute ELO

my_elo = list(NULL)
status = NULL

for(i in 2005:2015){
  # Get one year's data
  games_year = games %>% 
    filter(year == i) %>% 
    select(date, coach, coach2, coach_win)

  # Fit ELO model
  elo_20 = elo(games_year, init = 1500, kfac = 20, history = TRUE, status = status)
  
  # Write output to list element
  my_elo[[i-2004]] = elo_20$ratings %>% 
    mutate(Rank = 1:nrow(.)) %>%
    select(Player, Rating, Rank) %>%
    mutate(Year = i)
  
  # Update initial status for following year
  status = elo_20$ratings %>%
    mutate(Rating = (Rating * .75 + 1500 * .25)) %>%
    select(Player, Rating)
  
}


ELO = do.call("rbind", my_elo)
head(ELO)
tail(ELO)
nrow(ELO)


ELO = ELO %>% select(Coach = Player, Year, Rating, Rank)
head(ELO %>% arrange(Coach, Year))

ELO2 = coaches %>% 
  select(Coach, Year) %>% arrange(Coach, Year) %>%
  left_join(ELO, by = c("Coach", "Year"))




write.csv(ELO2, file = "data/elo-ratings.csv", row.names = FALSE)








ELO %>% 
  filter(Player %in% c("Glen Mason", "Kirk Ferentz", "Mack Brown", "Tim Brewster")) %>%
  ggplot(data = ., aes(x = Year, y = Rating, color = Player)) +
  geom_line() +
  theme_bw()

GLICKO %>% 
  filter(Player %in% c("Glen Mason", "Kirk Ferentz", "Mack Brown", "Tim Brewster")) %>%
  ggplot(data = ., aes(x = Year, y = Rating, color = Player)) +
  geom_line() +
  theme_bw()

ELO2 %>% 
  filter(Coach %in% c("Glen Mason", "Kirk Ferentz", "Mack Brown", "Tim Brewster")) %>%
  ggplot(data = ., aes(x = Year, y = Rank, color = Coach)) +
  geom_line() +
  theme_bw() +
  scale_y_reverse()

GLICKO %>% 
  filter(Player %in% c("Glen Mason", "Kirk Ferentz", "Mack Brown", "Tim Brewster")) %>%
  ggplot(data = ., aes(x = Year, y = Rank, color = Player)) +
  geom_line() +
  theme_bw() +
  scale_y_reverse()
  
ELO %>%
  group_by(Year) %>%
  summarize(M = mean(Rating), SD = sd(Rating), N = n())



# Loop to compute GLICKO

my_glicko = list(NULL)
status = NULL

for(i in 2005:2015){
  # Get one year's data
  games_year = games %>% 
    filter(year == i) %>% 
    select(date, coach, coach2, coach_win)
  
  # Fit ELO model
  glicko_20 = glicko(games_year, init = c(1500, 200), history = TRUE, status = status)
  
  # Write output to list element
  my_glicko[[i-2004]] = glicko_20$ratings %>% 
    mutate(Rank = 1:nrow(.)) %>%
    select(Player, Rating, Rank) %>%
    mutate(Year = i)
  
  # Update initial status for following year
  status = glicko_20$ratings %>%
    mutate(Rating = (Rating * .75 + 1500 * .25)) %>%
    mutate(Deviation = 200) %>%
    select(Player, Rating, Deviation)
  
}


GLICKO = do.call("rbind", my_glicko)
head(GLICKO)
tail(GLICKO)
nrow(GLICKO)


write.csv(GLICKO, file = "data/glicko-ratings.csv", row.names = FALSE)



myGlicko = glicko(cr2, init = c(0, 300), history = TRUE)
glicko = myGlicko$ratings
glicko$rank = 1:nrow(glicko)

glicko %>% filter(Player == "Glen Mason")
glicko %>% filter(Player == "Jerry Kill")
glicko %>% filter(Player == "Tracy Claeys")



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
#yby2 = read_excel("data/year_by_year_to2015_withcoaches_interim.xlsx")
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
  arrange(date) %>%
  na.omit()


##################################################
### Loop to estimate ELO for ALL years
##################################################

my_elo = list(NULL)
status = NULL

for(i in unique(cr2$year)){
  # Get one year's data
  games_year = cr2 %>% 
    filter(year == i) %>% 
    select(date2, coach1, coach2, outcome)
  
  # Fit ELO model
  elo_20 = elo(games_year, init = 1500, kfac = 20, history = TRUE, status = status)
  
  # Write output to list element
  my_elo[[i-1965]] = elo_20$ratings %>% 
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
### Add team-level ELO
##################################################

team_ELO = ELO2 %>%
  group_by(Team) %>%
  summarize(avg_team_elo = mean(Rating, na.rm = TRUE))

ELO3 = ELO2 %>%
  inner_join(team_ELO, by = c("Team")) %>%
  mutate(resid_elo = Rating - avg_team_elo)
  
yrs_coached = ELO3 %>%
  group_by(Coach, Team) %>% 
  mutate(years_coached = row_number() - 1) %>%
  select(Coach, Team, Year, years_coached)

ELO4 = ELO3 %>%
  inner_join(yrs_coached, by = c("Coach", "Team", "Year"))

head(ELO4)


##################################################
### Write data to file
##################################################

write.csv(ELO4, file = "data/elo-ratings-2017-09-18.csv", row.names = FALSE)



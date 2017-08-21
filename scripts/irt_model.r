## ----multilevel----------------------------------------------------------
# Read in data
library(readr)
library(dplyr)

# Read in Data ----
yby <- read_csv("Data/year_by_year_to2015.csv", na = 'N/A')
coaches <- read_csv("Data/coaches_to2015.csv", na = 'N/A')
expec <- read_csv("Data/fs_expec.csv")
# curr_coaches <- read_csv("Data/current_coaches.csv")

# Create num_games variable for coaches
coaches <- coaches %>%
  distinct(Year, Team, Coach, .keep_all = TRUE) %>% 
  filter(PF > 50)

# filter by year, and ensure one row per year, team, Coach, combo 
# join coaches and year by year  ----
yby_coach <- yby %>%
  filter(Year >= 1998) %>%
  group_by(Year, Team) %>%
  mutate(win_calc = PF > PA, 1, 0) %>%
  left_join(coaches, by = c("Year", "Team")) %>%
  group_by(Year, Team, Coach) %>%
  mutate(num_games = n()) %>%
  filter(num_games >= 6) %>%
  arrange(Team, Coach, Year) %>%
  mutate(games = 1:n())

team_aff <- read_csv("Data/Team_NCAA_Affiliation.csv")
yby_coach <- yby_coach %>% 
  left_join(team_aff, by = c("Team", "Year")) %>%
  filter(Subdivision == "Division I-A")

yrs_with_team <- yby_coach %>%
  group_by(Team, Coach, Year) %>%
  distinct() %>%
  ungroup() %>%
  group_by(Team, Coach) %>%
  arrange(Year) %>%
  mutate(yrs_with_team = 1:n())

yby_coach <- left_join(yby_coach, 
                       select(yrs_with_team, Team, Coach, Year, yrs_with_team))

# Scale years with team
yby_coach$yrs_with_team_scale <- scale(yby_coach$yrs_with_team,
                                       center = FALSE)
yby_coach$yrs_with_team_scale <- yby_coach$yrs_with_team_scale - min(yby_coach$yrs_with_team_scale)


yby_coach <- expec %>%
  select(Team, Year, expec) %>%
  right_join(yby_coach, by = c('Team', 'Year')) %>%
  filter(Year >= 2005)

# bring in opponent winning percentage
source("R/aggregate_team.r")

yby_coach <- yby_coach %>%
  left_join(win_per, by = c('Team', 'Year'))

# Fit Multilevel Rasch IRT Model
library(lme4)
library(optimx)
# Actual win/loss
# fm1a <- glmer(wingbg ~ 0 + yrs_with_team_scale + I(yrs_with_team_scale^2) +
#                 (1|Coach) + (1|Team) ,
#               data = yby_coach, family = binomial,
#               control = glmerControl(optimizer = "bobyqa",
#                                      optCtrl = list(maxfun = 10000)))
#-----
# Check to remove teams with only a few years of data.
# Fit model to winning percentage every year instead of win/loss data.
# comparison of these two models - model implied probability versus model implied 
# winning percentage.
# Think about adding two covariates instead of games (top25 vs outside top 25,
# conference vs non-conference)
# Flag bowl game to include as covariate

# Explore multilevel_irt file to fit Coach:yrs_with_team
# Treat yrs_with_team as fixed - compare to above model
# use weights for winning percentage model: wts = numgames

# Explore differences between 1/0 game level vs winning percentage

# 1. (1|yrs_with_team) + (1|Coach) + (1|Team)
# 2. (1|Coach:yrs_with_team) + (1|Team)
# 3. yrs_with_team + (yrs_with_team | Coach) + (1 | Team)
# 4. Add quadratic term to fixed and random

# Model 1
win_nogame_1 <- glmer(win_calc ~ 1 + 
                        (1 | yrs_with_team) +
                        (1 | Coach) +
                        (1 | Team),
                      data = yby_coach, family = binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 10000)))

# Model 2
win_nogame_2 <- glmer(win_calc ~ 1 + 
                        (1 | Coach:yrs_with_team) +
                        (1 | Team),
                      data = yby_coach, family = binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 10000)))

# Model 3
win_nogame_3 <- glmer(win_calc ~ 1 + yrs_with_team_scale + 
                        (yrs_with_team_scale  | Coach) +
                        (1 | Team),
                      data = yby_coach, family = binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 10000)))

# Model 4
win_nogame_4 <- glmer(win_calc ~ 1 + yrs_with_team_scale + 
                      I(yrs_with_team_scale^2) + 
                      (yrs_with_team_scale + I(yrs_with_team_scale^2) | Coach) +
                      (1 | Team),
                    data = yby_coach, family = binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 10000)))

# Model - winning percentage
win_per_mod <- lmer(Pct ~ 1 + yrs_with_team_scale + 
                       (1 | Coach) + 
                       (1 | Team), 
                     data = distinct(yby_coach, Team, Year, 
                                     .keep_all = TRUE))

# win_with_game <- glmer(win_calc ~ 0 + expec + avg_win_per + factor(games) + 
#                          yrs_with_team_scale + I(yrs_with_team_scale^2) + 
#                          (yrs_with_team_scale + I(yrs_with_team_scale^2)|Coach) + 
#                          (1|Team) , 
#                        data = yby_coach, family = binomial,
#                        control = glmerControl(optimizer = "bobyqa",
#                                               optCtrl = list(maxfun = 10000)))

# save data into rda file
save(win_nogame_1, win_nogame_2, win_nogame_3, win_nogame_4, win_per_mod,
     file = 'Data/irt_model_results.rda')

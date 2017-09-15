# load packages
library(tidyverse)

# Read in new yby_withcoaches file
yby <- readxl::read_excel('data/year_by_year_to2015_withcoaches.xlsx')

# manually calculate win/loss variable
yby_coach <- yby %>%
  mutate(win_calc = ifelse(PF > PA, 1, 0)) %>%
  group_by(Year, Team, Coach) %>%
  mutate(num_games = n()) %>%
  arrange(Team, Coach, Year) %>%
  mutate(games = 1:n())

yrs_with_team <- yby_coach %>%
  distinct(Team, Coach, Year) %>%
  group_by(Team, Coach) %>%
  arrange(Year) %>%
  mutate(yrs_with_team = 1:n())

yby_coach <- left_join(yby_coach,
                       select(yrs_with_team, Team, Coach, Year, yrs_with_team))


# fitting models
library(lme4)
library(optimx)
win_nogame_1 <- glmer(win_calc ~ 1 +
                        (1 | yrs_with_team) +
                        (1 | Coach) +
                        (1 | Team),
                      data = yby_coach, family = binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 10000)))

# Winning Percentage
yby_coach_wp <- yby_coach %>%
  group_by(Coach, Team) %>%
  mutate(win_per = mean(win_calc)) %>%
  distinct(Coach, Team, Year, .keep_all = TRUE)

# Model - winning percentage
win_per_mod <- lmer(win_per ~ 1 +
                      (1 | Coach) +
                      (1 | Team),
                    data = yby_coach_wp)

# combine model results by Team, Coach, Year



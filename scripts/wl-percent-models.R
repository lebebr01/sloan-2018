## ----multilevel----------------------------------------------------------
# Packages
library(readr)
library(dplyr)
library(lme4)
library(optimx)

#### Read in Data ####
yby <- read_csv("data/year_by_year_to2015.csv", na = 'N/A')
team_aff <- read_csv("Data/Team_NCAA_Affiliation.csv")
coaches <- read_csv("data/coaches_to2015.csv")
# expec <- read_csv("data/fs_expec.csv")

#### Joining data ####
## yby games and team affiliation, keeping only DI-A games
yby <- yby %>% 
  left_join(team_aff, by = c("Team", "Year")) %>%
  filter(Subdivision == "Division I-A")

## Removing duplicate Year-Team-Coach rows, creating num_games and
## keeping only coaches who coached at least 6 games
coaches <- coaches %>%
  distinct(Year, Team, Coach, .keep_all = TRUE) %>%
  mutate(num_games_coach = Win + Loss + Tie,
         win_pct_coach = Win/num_games_coach) %>%
  filter(num_games_coach >= 6)

## Keeping all rows for coaches who coached in 2005 or later
CoachNames2005 <- unique(coaches[coaches$Year==2005,]$Coach)
coaches_05to15 <- coaches %>% filter(Year>=2005 | Coach %in% CoachNames2005) %>%
  rename(PF_total=PF,PA_total=PA)

## Joining coaches to games and keeping matches only
# Adding variables for winning game (the WL column accounts for vacated wins which we don't want),
# for number of games played by the team in a year, and win% for the team in a year
yby_coach_05to15 <- yby %>%   group_by(Year, Team) %>%
  mutate(win_calc = ifelse(PF > PA, 1, 0),
         num_games_team = n(),
         win_pct_team = sum(win_calc)/num_games_team) %>% ungroup() %>%
  inner_join(coaches_05to15, by = c("Year", "Team")) %>%
  mutate(num_games_diff = num_games_team - num_games_coach)

## Determining number of years coach has been with a team and years coaching overall
yrs_with_team <- yby_coach_05to15 %>%
  distinct(Team, Coach, Year) %>%
  group_by(Team, Coach) %>%
  arrange(Year) %>%
  mutate(yrs_with_team = 1:n()) %>% ungroup() %>%
  group_by(Coach) %>%
  arrange(Year) %>%
  mutate(yrs_coaching = 1:n())

## Adding yrs_with_team to yby_coach and scaling
yby_coach_05to15 <- yby_coach_05to15 %>% left_join(yrs_with_team,by=c("Team","Year","Coach"))
yby_coach_05to15$yrs_with_team_scale <- scale(yby_coach_05to15$yrs_with_team,center = FALSE)
yby_coach_05to15$yrs_with_team_scale <- yby_coach_05to15$yrs_with_team_scale - min(yby_coach_05to15$yrs_with_team_scale)

#### IRT Models ####
## Not accounting for Team
# WL model
WL.noteam.mod <- glmer(win_calc ~ 1 + (1 | Coach:Year),
                       data = yby_coach_05to15, family = binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 10000)))

# win% model
WinPer.noteam.mod <- lmer(win_pct_coach ~ 1 + Year + (1 | Coach), 
                          data = distinct(yby_coach_05to15, Team, Year, Coach,
                                          .keep_all = TRUE))


summary(WL.noteam.mod)
summary(WinPer.noteam.mod)
ranef(WL.noteam.mod)
ranef(WinPer.noteam.mod)
WL_noteam_predict <- predict(WL.noteam.mod)

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

win_nogame_year <- glmer(win_calc ~ 1 + 
                           (1 | Coach:Year) +
                           (1 | Team),
                         data = yby_coach, family = binomial,
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 10000)))

# Winning Percentage
yby_coach_wp <- yby_coach %>%
  group_by(Coach, Team, Year) %>%
  mutate(win_per = mean(win_calc)) %>%
  distinct(Coach, Team, Year, .keep_all = TRUE)

# Model - winning percentage
win_per_mod <- lmer(win_per ~ 1 +
                      (1 | Coach) +
                      (1 | Team),
                    data = yby_coach_wp)

# combine model results by Team, Coach, Year

coach_by_year <- ranef(win_nogame_year)$`Coach:Year`
coach_by_year$coach_year <- rownames(coach_by_year)
coach_year <- do.call('rbind', strsplit(coach_by_year$coach_year, ':'))
names(coach_year) <- c('Coach', 'Year')
coach_year <- data.frame(coach_year)
names(coach_year) <- c('Coach', 'Year')
coach_by_year <- bind_cols(coach_by_year, coach_year)
names(coach_by_year)[1] <- 'rating'
coach_by_year$Year <- as.numeric(as.character(coach_by_year$Year))

# add in team information
coaches <- readr::read_csv('data/coaches_to2015.csv')
coaches <- select(coaches, Team, Year, Coach)

coach_by_year <- coach_by_year %>%
  left_join(coaches)

# add in team random effects
team_ranef <- ranef(win_nogame_year)$Team
team_ranef$Team <- rownames(team_ranef)
names(team_ranef)[1] <- 'avg_team_rating'

coach_by_year <- coach_by_year %>%
  left_join(team_ranef)

readr::write_csv(coach_by_year, path = 'data/glmm_coach_year.csv')


# Output for winning percentage model
coach_winper <- ranef(win_per_mod)$Coach


# coach_by_year %>%
#   filter(Coach %in% c('Mack Brown', 'Kirk Ferentz', 'Glen Mason', "Joe Paterno")) %>%
#   ggplot(aes(x = Year, y = rating)) +
#   geom_line(aes(color = Coach, group = Coach)) + 
#   theme_bw()

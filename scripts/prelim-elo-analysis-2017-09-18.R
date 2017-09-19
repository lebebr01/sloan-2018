##################################################
### Load libraries
##################################################

library(dplyr)
library(readr)
library(corrr)
library(ggplot2)



##################################################
### Read in data
##################################################

ELO = read_csv("data/elo-ratings-2017-09-18.csv")
head(ELO)


coaches = read_csv("data/coaches_to2015.csv")
head(coaches)


##################################################
### Add in school, etc.
##################################################


# YEARS = ELO %>%
#   group_by(Coach) %>%
#   summarize(Max = max(years_coached)) %>%
#   arrange(desc(Max))
# 
# TEAMS = ELO %>%
#   group_by(Coach) %>%
#   summarize(Num_Teams = n_distinct(Team)) %>%
#   arrange(desc(Num_Teams))
# 
# 
# 
# ELO2 = ELO %>%
#   inner_join(YEARS, by = "Coach") %>%
#   inner_join(TEAMS, by = "Coach")
# 
# write_csv(ELO5, "data/elo-ratings-2017-09-09.csv")
# 




##################################################
### Plot of ratings over time
##################################################

ELO %>% 
  filter(Coach %in% c("Glen Mason", "Kirk Ferentz", "Mack Brown", "Tim Brewster", "Joe Paterno")) %>%
  ggplot(data = ., aes(x = Year, y = Rating, color = Team)) +
  geom_line() +
  geom_hline(aes(yintercept = avg_team_elo, color = Team)) +
  theme_bw() +
  facet_wrap(~Coach)

# Residualized ELO
ELO %>% 
  filter(Coach %in% c("Glen Mason", "Kirk Ferentz", "Mack Brown", "Tim Brewster")) %>%
  ggplot(data = ., aes(x = Year, y = resid_elo, color = Team)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  facet_wrap(~Coach)


ELO %>% 
  filter(Team %in% c("Minnesota", "Iowa", "Texas")) %>%
  ggplot(data = ., aes(x = Year, y = Rating, color = Coach)) +
  geom_line() +
  geom_hline(aes(yintercept = avg_team_elo)) +
  theme_bw() +
  facet_wrap(~Team, nrow = 3)

# Residualized ELO
ELO %>% 
  filter(Team %in% c("Minnesota", "Iowa", "Texas")) %>%
  ggplot(data = ., aes(x = Year, y = resid_elo, color = Coach)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  facet_wrap(~Team, nrow = 3) +
  scale_colour_manual(values = rep(RColorBrewer::brewer.pal(5, "Set1"), times = 17)) +
  #ggsci::scale_color_d3() +
  guides(color = FALSE)





my_samp = sample(unique(ELO$Coach), 10, replace = FALSE)

ELO %>% 
  filter(Coach %in% my_samp) %>%
  ggplot(data = ., aes(x = Year, y = resid_elo, color = Coach)) +
  geom_hline(yintercept = 0) +
  geom_line(linetype = "dashed") +
  theme_bw() +
  #geom_smooth(se = FALSE, method = "lm", formula = y~poly(x,2)) +
  facet_wrap(~Coach) +
  guides(color = FALSE)

# Residualized ELO
ELO %>% 
  filter(Coach %in% c("Glen Mason", "Kirk Ferentz", "Mack Brown", "Tim Brewster")) %>%
  ggplot(data = ., aes(x = Year, y = resid_elo, color = Team)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  facet_wrap(~Coach)


##################################################
### Plot of rankings over time
##################################################

ELO %>% 
  filter(Coach %in% c("Glen Mason", "Kirk Ferentz", "Mack Brown", "Tim Brewster")) %>%
  ggplot(data = ., aes(x = Year, y = Rank, color = Coach)) +
  geom_line() +
  theme_bw() +
  scale_y_reverse()



##################################################
### Summaries by year
##################################################

ELO %>%
  group_by(Year) %>%
  summarize(M = mean(Rating, na.rm = TRUE), SD = sd(Rating, na.rm = TRUE), N = n())





ELO5 %>% 
  #filter(Max > 10) %>%
  ggplot(data = ., aes(x = years_coached, y = resid_elo)) +
  geom_line(alpha = 0.4, aes(group = Coach)) +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  facet_wrap(~Num_Teams)
  

length(unique(ELO5$Coach))

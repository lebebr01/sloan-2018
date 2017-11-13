library(tidyverse)
library(readr)
library(forcats)

ELO = read_csv("data/elo-ratings-2017-09-18.csv")
coach_info = read_csv("data/coaches_to2015.csv")

ELO$C_Team = paste(ELO$Coach, ":", ELO$Team)

ELO2 = ELO %>% 
  left_join(coach_info, by = c("Coach", "Year", "Team")) %>% 
  drop_na(Job_End_Category) %>%
  filter(Job_End_Category < 6) %>%
  group_by(Coach) %>%
  mutate(yrs_coached_ovr = (1:n() - 1)) %>%
  ungroup()

ELO2 <- ELO2 %>%
  mutate(Job_End_Category2 = as.character(Job_End_Category),
         Job_End_Category2 = fct_collapse(Job_End_Category2,
                                          'Current Coach' = '0',
                                          'Retired/Health' = c('1', '3'),
                                          'Left Better Job' = '2',
                                          'Resigned' = '4',
                                          'Fired' = '5'
         ))
      # 
      # ifelse(Job_End_Category == 0, 'Current Coach', 
      #                               ifelse(Job_End_Category %in% c(1, 3), 'Retired/Health',
      #                                      ifelse(Job_End_Category == 2, 'Left Better Job',
      #                                                    ifelse(Job_End_Category == 4, 'Resigned',
      #                                                           'Fired')))))
crazy = ELO2 %>% group_by(Job_End_Category2) %>%
  summarize(M = mean(Rating), Team = mean(avg_team_elo))

my_sample = sample(unique(ELO2$C_Team ), size = 50, replace = FALSE)

avg_team <- ELO2 %>%
  summarise(mean_team = mean(avg_team_elo))


# Job End Category
# 0 = current coach
# 1 = retired
# 2 = left for better job
# 3 = left for health reasons
# 4 = resigned due to pressure from school
# 5 = fired
# 6 = interim coach

"%ni%" <- Negate("%in%")

ELO2 %>% 
  filter(Coach %ni% c('Urban Meyer', 'Nick Saban', 'Bob Stoops', 'Gary Patterson')) %>%
  ggplot(data = ., aes(x = years_coached, y = resid_elo, group = Coach, color = Team)) +
  #geom_line(alpha = 0.3) +
  geom_smooth(aes(group = Job_End_Category2), se = FALSE) +
  geom_smooth(aes(group = Job_End_Category2, y = I(Rating-1500)), se = FALSE, linetype = "dotted") +
  theme_bw() +
  guides(color =  FALSE) +
  facet_wrap(~Job_End_Category2) +
  coord_cartesian(xlim = c(0, 10)) +
  geom_hline(data = crazy, aes(yintercept = I(M-1500))) +
  geom_hline(data = crazy, aes(yintercept = I(Team-1500)), color = "green") +
  geom_hline(data = avg_team, aes(yintercept = 7.533), color = 'orange') + 
  geom_hline(aes(yintercept = 0), linetype = 'dotted')
  

length_coached <- ELO2 %>%
  group_by(Job_End_Category2) %>%
  summarise(mean = mean(years_coached),
            median = median(years_coached),
            max = max(years_coached),
            min = min(years_coached),
            q25 = quantile(years_coached, probs = .25),
            q75 = quantile(years_coached, probs = .75))

length_coached_ovr <- ELO2 %>%
  group_by(Job_End_Category2) %>%
  summarise(mean = mean(yrs_coached_ovr),
            median = median(yrs_coached_ovr),
            max = max(yrs_coached_ovr),
            min = min(yrs_coached_ovr),
            q25 = quantile(yrs_coached_ovr, probs = .25),
            q75 = quantile(yrs_coached_ovr, probs = .75))

# select last coach record
last_coach <- ELO2 %>%
  group_by(Team, Coach) %>%
  mutate(max_coach = max(yrs_coached_ovr)) %>%
  filter(yrs_coached_ovr == max_coach)

ELO2 %>%
  filter(Coach %in% c('Urban Meyer', 'Tommy Tuberville',
                                   'Tyrone "TY" Willingham', "Nick L. Saban",
                                   "Rich Rodriguez")) %>%
  ggplot(aes(x = yrs_coached_ovr, y = Rating, group = Coach, color = Job_End_Category2)) + 
  geom_line(aes(x = yrs_coached_ovr, y = Rating, group = Coach, color = Job_End_Category2),
            size = 1) + 
  geom_text(data = last_coach, aes(label = Coach))


left_job <- ELO2 %>%
  filter(Job_End_Category2 == "Left Better Job") %>%
  select(Coach) %>%
  distinct()

ELO2 %>%
  semi_join(left_job) %>%
  ggplot(aes(x = yrs_coached_ovr, y = Rating, group = Coach, color = Job_End_Category2)) + 
  geom_line(aes(x = yrs_coached_ovr, y = Rating, group = Coach, color = Job_End_Category2),
            size = 1) + 
  geom_text(data = filter(last_coach, Job_End_Category2 == 'Left Better Job'), aes(label = Coach))
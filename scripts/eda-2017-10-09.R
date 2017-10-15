library(tidyverse)
library(readr)

ELO = read_csv("data/elo-ratings-2017-09-18.csv")
coach_info = read_csv("data/coaches_to2015.csv")

ELO$C_Team = paste(ELO$Coach, ":", ELO$Team)

ELO2 = ELO %>% 
  left_join(coach_info, by = c("Coach", "Year", "Team")) %>% 
  drop_na(Job_End_Category)


crazy = ELO2 %>% group_by(Job_End_Category) %>%
  summarize(M = mean(Rating), Team = mean(avg_team_elo))

my_sample = sample(unique(ELO2$C_Team ), size = 50, replace = FALSE)

ELO2 %>% 
  #filter(C_Team %in% my_sample) %>%
  ggplot(data = ., aes(x = years_coached, y = resid_elo, group = Coach, color = Team)) +
  #geom_line(alpha = 0.3) +
  geom_smooth(aes(group = Job_End_Category), se = FALSE) +
  geom_smooth(aes(group = Job_End_Category, y = I(Rating-1500)), se = FALSE, linetype = "dotted") +
  theme_bw() +
  guides(color =  FALSE) +
  facet_wrap(~Job_End_Category) +
  xlim(0, 10) +
  geom_hline(data = crazy, aes(yintercept = I(M-1500))) +
  geom_hline(data = crazy, aes(yintercept = I(Team-1500)), color = "green")
  



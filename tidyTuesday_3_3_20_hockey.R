#TidyTuesday - 3-3-20 - NHL Goals
setwd("~/Programming/data")

library(tidyverse)
library(scales)
library(ggrepel)
library(hrbrthemes)

# Get shot level details from MoneyPuck
URL <- "http://peter-tanner.com/moneypuck/downloads/shots_2007-2018.zip"
download.file(URL, "~/shots_2007-2018.zip")
unzip(zipfile = "shots_2007-2018.zip")

shots <- readr::read_csv(("shots_2007-2018.csv"))

# Which players have the best "take the lead" shot percentage?
clutch <- shots %>% 
          select(season, time, period, goal, team, homeTeamGoals, awayTeamGoals, shooterName, isPlayoffGame) %>% 
          mutate(take_the_lead = case_when(homeTeamGoals==awayTeamGoals ~ 1, TRUE ~ 0)) %>% 
          filter(take_the_lead == 1) %>% 
          select(shooterName, isPlayoffGame, goal) %>% 
          group_by(shooterName, isPlayoffGame) %>%
          summarise(totalShots = n(), totalGoals = sum(goal)) %>% 
          mutate(pct = totalGoals/totalShots) %>% 
          pivot_wider(names_from = isPlayoffGame, values_from = c(totalShots, totalGoals, pct)) %>% 
          drop_na() %>% 
          filter(totalShots_1 > 100) %>% 
          mutate(delta = pct_1 - pct_0) %>%
          ungroup() %>% 
          #Identify callouts
          mutate(callout = case_when(
            delta == min(delta) ~ "B. Marchand -6% Drop",
            delta == max(delta) ~ "N. Lindstrom +3% Gain",
            pct_0 > .1 & pct_1 > .1 ~ "J. Toews - Both +10%",
            TRUE ~ "X"))
              

#Create plot
ggplot(clutch, aes(x=pct_1, y=pct_0)) + 
  geom_point(color="black", fill="gray", shape=21, alpha=0.65, size=5, stroke = 1) +
  geom_text_repel(data = clutch %>% filter(callout != "X"), 
             aes(label = callout), nudge_y = -0.009) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,.12), xlim = c(0,.12)) +
  labs(title = "NHL 'Take the Lead' Shot Percentages", 
       subtitle = "Regular Season vs.Playoffs, limited to players with +100 shot attempts in playoffs n=70\nTake the Lead Shot defined as shot taken when game is tied") +
  labs(caption = "@jlytes - #TidyTuesday 3.3.20 - Shot data from moneypuck.com, 2007 - 2018 Seasons") +
  xlab("Playoffs") + ylab("Regular Season") +
  theme_ipsum() + theme(legend.position="none")

#Save Image of Plot
ggsave("~/Programming/TidyTuesday/tidyTuesday_hockey_3_3_20.png")
              

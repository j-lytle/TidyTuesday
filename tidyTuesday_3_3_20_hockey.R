#TidyTuesday - 3-3-20 - NHL Goals
setwd("~/Programming/data")

library(tidyverse)
library(scales)
library(ggrepel)
library(hrbrthemes)
library(plotly)
remotes::install_github("wilkelab/ggtext", force=TRUE)
library(ggtext)

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

#By Period look - will keep same 70 scorers
players <- unique(clutch$shooterName)
clutchPeriod <- shots %>% 
  select(season, time, period, goal, team, homeTeamGoals, awayTeamGoals, shooterName, isPlayoffGame) %>% 
  mutate(take_the_lead = case_when(homeTeamGoals==awayTeamGoals ~ 1, TRUE ~ 0)) %>% 
  filter(take_the_lead == 1 & shooterName %in% players) %>% 
  select(shooterName, period, isPlayoffGame, goal) %>% 
  group_by(shooterName, isPlayoffGame, period) %>%
  summarise(totalShots = n(), totalGoals = sum(goal)) %>% 
  mutate(pct = totalGoals/totalShots) %>% 
  pivot_wider(names_from = isPlayoffGame, values_from = c(totalShots, totalGoals, pct)) %>% 
  drop_na() %>% 
  mutate(delta = pct_1 - pct_0,
         period_txt = factor(case_when(
                                period == 4 ~ "Overtime",
                                TRUE ~ paste("Period ", period))),
         #Reorder factor as Overtime will jump to first slot
         period_txt = fct_reorder(period_txt, period)
         ) %>%
  ungroup()

#Calculate regular season (rs) shot % - not just "Take the Lead"
rs <- shots %>% 
  filter(isPlayoffGame == 0 & shooterName %in% players) %>% 
  select(goal, shooterName) %>% 
  group_by(shooterName) %>% 
  summarise(rsShots = n(), rsGoals = sum(goal)) %>% 
  mutate(rsPct = rsGoals/rsShots) 

#Add back into by Period ds
final <- inner_join(clutchPeriod, rs, by="shooterName") %>% 
  #remove any periods with 0 pct
  filter(pct_1 != 0) %>% 
  mutate(rsDelta = rsPct - pct_0,
         #Identify callouts
          callout = case_when(
           shooterName == "Alex Ovechkin" ~ "AO",
           shooterName == "Sidney Crosby" ~ "SC",
           TRUE ~ "X"))

#Subtitle coloring



p <-ggplot(final, aes(x=pct_1, y=rsPct, 
                             text=paste(shooterName,"<br>",
                                        sprintf("Regular Season: %s<br>Playoffs: %s<br>Playoff Shots: %s", rsPct, pct_1, totalShots_1)))) + 
  geom_point(color="black", fill="gray", shape=21, alpha=0.65, size=5, stroke = 1) +
  geom_point(data = final %>% filter(callout == "AO"), color="black", fill="red", shape=21, alpha=0.65, size=5, stroke = 1) +
  geom_point(data = final %>% filter(callout == "SC"), color="black", fill="yellow", shape=21, alpha=0.65, size=5, stroke = 1) +
  coord_cartesian(ylim = c(0,.15), xlim = c(0,.2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "NHL: Who becomes clutch in the playoffs?", 
         subtitle = "Regular Season all periods and situations vs. Playoff Tie Game\nLimited to players with +100 shot attempts in playoffs n=70\nFor reference Ovechkin (red) and Crosby (yellow)") +
  labs(caption = "@jlytes - #TidyTuesday 3.3.20 - Shot data from moneypuck.com, 2007 - 2018 Seasons") +
  xlab("Playoff Game Tied") + ylab("Regular Season (all periods and situations)") +
  theme_ipsum() + theme(legend.position="none") +
  facet_wrap(~ period_txt)

#Save Image of Plot
ggsave("~/Programming/TidyTuesday/tidyTuesday_hockey_3_3_20_by_period.png", p)

ggplotly(p, tooltip="text") %>% 
  layout(title = list(text = paste0('NHL: Who becomes clutch in the playoffs?',
                                    '<br>',
                                    '<sup>',
                                    '',
                                    '</sup>')))






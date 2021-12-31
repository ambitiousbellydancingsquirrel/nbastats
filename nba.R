# libraries

library(tidyverse)
library(nbastatR)
library(lubridate)
library(ggtea)
library(ggthemes)

library(GGally)
library(tidymodels)

theme_set(theme_void())

# data

NBA.Shot.Locations.1997...2020 <- read.csv("C:/Users/Pushkar/Downloads/NBA Shot Locations 1997 - 2020.csv", stringsAsFactors=TRUE)
nba <- as_tibble(NBA.Shot.Locations.1997...2020)

# EDA
#Season-wise stats
nba18 <- nba %>% 
  filter(Game.Date > 20171017 & Game.Date < 20180411) %>% 
  mutate(Shot.Made.Flag = as.factor(Shot.Made.Flag))

nba19 <- nba %>% 
  filter(Game.Date > 20181016 & Game.Date < 20190410) %>% 
  mutate(Shot.Made.Flag = as.factor(Shot.Made.Flag))

nba20 <- nba %>% 
  filter(Game.Date > 20191022 & Game.Date < 20200311) %>% 
  mutate(Shot.Made.Flag = as.factor(Shot.Made.Flag))

# Player-by-player shot chart

nba20 %>%
  filter(Player.Name == "Duncan Robinson" | Player.Name == "Luka Doncic") %>% 
  ggplot() + 
  geom_point(aes(X.Location, Y.Location, col = Shot.Type), alpha = 0.25, size = 2) + 
  coord_fixed() + facet_wrap(~Player.Name)

# 2017-18 Rookies comparison (Ben Simmons, Spida, JT)

rookies <- nba %>%
  filter(Player.Name == "Jayson Tatum" | Player.Name == "Donovan Mitchell" | Player.Name == "De'Aaron Fox") %>% 
  mutate(Season = if_else(Game.Date<20180411,"2017-18",if_else(Game.Date < 20190410, "2018-19", "2019-20"))) %>% 
  view()

#shot chart
rookies %>% 
  ggplot(aes(X.Location, Y.Location)) + 
  geom_point(alpha = 0.1, col = "grey30") +
  facet_wrap(~Season+Player.Name) + coord_fixed()

#shot zone
rookies_zone <- rookies %>% 
  group_by(Player.Name, Shot.Zone.Basic) %>% 
  summarise(n = n()) %>% 
  mutate(perc = round(n/sum(n),2)) %>% 
  filter(!perc < 0.05)

rookies_zone %>% 
  ggplot(aes(Shot.Zone.Basic, perc)) + theme_economist() + 
  geom_line(aes(col = Player.Name, group = Player.Name)) + 
  geom_point(aes(col = Player.Name), size = 2)

rookies_radar <- rookies_zone %>% 
  select(-n) %>% 
  pivot_wider(names_from = Shot.Zone.Basic, values_from = perc) %>% 
  rename(group = Player.Name) %>% 
  mutate(group = as.character(group)) %>% 
  as_tibble()

colnames(rookies_radar)

ggradar(rookies_radar)

#shots made by distance
rookies %>%
  filter(Shot.Distance < 32) %>% 
  ggplot() + 
  geom_bar(aes(Shot.Distance), alpha = 0.5) + 
  geom_col(aes(Shot.Distance,Shot.Made.Flag)) + 
  facet_wrap(~Season+Player.Name)
 

#shot range
rookies %>% 
  group_by(Player.Name, Shot.Zone.Range) %>% 
  filter(!Shot.Zone.Range == "Back Court Shot") %>% 
  summarise(n = n()) %>% 
  mutate(perc = round(n/sum(n),2)) %>% 
  select(-n) %>% 
  ggplot(aes(Shot.Zone.Range, perc)) + theme_economist() +
  geom_col(aes(fill = Player.Name), position = "dodge")

#shot type
nba18 %>% 
  group_by(Player.Name, Shot.Zone.Range) %>% 
  filter(!Shot.Zone.Range == "Back Court Shot") %>% 
  summarise(n = n()) %>% 
  mutate(perc = round(n/sum(n),2)) %>% 
  ggplot(aes(Shot.Zone.Range, perc)) + theme_gray() +
  geom_point(aes(group = Player.Name), alpha = 0.1) + theme(legend.position = "none")
  
nbashooting <- nba18 %>% 
  group_by(Player.Name, Shot.Zone.Range) %>% 
  filter(!Shot.Zone.Range == "Back Court Shot") %>% 
  summarise(n = n()) %>% 
  mutate(perc = round(n/sum(n),2)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Shot.Zone.Range, values_from = perc) %>% 
  replace(is.na(.),0) 

nbashooting %>% 
  ggcorr(label = TRUE)


nbashooting %>% 
  ggplot() + theme_economist() +
  geom_density(aes(`24+ ft.`), fill = "yellow", alpha = 0.3) +
  geom_density(aes(`16-24 ft.`), fill = "red", alpha = 0.3) + 
  geom_density(aes(`8-16 ft.`), fill = "blue", alpha = 0.3) + 
  geom_density(aes(`Less Than 8 ft.`), fill = "green", alpha = 0.3)

summary(nbashooting)

nbashooting %>% 
  filter(`24+ ft.` > 0.1875 & `24+ ft.` < 0.4900) %>% 
  ggplot() + theme_economist() +
  geom_density(aes(`24+ ft.`), fill = "yellow", alpha = 0.3) +
  geom_density(aes(`16-24 ft.`), fill = "red", alpha = 0.3) + 
  geom_density(aes(`8-16 ft.`), fill = "blue", alpha = 0.3) + 
  geom_density(aes(`Less Than 8 ft.`), fill = "green", alpha = 0.3)

nbashooting %>% 
  mutate(`16-24 ft.` = as.factor(round(`16-24 ft.`,1))) %>% 
  ggplot(aes(`Less Than 8 ft.`, `24+ ft.`, size = `8-16 ft.`, col = `16-24 ft.`)) + 
  geom_point(alpha = 0.5) + apricot_d()

jt <- rookies %>% 
  filter(Player.Name == "Jayson Tatum") %>% 
  group_by(Season) %>% 
  count(Action.Type) %>% summarise(n = sum(n))%>% mutate(Player.Name = "Jayson Tatum")

df <- rookies %>% 
  filter(Player.Name == "De'Aaron Fox") %>% 
  group_by(Season) %>% 
  count(Action.Type) %>% summarise(n = sum(n)) %>% mutate(Player.Name = "De'Aaron Fox")

dm <- rookies %>% 
  filter(Player.Name == "Donovan Mitchell") %>% 
  group_by(Season) %>% 
  count(Action.Type) %>% summarise(n = sum(n)) %>% mutate(Player.Name = "Donovan Mitchell")

rookies_shotstaken <- bind_rows(jt,df,dm) %>% select(Player.Name, Season, n)
rookies_shotstaken$Games.Played <- c(80,79,66,73,81,51, 79,77,69)

rookies_shotstaken %>% mutate(Shots.Per.Game = round(n/Games.Played,1))

rookies_action <- rookies %>% 
  group_by(Player.Name, Action.Type) %>% 
  summarise(n = n()) %>% 
  mutate(perc = round(n/sum(n),2)) %>% 
  filter(!perc < 0.05) %>% 
  arrange(Player.Name, -perc)

rookies %>% 
  group_by(Player.Name, Action.Type) %>% 
  summarise(n = n()) %>% 
  mutate(perc = round(n/sum(n),2)) %>% 
  filter(!perc < 0.07) %>% 
  arrange(Player.Name, -perc) %>% 
  ggplot(aes(Action.Type, perc)) + theme_economist_white() +
  geom_col(aes(fill = Player.Name), position = "dodge")


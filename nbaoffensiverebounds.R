# Libraries

library(tidyverse)
library(tidymodels)
library(GGally)
library(ggtea)
library(RColorBrewer)
library(ggrepel)
library(plotly)

# Data

nbaff <- as_tibble(nbaff)
nbaff

# Renaming features

nbaff <- nbaff %>% rename(n = ï..,
                 team = TEAM,
                 games = GP,
                 wins = W,
                 losses = L,
                 winrate = WIN.,
                 minutes = MIN,
                 efg = EFG.,
                 fta = FTA_RATE,
                 tov = TOV.,
                 off_reb = OREB.,
                 opp_efg = OPP_EFG.,
                 opp_fta = OPP_FTAÂ.RATE,
                 opp_tov = OPP_TOV.,
                 opp_off_reb = OPP_OREB) %>% 
  select(-n)

nbaff2 %>% ggcorr(label = TRUE)
nba <- nbaff2 %>% rename(TEAM = ï..TEAM)

# Defensive Statistics Only

nba1 <- nba %>% 
  select(c("TEAM","WIN.","PTS","OREB","DREB",
           "REB", "STL","BLK","BLKA","PFD")) %>% 
  as_tibble()

# Correlation Matrix

nba1 %>% ggcorr(label = TRUE) + 
  scale_fill_viridis_c()


# EDA

nba %>% ggplot(aes(REB, W)) + geom_point()
nba %>% ggplot(aes(OREB, W)) + geom_point()

# Adding Rankings

test <- nba %>% 
  mutate(RANK = if_else(STANDING < 5, 
                        "Elite", 
                        if_else(STANDING < 12, 
                                "Average", 
                                "Poor")))

# Defensive Rebounds vs. Win %

ggplot(test, aes(DREB, W)) + 
  geom_point(aes(col = RANK), size = 4) + 
  ggrepel::geom_label_repel(aes(label = TEAM)) + 
  apricot_d()

# Switching to plotly
# Defensive Rebounds

plot_ly(test, x = ~DREB, y = ~WIN., color = ~RANK,
        marker = list(size = 20),
        text = ~paste(TEAM))

# Offensive Rebounds

plot_ly(test, x = ~OREB, y = ~WIN., color = ~RANK,
        marker = list(size = 20),
        text = ~paste(TEAM))

# Steals

plot_ly(test, x = ~STL, y = ~WIN., color = ~RANK,
        marker = list(size = 20),
        text = ~paste(TEAM))

# Blocks

plot_ly(test, x = ~BLK, y = ~WIN., color = ~RANK,
        marker = list(size = 20),
        text = ~paste(TEAM))



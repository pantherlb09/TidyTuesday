# Gathering Data
pacman::p_load(ggplot2, tidyverse)


game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')
top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')
season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')


#### Total goals vs first year in the league ####
totgoals <- ggplot(top_250, aes(y = total_games, x = yr_start, fill = active, color = active)) +
  theme_bw(base_size = 16) + 
  xlab("Rookie Year") +
  ylab("Total Goals") +
  ggtitle("Total Number of Goals Scored") +
  scale_color_manual(values=c("goldenrod", "gray30")) +
  theme(legend.title = element_blank()) +
  geom_point(alpha = 0.7)
totgoals

ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_3_3_NHL","totgoals.png"))

#### Gretzky vs Ovechkin densities ####
gogoals <- ggplot(top_250, aes(y = total_games, x = yr_start, fill = active, color = active)) +
  theme_bw(base_size = 16) + 
  xlab("Rookie Year") +
  ylab("Total Goals") +
  ggtitle("Total Number of Goals Scored") +
  scale_color_manual(values=c("goldenrod", "gray30")) +
  theme(legend.title = element_blank()) +
  geom_point(alpha = 0.7)
gogoals
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_3_3_NHL","gogoals.png"))












#### Last ####











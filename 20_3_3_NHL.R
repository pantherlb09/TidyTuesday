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

#Want to highlight Ovechkin, Crosby, and Gretzky
totgoalshi <- ggplot(top_250, aes(y = total_games, x = yr_start)) +
  theme_bw(base_size = 16) + 
  xlab("Rookie Year") +
  ylab("Total Goals") +
  ggtitle("Total Number of Goals Scored") +
#  theme(legend.title = element_blank()) +
  geom_point(aes(fill = active, color = active, alpha = 0.5), show.legend = F) +
  geom_point(data = (top_250 %>% filter( player %in% c("Wayne Gretzky", "Alex Ovechkin", "Sidney Crosby"))), aes(x = yr_start, y = total_games, group = player, col = player), alpha = .6) +
  scale_color_manual(name = "Player", values=c("darkorchid", "red2", "gray30", "goldenrod", "dodgerblue3"))
totgoalshi
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_3_3_NHL","totgoals.png"))


#### Gretzky vs Ovechkin vs Sid densities ####


grovsi <- season_goals %>%
  filter(season_goals$player == "Wayne Gretzky" | season_goals$player == "Alex Ovechkin" | season_goals$player == "Sidney Crosby")


#Now can compare densities of each!

#Goals
gosgoals <- ggplot(grovsi, aes(x = goals/season_games, fill = player, color = player)) +
  theme_bw(base_size = 16) + 
  xlab("Goals/game") +
  ylab(NULL) +
  ggtitle("Goals Scored per Game per Season") +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.7)) +
  scale_color_manual(values=c("gray30", "black", "orange")) +
  scale_fill_manual(values=c("red2", "goldenrod", "dodgerblue3")) +
  geom_density(alpha = 0.4)
gosgoals
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_3_3_NHL","gosgoals.png"))

#Assists
gosassists <- ggplot(grovsi, aes(x = assists/season_games, fill = player, color = player)) +
  theme_bw(base_size = 16) + 
  xlab("Assists/game") +
  ylab(NULL) +
  ggtitle("Assists per Game per Season") +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.7)) +
  scale_color_manual(values=c("gray30", "black", "orange")) +
  scale_fill_manual(values=c("red2", "goldenrod", "dodgerblue3")) +
  geom_density(alpha = 0.4)
gosassists
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_3_3_NHL","gosassists.png"))

#Points
gospoints <- ggplot(grovsi, aes(x = points/season_games, fill = player, color = player)) +
  theme_bw(base_size = 16) + 
  xlab("Points/game") +
  ylab(NULL) +
  ggtitle("Points Scored per Game per Season") +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.7)) +
  scale_color_manual(values=c("gray30", "black", "orange")) +
  scale_fill_manual(values=c("red2", "goldenrod", "dodgerblue3")) +
  geom_density(alpha = 0.4)
gospoints
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_3_3_NHL","gospoints.png"))








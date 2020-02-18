#Installing and loading packages
pacman::p_load(tidyverse,gridExtra,ggplot2,ggimage)

#Bringing in data
attendance <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv")
standings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv")
games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv")


attendance_joined <- attendance %>%
  left_join(standings,
            by = c("year", "team_name", "team")
  )
#Need to rename some columns in games to join (home_team -> team, home_team_name -> team_name)
names(games)[names(games) == "home_team_name"] <- "team_name"
names(games)[names(games) == "home_team_city"] <- "team"

# Need to only keep regular season games, then can make week an integer and join
games$week <- gsub("[^0-9]", "", games$week) #deletes the naming on playoff games
games$week <- as.numeric(as.character(games$week))


joined <- attendance_joined %>%
  right_join(games,
            by = c("year", "team_name", "team", "week")
  )

joined <- joined[order(joined$year, joined$week, joined$team),]

#Incorporating NFL logos into plots - special thanks to @BenBBaldwin for script and @StatsByLopez for creating the file
logos <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")

#Need to separate team into 
names(logos)[names(logos) == "team"] <- "home_team"

joined <- joined %>%
  left_join(logos,
            by = c("home_team")
  )

joined <- joined %>% drop_na(weekly_attendance)
  
#Let's roll it up by team and year (average attendance per year per team)
byyear_team <- joined %>%
  group_by(year, home_team) %>%
  summarize(mean_attendance = mean(weekly_attendance))

#Adding in team codes, need to merge again after editing chargers and rams
byyear_team <- byyear_team %>%
  left_join(logos,
            by = c("home_team")
  )

#Need to give Chargers and Rams team codes prior to move to LA
byyear_team$team_code[byyear_team$home_team == "San Diego Chargers"] <- "LAC"
byyear_team$team_code[byyear_team$home_team == "St. Louis Rams"] <- "LA"

byyear_team <- byyear_team[ , !(names(byyear_team) %in% c("home_team","url"))]

byyear_team <- byyear_team %>%
  left_join(logos,
            by = c("team_code")
  )

#This creates a single plot of all of the teams attendance from every year
byyear_team %>%
  filter(!is.na(mean_attendance)) %>%
  ggplot(aes(year,
             mean_attendance,
             group=home_team)) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  labs(
    fill = NULL, x = NULL,
    y = "Weekly NFL game attendance"
  )

#Should split this into graphs for each division so it's easier to see - I made my own csv file here
setwd("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_2_04_NFL")
divisions <- read_csv("NFL_Divisions.csv")

byyear_team <- byyear_team %>%
  left_join(divisions,
            by = c("team_code")
  )

#Plots by year - AFC
AN <- byyear_team %>%
  filter(!is.na(mean_attendance)) %>%
  filter(combo == "AN") %>%  
  ggplot(aes(year,
             mean_attendance,
             group=home_team,
             ymin = 25000,
             ymax = 95000)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("black", "chocolate4", "orange", "purple")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

AE <- byyear_team %>%
  filter(!is.na(mean_attendance)) %>%
  filter(combo == "AE") %>%  
  ggplot(aes(year,
             mean_attendance,
             group=home_team,
             ymin = 25000,
             ymax = 95000)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("darkblue", "darkgreen", "darkturquoise", "red")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

AS <- byyear_team %>%
  filter(!is.na(mean_attendance)) %>%
  filter(combo == "AS") %>%  
  ggplot(aes(year,
             mean_attendance,
             group=home_team,
             ymin = 25000,
             ymax = 95000)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("aquamarine3", "blue", "red")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

AW <- byyear_team %>%
  filter(!is.na(mean_attendance)) %>%
  filter(combo == "AW") %>%  
  ggplot(aes(year,
             mean_attendance,
             group=home_team,
             ymin = 25000,
             ymax = 95000)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("azure4", "orange", "red", "steelblue")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

AFC <- grid.arrange(AN, AE, AS, AW, nrow = 2, top = "AFC")

#Plots by year - NFC
NN <- byyear_team %>%
  filter(!is.na(mean_attendance)) %>%
  filter(combo == "NN") %>%  
  ggplot(aes(year,
             mean_attendance,
             group=home_team,
             ymin = 25000,
             ymax = 95000)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("blue", "darkgreen", "navy", "purple")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

NE <- byyear_team %>%
  filter(!is.na(mean_attendance)) %>%
  filter(combo == "NE") %>%  
  ggplot(aes(year,
             mean_attendance,
             group=home_team,
             ymin = 25000,
             ymax = 95000)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("blue", "darkblue", "darkgreen", "darkred")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

NS <- byyear_team %>%
  filter(!is.na(mean_attendance)) %>%
  filter(combo == "NS") %>%  
  ggplot(aes(year,
             mean_attendance,
             group=home_team,
             ymin = 25000,
             ymax = 95000)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("black", "darkred", "goldenrod3", "steelblue")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

NW <- byyear_team %>%
  filter(!is.na(mean_attendance)) %>%
  filter(combo == "NW") %>%  
  ggplot(aes(year,
             mean_attendance,
             group=home_team,
             ymin = 25000,
             ymax = 95000)) +
  geom_line(aes(color = home_team)) +
  scale_color_manual(values= c("brown3", "darkblue", "darkred", "darkslategray4")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

NFC <- grid.arrange(NN, NE, NS, NW, nrow = 2, top = "NFC")

NFL <- grid.arrange(AFC, NFC, nrow = 1, top = "NFL average home attendance by year")


#I also collected stadium capacities for each team for each year
capacity <- read_csv("NFL_Capacity.csv")

byyear_team <- byyear_team %>%
  left_join(capacity,
            by = c("team_code", "year")
  )

byyear_team$proportion <- byyear_team$mean_attendance/byyear_team$capacity

#Need to change home_team, team_code, and url for STL and SD in joined prior to join
for (i in 1:length(joined$home_team)){
  if (joined$home_team[i] == "St. Louis Rams") {
    joined$home_team[i] <- "Los Angeles Rams"
  }
  if (joined$home_team[i] == "San Diego Chargers") {
    joined$home_team[i] <- "Los Angeles Chargers"
  }
}
for (i in 1:length(joined$home_team)){
  if (joined$home_team[i] == "Los Angeles Rams"){
    joined$team_code[i] <- "LA"
  }
  if (joined$home_team[i] == "Los Angeles Chargers"){
    joined$team_code[i] <- "LAC"
  }
}
for (i in 1:length(joined$home_team)){
  if (joined$home_team[i] == "Los Angeles Rams"){
    joined$url[i] <- "https://upload.wikimedia.org/wikipedia/en/thumb/8/8a/Los_Angeles_Rams_logo.svg/100px-Los_Angeles_Rams_logo.svg.png"
  }
  if (joined$home_team[i] == "Los Angeles Chargers"){
    joined$url[i] <- "https://upload.wikimedia.org/wikipedia/en/thumb/7/72/NFL_Chargers_logo.svg/100px-NFL_Chargers_logo.svg.png"
  }
}

byyear_team <- byyear_team %>%
  left_join(joined,
            by = c("home_team", "year", "team_code", "url")
  )

byyear_team <- byyear_team %>%
  select(year, team_code, home_team, url, wins, playoffs, combo, year_jitter, team_jitter, color, mean_attendance, offensive_ranking, defensive_ranking, capacity, sb_winner)

byyear_team <- byyear_team %>% distinct()

#Plots by year (wins) - AFC
ANp <- byyear_team %>%
  filter(!is.na(wins)) %>%
  filter(combo == "AN") %>%  
  ggplot(aes(year,
             wins,
             group=home_team,
             ymin = 0,
             ymax = 16)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("black", "chocolate4", "orange", "purple")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

AEp <- byyear_team %>%
  filter(!is.na(wins)) %>%
  filter(combo == "AE") %>%  
  ggplot(aes(year,
             wins,
             group=home_team,
             ymin = 0,
             ymax = 16)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("darkblue", "darkgreen", "darkturquoise", "red")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

ASp <- byyear_team %>%
  filter(!is.na(wins)) %>%
  filter(combo == "AS") %>%  
  ggplot(aes(year,
             wins,
             group=home_team,
             ymin = 0,
             ymax = 16)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("aquamarine3", "blue", "red")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

AWp <- byyear_team %>%
  filter(!is.na(wins)) %>%
  filter(combo == "AW") %>%  
  ggplot(aes(year,
             wins,
             group=home_team,
             ymin = 0,
             ymax = 16)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("azure4", "orange", "red", "steelblue")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

AFCp <- grid.arrange(ANp, AEp, ASp, AWp, nrow = 2, top = "AFC")

#Plots by year - NFC
NNp <- byyear_team %>%
  filter(!is.na(wins)) %>%
  filter(combo == "NN") %>%  
  ggplot(aes(year,
             wins,
             group=home_team,
             ymin = 0,
             ymax = 16)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("blue", "darkgreen", "navy", "purple")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

NEp <- byyear_team %>%
  filter(!is.na(wins)) %>%
  filter(combo == "NE") %>%  
  ggplot(aes(year,
             wins,
             group=home_team,
             ymin = 0,
             ymax = 16)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("blue", "darkblue", "darkgreen", "darkred")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

NSp <- byyear_team %>%
  filter(!is.na(wins)) %>%
  filter(combo == "NS") %>%  
  ggplot(aes(year,
             wins,
             group=home_team,
             ymin = 0,
             ymax = 16)) +
  geom_line(aes(color = color)) +
  scale_color_manual(values= c("black", "darkred", "goldenrod3", "steelblue")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

NWp <- byyear_team %>%
  filter(!is.na(wins)) %>%
  filter(combo == "NW") %>%  
  ggplot(aes(year,
             wins,
             group=home_team,
             ymin = 0,
             ymax = 16)) +
  geom_line(aes(color = home_team)) +
  scale_color_manual(values= c("brown3", "darkblue", "darkred", "darkslategray4")) +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    fill = NULL, x = NULL, y = NULL
  )

NFCp <- grid.arrange(NNp, NEp, NSp, NWp, nrow = 2, top = "NFC")

NFLp <- grid.arrange(AFCp, NFCp, nrow = 1, top = "NFL regular season wins by Year")

#Animation - total number of wins, and playoff appearances/super bowl winners each year
byyear_team <- na.omit(byyear_team)

byyear_team$wins_jit <- byyear_team$wins + byyear_team$team_jitter

byyear_team$play <- 0
for (i in 1:length(byyear_team$play)){
  if (byyear_team$playoffs[i] == "Playoffs") {
    byyear_team$play[i] <- byyear_team$play[i] + 2
  }
  if (byyear_team$sb_winner[i] == "Won Superbowl"){
    byyear_team$play[i] <- byyear_team$play[i] + 1
  }
}

byyear_team$playoffs_jit <- byyear_team$play + byyear_team$team_jitter

playoff_wins <- byyear_team %>%
  filter(!is.na(playoffs)) %>%
  ggplot(aes(wins_jit,
             playoffs_jit,
             xmin = -0.5,
             xmax = 16.5,
             ymin = -0.5,
             ymax = 3.5)) +
  annotate("rect", xmin = -0.5, xmax = 0.5, ymin = -0.5, ymax = 3.5,alpha = .4) +
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -0.5, ymax = 3.5,alpha = .4) +
  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = -0.5, ymax = 3.5,alpha = .4) +
  annotate("rect", xmin = 5.5, xmax = 6.5, ymin = -0.5, ymax = 3.5,alpha = .4) +
  annotate("rect", xmin = 7.5, xmax = 8.5, ymin = -0.5, ymax = 3.5,alpha = .4) +
  annotate("rect", xmin = 9.5, xmax = 10.5, ymin = -0.5, ymax = 3.5,alpha = .4) +
  annotate("rect", xmin = 11.5, xmax = 12.5, ymin = -0.5, ymax = 3.5,alpha = .4) +
  annotate("rect", xmin = 13.5, xmax = 14.5, ymin = -0.5, ymax = 3.5,alpha = .4) +
  annotate("rect", xmin = 15.5, xmax = 16.5, ymin = -0.5, ymax = 3.5,alpha = .4) +
  annotate("rect", xmin = -0.5, xmax = 16.5, ymin = 2.5, ymax = 3.5,alpha = .4, fill = 'goldenrod4') +
  annotate("rect", xmin = -0.5, xmax = 16.5, ymin = 1.5, ymax = 2.5,alpha = .4, fill = 'blue') +
#  geom_vline(xintercept = -0.5:15.5, colour="gray", linetype = "longdash") +
  geom_image(aes(image = url), size = 0.05) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_blank()) +
  labs(
    fill = NULL, x = "Wins", y = NULL
  ) 
playoff_wins

library(gganimate)

anim <- playoff_wins + 
  transition_states(year,
                    transition_length = 1,
                    state_length = 1.5) +
  ggtitle('Now showing {closest_state}') +
  ease_aes('cubic-in-out')

SB <- animate(anim, duration = 60)
SB

anim_save("SB.gif", SB)

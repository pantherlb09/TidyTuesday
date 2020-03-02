#20_2_18_Food_CO2
pacman::p_load(ggplot2, tidyverse, sf, stringr)

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

#Looking at what is in the data
table(measles$state)
table(measles$type)

#Need to delete the missing obs (-1)
measles$mmr <- na_if(measles$mmr, -1)
measles$overall <- na_if(measles$overall, -1)
measles$xrel <- na_if(measles$xrel, -1)
measles$xmed <- na_if(measles$xmed, -1)
measles$xper <- na_if(measles$xper, -1)

#Ohio only has data for public and private, so want to compare same at US level
pubpriv <- filter(measles, type == 'Public' | type == 'Private')
ohio <- filter(measles, state == 'Ohio')

#Want to make school district vaccination charts
#1. Bar graph by school type - all data and just Ohio
#2. Make density graphs to show more information available
#3. Map by school district - only public data, and need to learn how to import and use a shape file here

#### Plotting bar graphs ####
ggplot(pubpriv, aes(x = type, y = mmr, fill = type)) +
  theme_bw(base_size = 16) + 
  ylab("Vaccination Rate (MMR)") +
  xlab(NULL) +
  ggtitle("US Vaccination rates") +
  geom_bar(stat = "summary", fun.y = "mean")

ggplot(ohio, aes(x = type, y = mmr, fill = type)) +
  theme_bw(base_size = 16) + 
  ylab("Vaccination Rate (MMR)") +
  xlab(NULL) +
  ggtitle("Ohio Vaccination rates") +
  geom_bar(stat = "summary", fun.y = "mean")

#### Plotting bar graphs - with error bars ####

#Need to delete missing obs before taking summary stats
pubprivna <- pubpriv %>% drop_na(mmr)
ohiona <- ohio %>% drop_na(mmr)

#Need to capture mean and sd 
mmr_sum <- pubprivna %>% 
  group_by(type) %>%   
  summarise(mean = mean(mmr),  
            sd = sd(mmr))

mmr_sum_oh <- ohiona %>% 
  group_by(type) %>%   
  summarise(mean = mean(mmr),  
            sd = sd(mmr)) 

#Now I can plot the error bars
usbar <- ggplot(mmr_sum, aes(type, mean, fill = type)) + 
  coord_flip() +
  theme_bw(base_size = 16) + 
  xlab(NULL) +
  xlab("School Type") +
  ylab("Vaccination Rate (MMR)") +
  ggtitle("US Vaccination rates") +
  theme(legend.position = ('none')) +
  geom_col() +  
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=0.2, color = 'gray30')
usbar
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_2_25_MMR","usbar.png"))

ohbar <- ggplot(mmr_sum_oh, aes(type, mean, fill = type)) + 
  coord_flip() +
  theme_bw(base_size = 16) + 
  xlab("School Type") +
  ylab("Vaccination Rate (MMR)") +
  ggtitle("Ohio Vaccination rates") +
  theme(legend.position = ('none')) +
  geom_col() +  
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=0.2, color = 'gray30')
ohbar
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_2_25_MMR","ohbar.png"))

#### Plotting densities ####
usdens <- ggplot(pubpriv, aes(x = mmr, fill = type, color = type)) +
  theme_bw(base_size = 16) + 
  ylab(NULL) +
  xlab("Vaccination Rate (MMR)") +
  ggtitle("US Vaccination rates") +
  geom_density(alpha = 0.4)
usdens
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_2_25_MMR","usdens.png"))

ohdens <- ggplot(ohio, aes(x = mmr, fill = type, color = type)) +
  theme_bw(base_size = 16) + 
  ylab(NULL) +
  xlab("Vaccination Rate (MMR)") +
  ggtitle("Ohio Vaccination rates") +
  geom_density(alpha = 0.4)
ohdens
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_2_25_MMR","ohdens.png"))

#### Plotting private school only bar ####
priv <- filter(measles, type == 'Private')
ohiopriv <- filter(priv, state == 'Ohio')

#dropping missing
privna <- priv %>% drop_na(mmr)
ohioprivna <- ohiopriv %>% drop_na(mmr)

#Need to capture mean and sd 
priv_mmr_sum <- privna %>% 
  group_by(type) %>%   
  summarise(mean = mean(mmr),  
            sd = sd(mmr))

priv_mmr_sum_oh <- ohioprivna %>% 
  group_by(type) %>%   
  summarise(mean = mean(mmr),  
            sd = sd(mmr)) 

#Now I can plot the error bars
usbarpriv <- ggplot(priv_mmr_sum, aes(type, mean, fill = type)) + 
  coord_flip() +
  theme_bw(base_size = 16) + 
  xlab(NULL) +
  xlab("School Type") +
  ylab("Vaccination Rate (MMR)") +
  ggtitle("US Vaccination rates") +
  theme(legend.position = ('none')) +
  geom_col() +  
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=0.2, color = 'gray30')
usbarpriv
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_2_25_MMR","usbarpriv.png"))

ohbarpriv <- ggplot(priv_mmr_sum_oh, aes(type, mean, fill = type)) + 
  coord_flip() +
  theme_bw(base_size = 16) + 
  xlab("School Type") +
  ylab("Vaccination Rate (MMR)") +
  ggtitle("Ohio Vaccination rates") +
  theme(legend.position = ('none')) +
  geom_col() +  
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=0.2, color = 'gray30')
ohbarpriv
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_2_25_MMR","ohbarpriv.png"))

#### Plotting private school only density ####
usdenspriv <- ggplot(priv, aes(x = mmr, fill = type, color = type)) +
  theme_bw(base_size = 16) + 
  ylab(NULL) +
  xlab("Vaccination Rate (MMR)") +
  ggtitle("US Vaccination rates") +
  geom_density(alpha = 0.4)
usdenspriv
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_2_25_MMR","usdenspriv.png"))

ohdenspriv <- ggplot(ohiopriv, aes(x = mmr, fill = type, color = type)) +
  theme_bw(base_size = 16) + 
  ylab(NULL) +
  xlab("Vaccination Rate (MMR)") +
  ggtitle("Ohio Vaccination rates") +
  geom_density(alpha = 0.4)
ohdenspriv
ggsave(filename = file.path("C:/Users/Beau/OneDrive - University of Cincinnati/TidyTuesday/20_2_25_MMR","ohdenspriv.png"))


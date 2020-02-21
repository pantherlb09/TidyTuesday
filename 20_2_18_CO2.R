#20_2_18_Food_CO2
pacman::p_load(ggplot2, tidyverse, echarts4r)

fc <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv")

fc
#Have Country, Food Type, Amount Consumed, and Amount of Emissions created
#Would like to create some kind of bar graph for type consumed, and emission per consumption?
#Also would like to make a map of total consumption, total emissions, and total emission/consumption

#First step, create emission/consumption

fc$emispcons <- fc$co2_emmission/fc$consumption
fc
#Need to relabel food_categories so they can fit on the bar chart and not take up so much space
fc$food_category <- recode(fc$food_category, "Wheat and Wheat Products" = "Wheat")
fc$food_category <- recode(fc$food_category, "Nuts inc. Peanut Butter" = "Nuts")
fc$food_category <- recode(fc$food_category, "Milk - inc. cheese" = "Milk")

#Creating bar graphs of summary stastics for USA, then World

us <- filter(fc, country == "USA")
#Consumption
uscon <- ggplot(us, aes(x=food_category)) + 
  theme_bw(base_size = 16) + 
  geom_bar(position="dodge", stat="identity", fill = "blue", aes(y=consumption)) +
  labs(x = NULL, y = "Consumption") +
  theme(axis.text.x = element_text(angle = 45 ,hjust = 1)) +
  theme(legend.position = ('none')) 
uscon
#Emissions
usemi <- ggplot(us, aes(x=food_category)) + 
  theme_bw(base_size = 16) + 
  geom_bar(position="dodge", stat="identity", fill = "brown", aes(y=us$co2_emmission)) +
  labs(x = NULL, y = "Emissions") +
  theme(axis.text.x = element_text(angle = 45 ,hjust = 1)) +
  theme(legend.position="top") 
usemi
#Emissions/Consumption
usepc <- ggplot(us, aes(y=emispcons, x=food_category)) + 
  theme_bw(base_size = 16) + 
  geom_bar(position="dodge", stat="identity", fill = "seagreen3") +
  labs(x = NULL, y = "Emis per Cons") +
  theme(axis.text.x = element_text(angle = 45 ,hjust = 1)) +
  theme(legend.position = ('none')) 
usepc

#Low alpha to see both on top of each other
useac <- ggplot(us, aes(x=food_category)) + 
  theme_bw(base_size = 16) + 
  geom_bar(position="dodge", stat="identity", fill = "grey35", aes(y=us$co2_emmission, alpha = 0.4)) +
  geom_bar(position="dodge", stat="identity", fill = "blue", aes(y=consumption, alpha = 0.4)) +
  labs(x = NULL, y = "CO2/Cons", colour = NULL, title = "US CO2 Emissions and Consumption", caption = "Emissions and consumption is kg/person/year") +
  theme(axis.text.x = element_text(angle = 45 ,hjust = 1), legend.position = "none") +
  scale_fill_manual("legend", labels = c("CO2", "Cons"), values = c("consumption" = "blue", "co2_emmission" = "grey35"))
useac

ggsave("co2stacked.png", plot = useac, dpi = 600)

#Need to reformat data to create dual axis bar graph with consumption and emissions
uslong <- us %>%
  pivot_longer(-consumption, names_to = "type", values_to = "value")

#Had trouble with this conversion, so manually converted in Excel to long format

setwd("C:/Users/sauleybu/OneDrive - University of Cincinnati/TidyTuesday/20_2_18_CO2")
long <- read.csv("CO2_long.csv", header = T)

long$food_category <- recode(long$food_category, "Wheat and Wheat Products" = "Wheat")
long$food_category <- recode(long$food_category, "Nuts inc. Peanut Butter" = "Nuts")
long$food_category <- recode(long$food_category, "Milk - inc. cheese" = "Milk")

usl <- filter(long, country == "USA")


useacdual <- ggplot(usl, aes(x=food_category, y=amount, fill = type)) + 
  theme_bw(base_size = 16) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x = NULL, y = "Emissions", colour = NULL, title = "US CO2 Emissions and Consumption", caption = "Emissions and consumption is kg/person/year") +
  theme(axis.text.x = element_text(angle = 45 ,hjust = 1), legend.title = element_blank()) +
  scale_fill_manual("legend", labels = c("CO2", "Cons"), values = c("consumption" = "blue", "co2_emmission" = "grey35")) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.25, name = "Consumption"))

useacdual

ggsave("co2dual.png", plot = useacdual, dpi = 600)


######################
### Creating Maps ####
######################

#Altered code provided by @Ruben46563154 using echarts4r

map <- fc %>% 
  group_by(country) %>%
  summarise(co2_total = sum(co2_emmission),
            consumption_total = sum(consumption)) %>%
  mutate(country = recode_factor (country,
                                `USA` = "United States",
                                `Czech Republic`= "Czech Rep.",
                                `South Korea`= "Korea"))
#Emissions
mapE <- map %>%
  e_charts(country) %>%
  e_map(co2_total) %>%
  e_visual_map(min=0, max=2200) %>%
  e_title("Total CO2 emissions due to food products \n (kg CO2/person/year)", left = "center") %>%
  e_theme("chalk")
mapE

#Consumption
mapC <- map %>%
  e_charts(country) %>%
  e_map(consumption_total) %>%
  e_visual_map(min=0, max=650) %>%
  e_title("Total consumption of food products \n (kg/person/year)", left = "center") %>%
  e_theme("dark")
mapC

#Need to learn how to save these and keep interactive functionality
#ggsave("mapCO2.png", plot = mapE, dpi = 600)
#ggsave("mapcons.png", plot = mapC, dpi = 600)

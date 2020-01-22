library(pacman)
pacman::p_load(choroplethrMaps, choroplethr, maps, gdata, tidyverse, tidytext, 
               lubridate, tidyr, dplyr, stringr, ggplot2, lubridate, scales, lmtest, 
               rebus, rvest, foreign, Hmisc, janitor, geofacet, zipcode)

###################################################
#     National name data from 1918 to 2018        #
###################################################
# Load in national name data
national_name_data_original <- read.csv("data/nationalNames/nationalNames.csv")
national_name_data <- national_name_data_original 

# Reset row index
rownames(national_name_data) <- seq(length=nrow(national_name_data))
# Only top 10 names for each state in each year
national_name_data <- national_name_data %>% group_by(sex, year) %>%
  mutate(rank = row_number(), firstLetter = substr(name, 1, 1), length = str_length(name)) %>% 
  filter(rank <51) 

ggplot(subset(national_name_data, firstLetter == "J" & sex == "M"), 
       aes(x=as.numeric(year), y=as.numeric(number), fill=name)) + 
  geom_bar(stat="identity") +
  #  facet_geo(~ state, scales = "free_y")
  labs(title = "Most popular names of men from 1918 to 2018",
       caption = "Data Source: data.gov",
       x = "Year",
       y = "Count") 

# Load in state name data
state_name_data_original <- read.csv("data/namesByState/stateNames.csv")
state_name_data <- state_name_data_original 

# Limit state data to 100 years
state_name_data <- subset(state_name_data, year > 1917)
# Reset row index
rownames(state_name_data) <- seq(length=nrow(state_name_data))
# Only top 10 names for each state in each year
state_name_data <- state_name_data %>% group_by(state, sex, year) %>%
  mutate(rank_by_state = row_number()) %>% add_tally() %>% filter(rank_by_state < 11) 

# Count males and females in state data
nrow(subset(state_name_data, sex == "M"))
nrow(subset(state_name_data, sex == "F"))


# Write dataframe as CSV file
write.csv(state_name_data,'data/namesByState/stateNamesClean.csv')

ggplot(subset(state_name_data, rank_by_state < 11 & sex == "M"), 
       aes(x=as.numeric(year), y=as.numeric(number), fill=name)) + 
  geom_bar(stat="identity") +
#  facet_geo(~ state, scales = "free_y")
  labs(title = "Most popular names of men from 1918 to 2018",
       caption = "Data Source: data.gov",
       x = "Year",
       y = "Count") 
       
ggexport(plotlist = list(), filename = "plots/stateNamePlots.pdf")

ggplot(data = all_semester_stats,
       aes(fill=sems,x=as.numeric(hours), y= as.numeric(mean))) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=as.numeric(mean)-as.numeric(sd), color="std", ymax=as.numeric(mean)+as.numeric(sd)), 
                width=.1, position=position_dodge(.9)) + 
  scale_x_continuous(labels=c("0","6","12","18","24"),breaks=c(0,6,12,18,24)) + 
  scale_y_continuous() +
  xlab("Time") + 
  facet_grid(sems ~ days) +
  #  facet_wrap(~ days, nrow=1) +
  ylab("Mean") + labs(color = "sems") + 
  scale_fill_manual("sems", values = c("spring" = "midnightblue",
                                       "summer" = "mediumvioletred",
                                       "fall" = "mediumturquoise")) + 
  theme(legend.position = "bottom",legend.text = element_text(size = 8),
        legend.title=element_blank(),legend.key.size = unit(0.7,"line")) + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE))


library(tidyverse)
library(here)

#Instructions: 
# Choose a single year and a single state.  
# Download the most detailed FARS data set you can for this year in this state of choice.  
# Familiarize yourself with the data set and come to class with a basic R script (plain old R,
# please, not R Markdown and not Python) that does just a little exploratory data analysis (EDA).  
# For example, how many accidents are represented in your data set?  How many individuals
# died (the most severe injury)?  

#https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/FARS/ 


accident <- read.csv(here::here("class_1_jan_17/FARS1986NationalCSV/ACCIDENT.CSV"))
miacc <- read.csv(here::here("class_1_jan_17/FARS1986NationalCSV/MIACC.CSV"))
#Crash level Alcohol data derived from driver and non-occupant records in MIPER 
# ST_CASE - State Case Number 
# A1 -A10 - 10 Imputed Person BAC Values
midravacc <- read.csv(here::here("class_1_jan_17/FARS1986NationalCSV/MIDRVACC.CSV"))
#Crash level alcohol data derived from driver records in the MIPER file 
# ST_CASE - State Case Number 
# A1 -A10 - 10 Imputed Person BAC Values
miper <- read.csv(here::here("class_1_jan_17/FARS1986NationalCSV/MIPER.CSV"))
#Person level alchol data 
# ST_CASE - State Case Number 
# VEH_NO - Vehicle Number 
# PER_NO - Person Number 
# P1-P10 - 10 Imputed Person BAC Values 
person <- read.csv(here::here("class_1_jan_17/FARS1986NationalCSV/PERSON.CSV"))
vehicle <- read.csv(here::here("class_1_jan_17/FARS1986NationalCSV/VEHICLE.CSV"))

summary(accident) 

stateid <- data.frame(states =  c("Alabama", "Alaska", "Arizona","Arkansas", "California", "Colorado", 
                       "Connecticut","Delaware", "District of Columbia", "Florida", "Georgia", 
                       "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
                       "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                       "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                       "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                       "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", 
                       "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                       "Virgin Islands", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                       "Wyoming"), 
           num = c(1, 2, 4, 5, 6, 8,9, 10, 11, 12, 13, 15, 16, 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,
                   37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,51,53,54,55,56))

accident <- accident %>% left_join(stateid, by = c("STATE" = "num"))
#(41090, 48) 
#accident %>% filter(if_any(states, is.na)) 


accident %>% 
  group_by(states) %>%  
  count() %>% 
  ggplot(aes(states, n)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1))

accident %>% group_by(states) %>% 
  summarize(n = sum(PERSONS)) %>% 
  arrange(desc(n))
  
accident %>% group_by(HOUR) %>% 
  count() %>% 
  arrange(desc(n))


accident %>% group_by(HARM_EV) %>% 
  count() %>% 
  arrange(desc(n))
#Motor Vehicle in Transport, Pedestrian, Overturn, Tree(standing only), Utility Pole, Guardrail, Pedalcycle, 

accident %>% group_by(WEATHER) %>% 
  count() %>% 
  arrange(desc(n))

accident %>% group_by(states) %>% 
  summarize(n = sum(FATALS)) %>% 
  arrange(desc(n))


#----Person 

person <- person %>% left_join(stateid, by = c("STATE" = "num"))

person %>% 
  group_by(DEATH_MO) %>% 
  count() %>% 
  filter(DEATH_MO > 0 & DEATH_MO <13) %>% 
  ggplot(aes(DEATH_MO, n)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(size = 10))

person  %>% filter(DEATH_MO == 0) %>% count() #62985
person %>% filter(DEATH_MO != 0 | DEATH_MO != 99) %>% count() #109073
62985/109073 #57.8% living 


person <- person %>% mutate(
  age_groups = case_when(
    AGE < 18 ~ '<18', 
    AGE >= 18 & AGE <= 25 ~ '18-25', 
    AGE > 25 & AGE <=35 ~ '26-35', 
    AGE > 35 & AGE <= 45 ~ '36-45', 
    AGE > 45 & AGE <= 60 ~ '46-60', 
    AGE > 60 ~ '60+'
    )
  )

person %>% group_by(age_groups) %>% count() %>% 
  ggplot(aes(age_groups, n)) + geom_bar(stat= 'identity')


person %>% group_by(SEX) %>% filter(SEX == 1 | SEX ==2) %>% count() %>% 
  ggplot(aes(SEX, n)) + geom_bar(stat= 'identity') #1 - Male, 2- Female 

#---Vehicle 

vehicle %>% group_by(DR_DRINK) %>%count()
#0 - no drinking 
#1 - drinking 

vehicle %>% group_by(PREV_ACC) %>% count()
#0 - none 
#1-97 actual count 
#99 - unknown 



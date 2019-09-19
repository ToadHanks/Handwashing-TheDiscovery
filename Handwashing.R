#Author: Mihir Patel

#Load in  packages
library(tidyverse)

#Reading yearly_deaths_by_clinic.csv into yearly
yearly <- read_csv("yearly_deaths_by_clinic.csv")

#Print out yearly
print(yearly)

#Getting the number of deaths
#Adding a new column to yearly with proportion of deaths per no. births
yearly = yearly %>%mutate(proportion_deaths = deaths / births)

#Print out yearly
print(yearly)

#Death at the clinic
options(repr.plot.width=7, repr.plot.height=4)
#Plotting yearly proportion of deaths at the two clinics
ggplot(yearly, aes(x=year,y = proportion_deaths,color=clinic))+geom_line()

#Handwashing is introduced

#Read monthly_deaths.csv into monthly
monthly <- read_csv("monthly_deaths.csv")

#Adding a new column with proportion of deaths per no. births
monthly = monthly %>%mutate(proportion_deaths = deaths / births)
#First rows in monthly
head(monthly)

#Effect of handwashing seen through plot
ggplot(monthly, aes(x=date,y = proportion_deaths))+geom_line()

#Highlightinh the effects if handwashing

#Apparently, from this date handwashing was made mandatory
handwashing_start = as.Date('1847-06-01')

#Added a TRUE/FALSE column to monthly called handwashing_started
monthly = monthly%>%mutate(handwashing_started = (date >= handwashing_start))

#Plotting monthly proportion of deaths before and after handwashing
ggplot(monthly, aes(x = date, y = proportion_deaths, color = handwashing_started))+geom_line()

#National movement begins for handwashing

#Calculating the mean proportion of deaths before and after handwashing.
monthly_summary <- monthly %>% group_by(handwashing_started)%>%
  summarise(mean_proportion_deaths = mean(proportion_deaths))

#Printing out the summary
print(monthly_summary)

#Calculating a 95% Confidence intrerval using t.test 
test_result <- t.test( proportion_deaths ~ handwashing_started, data = monthly)
print(test_result)
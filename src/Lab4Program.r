# Name: Dillon Thoma
# Date: 12 October 2018
# This work is mine unless otherwise cited.

# Run the below only if the library is not already installed.
# install.packages(dslabs)

library(dslabs)
library(dplyr)
library(tidyverse)
data(us_contagious_diseases)

#Question 1.

dat <- filter(us_contagious_diseases, disease == "Measles", state != "Alaska" & state != "Hawaii")
dat <- mutate(dat, per100000rate = ((count*100000)/population) * ((weeks_reporting)/52))

#filtering the data to specifically measles in all states but alaska and hawaii
#mutating the data using the formula for per100000rate in the pdf

#Question 2.

data_cali <- filter(dat, disease == "Measles", state == "California")
ggplot(data = data_cali, mapping = aes(x = year, y = per100000rate)) + geom_line() + geom_vline(xintercept = 1965)

#filtering the data to specifically measles in the state of california
#plotting the created data set data_cali by year and per100000rate

#Question 3.

dat_caliFocus <- filter(us_contagious_diseases, state == "California")

dat_caliFocus$yearBlock[dat_caliFocus$year >= 1950] <- "1950’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1960] <- "1960’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1970] <- "1970’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1980] <- "NA"

dat_caliFocus <- filter(dat_caliFocus, yearBlock != "NA")

ggplot(data = dat_caliFocus) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), 
position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, 
hjust = 1, vjust=-0.01))

ggplot(data = dat_caliFocus) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), 
position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, 
hjust = 1, vjust=-0.01))

#filtering the data to strictly california
#selecting the specified years that are required for the data set
#filtering out the na responses from the data
#plotting the new data set using a bar graph
#plotting the same data using the square root function on the count

#Question 4.

dat_Focus <- filter(us_contagious_diseases)

dat_Focus$yearBlock[dat_Focus$year >= 1950] <- "1950's"
dat_Focus$yearBlock[dat_Focus$year >= 1960] <- "1960's"
dat_Focus$yearBlock[dat_Focus$year >= 1970] <- "1970's"
dat_Focus$yearBlock[dat_Focus$year >= 1980] <- "NA"

dat_Focus <- filter(dat_Focus, yearBlock != "NA")

ggplot(data = dat_Focus) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), 
position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, 
hjust = 1, vjust=-0.01))

#this is the same justification as number three, except a new data set was created to include all data, not just california
                                                                                                                                                                          
#Question 5.

dat_Focus <- filter(us_contagious_diseases)

dat_Focus$yearBlock[dat_Focus$year >= 1950] <- "1950's"
dat_Focus$yearBlock[dat_Focus$year >= 1960] <- "1960's"
dat_Focus$yearBlock[dat_Focus$year >= 1970] <- "1970's"
dat_Focus$yearBlock[dat_Focus$year >= 1980] <- "NA"

dat_Focus <- filter(dat_Focus, yearBlock != "NA")

dat_Focus <- mutate(dat_Focus, per100000rate = ((count*100000)/population) * (weeks_reporting/52))

ggplot(data = dat_Focus) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), 
position = "dodge", stat = "identity") + geom_tile(mapping = aes(x=state, y=count, color = per100000rate)) + 
theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#this is the same justification as number four, plus the addition of the geom_tile which looks more closely at the per100000rate on the previous graph

#Question 6.

autismData <- read.csv("~/cs301F2018/classDocs/labs/04_lab/04-i-lab-cs301-fall-2018-thomad74/src/autismData.csv", comment.char="#")

dat_Autism <- filter(autismData)

ggplot(data = dat_Autism) + geom_line(mapping = aes(x = Year.., y = Net.Growth))

#the first step is importing the data to rstudio from my files
#filter the data into a new set called dat_Autism
#create a graph that takes the autismData and graphs it based on net growth by year

#The graph shows an increase in autism from 1985 to 2006, but that does not necessarily mean that it is due
#to the vaccines. It may be because of more people understanding the diagnoses and reporting them, as well as
#medical advancements that allow people to be more informed. There is no real data that can 100% prove that
#autism is directly related to vaccines, but there is in fact an increase in autism cases as vaccinations increased.
#Correlation does not imply causation.


# Name: Dillon Thoma
# Date: 12 October 2018

# Run the below only if the library is not already installed.
# install.packages(dslabs)

library(dslabs)
library(dplyr)
library(tidyverse)
data(us_contagious_diseases)

#Question 1.
dat <- filter(us_contagious_diseases, disease == "Measles", state != "Alaska" & state != "Hawaii")
dat <- mutate(dat, per100000rate = ((count*100000)/population) * ((weeks_reporting)/52))
#Question 2.
ggplot(data = dat, mapping = aes(x = year, y = per100000rate)) + geom_line() + geom_vline(xintercept = 1965)
#Question 3.
dat_caliFocus <- filter(us_contagious_diseases, state == "California")
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1950] <- "1950’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1960] <- "1960’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1970] <- "1970’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1980] <- "NA"

ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), 
position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, 
hjust = 1, vjust=-0.01))
#Question 4.

#Question 5.

#Question 6.






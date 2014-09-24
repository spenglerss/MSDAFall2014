# Sandesh Sadalge
# Week 4 Assignment

# ANSWER:



#  ********** SCRATCH WORK ************

require(hflights)

library(ggplot2)

library(plyr)
m <- tbl_df(movies)

library(dplyr)
a <- group_by(m, year)

a

dim(hflights)

group_by(hflights, Year, Month, DayofMonth)

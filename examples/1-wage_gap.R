library(dplyr)
library(stats)
library(spatstat)
library(quantmod)

data = data[data$WKSWORK2 != "N/A",]

overview_all = group_by(data, SEX) %>% 
  summarise(
    n = length(PERWT),
    population = sum(PERWT),
    mean = weighted.mean(INCWAGE, PERWT),
    median = weighted.median(INCWAGE, PERWT)
  )

overview_both = group_by(data, WKSWORK2) %>% 
  summarise(
    n = length(PERWT),
    population = sum(PERWT),
    mean = weighted.mean(INCWAGE, PERWT),
    median = weighted.median(INCWAGE, PERWT)
  )

overview_gender = group_by(data, WKSWORK2, SEX) %>% 
  summarise(
    n = length(PERWT),
    population = sum(PERWT),
    mean = weighted.mean(INCWAGE, PERWT),
    median = weighted.median(INCWAGE, PERWT)
  )





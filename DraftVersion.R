
library(dplyr)
library(ggplot2)
library(tidyr)


emissions <- read.csv("data/emissions.csv")

str(emissions)

colSums(is.na(emissions))

head(emissions)

emissions <- emissions %>%
  mutate(year_centered = year - mean(year, na.rm = TRUE))

emissions <- emissions %>%
  mutate(commodity = as.factor(commodity))

summary(emissions)

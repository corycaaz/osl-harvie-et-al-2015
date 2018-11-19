####### SETUP WORKSPACE #######

library(tidyverse)
library(psych)
library(car)

# Set working directory
setwd("~/GitHub/osl-harvie-et-al-2015/src")

# Import dataset
harvie <- read_csv("../data/Harvie et al. 2015.csv")

####### DESCRIPTIVE STATISTICS #######

(harvie_desc <- harvie %>%
  select(ends_with("Feedback")) %>%
  describe()
)
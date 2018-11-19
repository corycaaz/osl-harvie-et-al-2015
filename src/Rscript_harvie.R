####### SETUP WORKSPACE #######

library(tidyverse)
library(psych)

# Set working directory
setwd("~/GitHub/osl-harvie-et-al-2015/src")

# Import dataset
harvie <- read_csv("../data/Harvie et al. 2015.csv")

####### CLEAN DATA #######

# Rename variables
(harvie <- harvie %>%
  rename(
    participant = Participant,
    direction_rotation = DirectionofRotation,
    understated_feedback = Understated_Visual_Feedback,
    accurate_feedback = Accurate_Visual_Feedback,
    overstated_feedback = Overstated_Visual_Feedback
  ))

# Create long form
(harvie_clean <- harvie %>% 
  gather(key = feedback_type, value = pain_onset,
         -c(participant, direction_rotation, accurate_feedback))
)

# Set variables as factors
harvie_clean <- within(harvie_clean, {
  direction_rotation <- factor(direction_rotation)
  feedback_type <- factor(feedback_type)
  })

# Check factor
class(harvie_clean$feedback_type)

####### DESCRIPTIVE STATISTICS #######

(harvie_desc <- harvie %>%
  select(ends_with("Feedback")) %>%
  describe()
)

####### ONE-WAY REPEATED MEASURES ANOVA #######

### Significant Outliers?
### Normality?
### Sphericity?
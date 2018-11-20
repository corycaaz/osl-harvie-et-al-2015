####### SETUP WORKSPACE #######

library(tidyverse)
library(psych)
library(broom)

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
           -c(participant, direction_rotation))
)

# Set variables as factors
harvie_clean <- within(harvie_clean, {
  direction_rotation <- factor(direction_rotation)
  feedback_type <- factor(feedback_type)
  participant <- factor(participant)
  })

####### DESCRIPTIVE STATISTICS #######

(harvie_desc <- harvie %>%
  select(ends_with("Feedback")) %>%
  describe()
)

####### ONE-WAY REPEATED MEASURES ANOVA #######

# Create the analysis of variance object
harvie_aov <- aov(pain_onset ~ feedback_type + Error(participant/direction_rotation),
                  data = harvie_clean)

# Summarize the aov object
summary(harvie_aov)

# Extract sum of squares
(harvie_aov_tidy <- tidy(harvie_aov))

# Collect sum of squares
harvie_aov_tidy$sumsq[3]
harvie_aov_tidy$sumsq[4]

# Calculate partial eta-squared
(harvie_eta <- harvie_aov_tidy$sumsq[3] / (harvie_aov_tidy$sumsq[3] + harvie_aov_tidy$sumsq[4]))

### Significant Outliers?
### Normality?
### Sphericity?
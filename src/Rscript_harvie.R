####### SETUP WORKSPACE #######

library(tidyverse)
library(psych)
library(broom)
library(car)
library(nlme)
library(multcomp)

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
ggplot(harvie_clean, aes(x = feedback_type, y = pain_onset)) +
  geom_boxplot()

### Normality?
shapiro.test(harvie$understated_feedback)
shapiro.test(harvie$overstated_feedback)

### Sphericity?
harvie_matrix <- as.matrix(harvie)
harvie_matrix <- harvie_matrix[, -c(1, 2)]
model <- lm(harvie_matrix ~ 1)
design <- factor(c("understated_feedback", "accurate_feedback", "overstated_feedback"))
options(contrasts = c("contr.sum", "contr.poly"))
results <- Anova(model, idata = data.frame(design), idesign = ~design, type="III")
summary(results, multivariate = FALSE)

####### PAIRWISE COMPARISONS #######

# Construct repeated measures ANOVA
lme_harvie <- lme(pain_onset ~ feedback_type, 
                  random = ~1|participant/direction_rotation, data = harvie_clean)

# Perform pairwise comparisons with Bonferroni correction
summary(glht(lme_harvie, linfct=mcp(feedback_type = "Tukey")),
        test = adjusted(type = "bonferroni"))

####### VISUALIZE #######

# Construct the APA theme
apa_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12))

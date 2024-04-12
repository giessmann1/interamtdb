library(ggplot2)
library(dplyr)
library(patchwork)

sector_preference <- c("private","public", "public", "public", "public", "public", "public", "private", "private", "private", "private", "private")
sector_affiliation <- c("public", "private","public", "public", "public", "private", "private", "private", "private", "private", "public", "public") 
sec_pref_met <- c("No", "No", "Yes", "Yes", "Yes", "No", "No", "Yes", "Yes", "Yes", "No", "No")
signaling <- c(3, -3, 5, 3, -5, 5, -5, -5, -3, 5, -5, 5)
intention_to_apply <- c(0.71, 0.71, 5, 4.29, 1.43, 3.57, 0, 5, 4.29, 1.43, 3.57, 0)
manipulated <- c("No", "No", "No", "No", "Yes", "Yes", "No", "No", "No", "Yes", "Yes", "No")

data <- as.data.frame(cbind(sector_preference, sector_affiliation, sec_pref_met, signaling, intention_to_apply, manipulated))
data$signaling <- as.numeric(data$signaling)
data$intention_to_apply <- as.numeric(data$intention_to_apply)
data$sector_preference <- as.factor(data$sector_preference)
data$sector_affiliation <- as.factor(data$sector_affiliation)

plot_1 <- ggplot(data %>% filter(sector_preference == 'public'), aes(y = intention_to_apply, x = signaling, colour = sector_affiliation)) + 
  geom_point() +
  geom_line() +
  ylim(0, 5) +
  xlim(-5, 5) +
  ggtitle("sector_preference == 'public'")

plot_2 <- ggplot(data %>% filter(sector_preference == 'private'), aes(y = intention_to_apply, x = signaling, colour = sector_affiliation)) + 
  geom_point() +
  geom_line() +
  ylim(0, 5) +
  xlim(-5, 5) +
  ggtitle("sector_preference == 'private'")

plot_1 + plot_2 + plot_layout(guides = "collect")

# Hypothesis 2:
ggplot(data %>% filter(manipulated == 'No'), aes(y = intention_to_apply, x = as.factor(sec_pref_met), colour = signaling)) + 
  geom_boxplot(width = 0.3) +
  ylim(0, 5)

# Hypothesis 3:
intention_to_apply.lm <- lm(intention_to_apply ~ sector_preference*signaling, data %>% filter(sec_pref_met == 'Yes' & manipulated == 'No'))
summary(intention_to_apply.lm)

# Combined:
intention_to_apply.lm <- lm(intention_to_apply ~ sector_preference*signaling + sector_preference*sector_affiliation, data %>% filter(manipulated == 'No'))
summary(intention_to_apply.lm)

unique(data[c("sector_affiliation", "signaling")])


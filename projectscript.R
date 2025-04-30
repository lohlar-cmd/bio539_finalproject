#question 1:evaluate genetic variability for resistance to FAW among early-maturing white maize inbred lines under infested environment (natural and artificial infestations).

# Load necessary libraries
library(dplyr)
library(tidyverse)

# Load the data
maize_data <- read.csv("MAIZE DATA.csv")

# Inspect the structure of the data
str(maize_data)
summary(maize_data)

# Ensure categorical variables are factors
maize_data <- maize_data %>%
  mutate(
    hybrid = as.factor(hybrid),
    env = as.factor(env),
    rep = as.factor(rep)
  )

# Add environment labels (if not already present)
maize_data <- maize_data %>%
  mutate(env_label = ifelse(env == 1, "Infested", "Controlled"))

# Group data by environment and hybrid
fa_worm_variability <- maize_data %>%
  group_by(env_label, hybrid) %>%
  summarise(
    mean_EASP = mean(EASP, na.rm = TRUE), # Mean FAW resistance score
    sd_EASP = sd(EASP, na.rm = TRUE),     # Standard deviation of EASP
    var_EASP = var(EASP, na.rm = TRUE)    # Variance of EASP
  )

# Visualize variability using violin plots
ggplot(maize_data, aes(x = hybrid, y = EASP, fill = env_label)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(title = "FAW Resistance (EASP) Distribution Across Hybrids and Environments",
       x = "Hybrid", y = "EASP (Lower is Better)", fill = "Environment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Test for significant differences in EASP across hybrids and environments:
# using a linear model(ANOVA)
model <- lm(EASP ~ hybrid * env_label, data = maize_data)

# View ANOVA results
summary(aov(model))

# Interpretation:
#Significant hybrid effect: Indicates genetic variability among hybrids.
#Significant env_label effect: Indicates differences between infested env (natural and artificial infestations).
#Significant interaction (hybrid:env_label): Indicates that hybrid performance varies across environments

#save anova summary.
model <- lm(EASP ~ hybrid * env_label, data = maize_data)
model2 <- aov(model)
sink("anova_summary.txt")
summary(model2)
sink()

install.packages("agricolae")
library(agricolae)

# Perform AMMI analysis
ammi_model <- AMMI(
  ENV = maize_data$env,
  GEN = maize_data$hybrid,
  REP = maize_data$rep,
  Y = maize_data$EASP
)

# View AMMI results
summary(ammi_model)

# Aggregate data by hybrid and environment
maize_aggregated <- maize_data %>%
  group_by(hybrid, env) %>%
  summarise(
    mean_EASP = mean(EASP, na.rm = TRUE), # Mean FAW resistance score
    mean_GY = mean(GY, na.rm = TRUE)      # Mean grain yield
  )


ggplot(maize_aggregated, aes(x = factor(env), y = mean_EASP, fill = factor(env))) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "FAW Resistance (EASP) Across Environments",
       x = "Environment", y = "Mean EASP (Lower is Better)") +
  theme_minimal()

# line plot for hybrid across environment
ggplot(maize_aggregated, aes(x = factor(env), y = mean_EASP, group = hybrid, color = hybrid)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "FAW Resistance (EASP) Stability Across Environments",
       x = "Environment", y = "Mean EASP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Interpretation :
#Hybrids with flat lines (little variation across environments) are more stable.
#Hybrids with steep slopes or large fluctuations are less stable

# Calculate CV for each hybrid
cv_analysis <- maize_aggregated %>%
  group_by(hybrid) %>%
  summarise(
    mean_EASP = mean(mean_EASP, na.rm = TRUE),
    sd_EASP = sd(mean_EASP, na.rm = TRUE),
    CV_EASP = (sd_EASP / mean_EASP) * 100
  ) %>%
  arrange(CV_EASP)

# View the results
print(cv_analysis)





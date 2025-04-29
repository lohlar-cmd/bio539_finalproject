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


#question 2 :identify promising hybrids that combine FAW resistance with yield and other desirable agronomic characteristics.

# Aggregate data by hybrid (average across replications and environments)
maize_aggregated <- maize_data %>%
  group_by(hybrid) %>%
  summarise(
    mean_EASP = mean(EASP, na.rm = TRUE),
    mean_GY = mean(GY, na.rm = TRUE),
    mean_PHT = mean(PHT, na.rm = TRUE), # Plant height
    mean_EHT = mean(EH, na.rm = TRUE), # Ear height
    mean_DA = mean(DA, na.rm = TRUE),    # Days to anthesis
    mean_DS = mean(DS, na.rm = TRUE),    # Days to silking
    mean_ASI = mean(ASI, na.rm = TRUE), # Anthesis-silking interval
    mean_EPP = mean(EPP, na.rm = TRUE)
  )

# Identify hybrids with low EASP (bottom 25%) and high GY (top 25%)
threshold_EASP <- quantile(maize_aggregated$mean_EASP, 0.25) # Bottom 25% for EASP
threshold_GY <- quantile(maize_aggregated$mean_GY, 0.75)    # Top 25% for GY

# Filter hybrids based on criteria
promising_hybrids <- maize_aggregated %>%
  filter(
    mean_EASP <= quantile(mean_EASP, 0.25), # Bottom 25% for FAW resistance
    mean_GY >= quantile(mean_GY, 0.75),     # Top 25% for grain yield
    mean_PHT >= 180,                        # Plant height within range
    mean_PHT <= 220,
    mean_EHT <= 100                         # Low ear placement
  )

# View the filtered hybrids
print(promising_hybrids)

promising_hybrids <- maize_aggregated %>%
  filter(mean_EASP <= threshold_EASP, mean_GY >= threshold_GY)

# Add a combined score
promising_hybrids <- promising_hybrids %>%
  mutate(
    combined_score = (1 / mean_EASP) + (mean_GY / 1000) - abs(mean_PHT - 200) / 100 - (mean_EHT / 100)
  ) %>%
  arrange(desc(combined_score)) # Sort by combined score

# View the ranked hybrids
print(promising_hybrids)

ggplot(maize_aggregated, aes(x = mean_EASP, y = mean_GY, label = hybrid)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_text(data = promising_hybrids, aes(label = hybrid), vjust = -0.5, size = 3) +
  labs(title = "Hybrid Performance: FAW Resistance vs. Grain Yield",
       x = "Mean EASP (Lower is Better)",
       y = "Mean Grain Yield (Higher is Better)") +
  theme_minimal()

ggplot(promising_hybrids, aes(x = mean_EASP, y = mean_GY, label = hybrid)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_text(aes(label = hybrid), vjust = -0.5, size = 3) +
  labs(title = "Promising Hybrids: FAW Resistance vs. Grain Yield",
       x = "Mean EASP (Lower is Better)",
       y = "Mean Grain Yield (Higher is Better)") +
  theme_minimal()

promising_hybrids_top10 <- promising_hybrids %>% slice_head(n = 10)

ggplot(promising_hybrids_top10, aes(x = reorder(hybrid, combined_score), y = combined_score)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Top 10 Promising Hybrids",
       x = "Hybrid",
       y = "Combined Score (FAW Resistance + Grain Yield)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Identify Top-Performing Hybrids:

# Filter top-performing hybrids under infested conditions
top_hybrids_infested <- fa_worm_variability %>%
  filter(env_label == "Infested", mean_EASP <= quantile(mean_EASP, 0.25)) %>% # Bottom 25%
  arrange(mean_EASP)

# Filter top-performing hybrids under controlled conditions
top_hybrids_controlled <- fa_worm_variability %>%
  filter(env_label == "Controlled", mean_EASP <= quantile(mean_EASP, 0.25)) %>%
  arrange(mean_EASP)

# Combine results
top_hybrids <- bind_rows(
  top_hybrids_infested %>% mutate(condition = "Infested"),
  top_hybrids_controlled %>% mutate(condition = "Controlled")
)

# View top hybrids
print(top_hybrids)


#question 3: Assess relationship between agronomic traits and fall armyworm resistance.

ggplot(maize_aggregated, aes(x = mean_PHT, y = mean_EASP)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add regression line
  labs(title = "Relationship Between Plant Height and FAW Resistance",
       x = "Mean Plant Height (cm)",
       y = "Mean EASP (Lower is Better)") +
  theme_minimal()     

# Compute correlation matrix
correlation_matrix <- cor(maize_aggregated[, c("mean_EASP", "mean_GY", "mean_PHT", "mean_EHT", "mean_DA", "mean_DS", "mean_ASI", "mean_EPP")],
                          use = "complete.obs", method = "pearson")

# View the correlation matrix
print(correlation_matrix)

# Fit a multiple linear regression model
model <- lm(mean_EASP ~ mean_PHT + mean_EHT + mean_DA + mean_DS + mean_ASI + mean_EPP + mean_GY, data = maize_aggregated)

# Summarize the model
summary(model)
print(summary(model))

# Extract correlation coefficients for EASP
easp_correlations <- correlation_matrix["mean_EASP", ]
easp_correlations <- data.frame(Trait = names(easp_correlations), Correlation = easp_correlations)

# plot of correlations
ggplot(easp_correlations, aes(x = reorder(Trait, Correlation), y = Correlation)) +
  geom_segment(aes(xend = Trait, yend = 0), color = "gray") + # Add vertical lines
  geom_point(size = 4, color = "red") + # Add points at the end of the lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Reference line at zero
  labs(title = "Correlation of Agronomic Traits with FAW Resistance (EASP)",
       x = "Trait",
       y = "Correlation Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




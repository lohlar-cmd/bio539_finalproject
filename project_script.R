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
    mean_EHT = mean(EHT, na.rm = TRUE), # Ear height
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

# Install required packages if not already installed
install.packages("DiallelAnalysisR") # For diallel analysis
install.packages("dplyr")           # For data manipulation

# Load libraries
library(DiallelAnalysisR)
library(dplyr)
library(tidyverse)
library(lme4)       # For mixed-effects models
# Load the dataset
maize_data <- read.csv("MAIZE DATA.csv")

# Inspect the structure of the dataset
str(maize_data)


library(lme4)       # For mixed-effects models

# Load the dataset
maize_data <- read.csv("MAIZE DATA.csv")

# Inspect the structure of the dataset
str(maize_data)


# qustion 4: determine the genetic performace of parents for grain yield and FAW resistance, 
# Check for missing values in GY, male, and female
missing_values <- maize_clean %>%
  summarise(
    missing_GY = sum(is.na(GY)),
    missing_male = sum(is.na(male)),
    missing_female = sum(is.na(female))
  )

print(missing_values)

# Remove rows with missing values
maize_clean <- maize_clean %>%
  filter(!is.na(GY), !is.na(male), !is.na(female))

# Inspect the first few rows of the dataset
head(maize_clean)

# Check the dimensions of the dataset
dim(maize_clean)

# Check the structure of the dataset
str(maize_clean)

# Convert male and female to factors if they are not already
maize_clean$male <- as.factor(maize_clean$male)
maize_clean$female <- as.factor(maize_clean$female)

# Ensure GY is numeric
maize_clean$GY <- as.numeric(maize_clean$GY)

# Fit mixed-effects model for grain yield
gy_model <- lmer(
  GY ~ (1 | male) + (1 | female), # Random effects for male and female parents
  data = maize_clean
)

# View summary of the model
summary(gy_model)

# Fit mixed-effects model for FAW resistance
easp_model <- lmer(
  EASP ~ (1 | male) + (1 | female), # Random effects for male and female parents
  data = maize_clean
)

# View summary of the model
summary(easp_model)

# Summary statistics for GY and EASP
summary(maize_clean$GY)
summary(maize_clean$EASP)

# Variance of GY and EASP
var(maize_clean$GY, na.rm = TRUE)
var(maize_clean$EASP, na.rm = TRUE)

# Simplified model with only male as a random effect
gy_model_simplified <- lmer(
  GY ~ (1 | male),
  data = maize_clean
)

# View summary of the simplified model
summary(gy_model_simplified)

# Fit mixed-effects model for grain yield
gy_model <- lmer(
  GY ~ (1 | male) + (1 | female), # Random effects for male and female parents
  data = maize_clean
)

# View summary of the model
summary(gy_model)
# Fit mixed-effects model for FAW resistance
easp_model <- lmer(
  EASP ~ (1 | male) + (1 | female), # Random effects for male and female parents
  data = maize_clean
)

# View summary of the model
summary(easp_model)

# Extract random effects for grain yield
gy_random_effects <- ranef(gy_model)

# Convert to a data frame for easier interpretation
gy_gca_effects <- gy_random_effects$male %>%
  as.data.frame() %>%
  rownames_to_column(var = "Parent") %>%
  rename(GCA_Effect_GY = `(Intercept)`)

# View the GCA effects
print(gy_gca_effects)


# Extract random effects for FAW resistance
easp_random_effects <- ranef(easp_model)

# Convert to a data frame for easier interpretation
easp_gca_effects <- easp_random_effects$male %>%
  as.data.frame() %>%
  rownames_to_column(var = "Parent") %>%
  rename(GCA_Effect_EASP = `(Intercept)`)

# View the GCA effects
print(easp_gca_effects)

library(ggplot2)

# Bar plot for GCA effects (grain yield)
ggplot(gy_gca_effects, aes(x = reorder(Parent, GCA_Effect_GY), y = GCA_Effect_GY)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "GCA Effects of Parents for Grain Yield",
       x = "Parent", y = "GCA Effect") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar plot for GCA effects (fall armyworm resistance)
ggplot(easp_gca_effects, aes(x = reorder(Parent, GCA_Effect_EASP), y = GCA_Effect_EASP)) +
  geom_bar(stat = "identity", fill = "red") + # Use red to indicate resistance
  labs(title = "GCA Effects of Parents for Fall Armyworm Resistance",
       x = "Parent", y = "GCA Effect (Lower is Better)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dot plot for GCA effects
ggplot(easp_gca_effects, aes(x = reorder(Parent, GCA_Effect_EASP), y = GCA_Effect_EASP)) +
  geom_point(size = 4, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Add reference line at zero
  labs(title = "GCA Effects of Parents for Fall Armyworm Resistance",
       x = "Parent", y = "GCA Effect (Lower is Better)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("gca_easp_effects.png", width = 8, height = 6, dpi = 300)


# Combine GCA effects for grain yield and FAW resistance
combined_gca_effects <- gy_gca_effects %>%
  left_join(easp_gca_effects, by = "Parent") %>%
  rename(GCA_Effect_GY = GCA_Effect_GY, GCA_Effect_EASP = GCA_Effect_EASP)

# View combined GCA effects
print(combined_gca_effects)

# Residual plot for grain yield model
plot(gy_model_infested)

# Residual plot for FAW resistance model
plot(easp_model_infested)

# Fit mixed-effects model for grain yield
gy_model <- lmer(
  GY ~ (1 | male) + (1 | female), # Random effects for male and female parents
  data = maize_clean
)

# View summary of the model
summary(gy_model)

# Fit mixed-effects model for FAW resistance
easp_model <- lmer(
  EASP ~ (1 | male) + (1 | female), # Random effects for male and female parents
  data = maize_clean
  
  
  
# under infested condition.
  ##############
  gy_model_infested <- lmer(
    GY ~ (1 | male) + (1 | female), # Random effects for male and female parents
    data = maize_clean
  )
  
  summary(gy_model_infested)
  
  easp_model_infested <- lmer(
    EASP ~ (1 | male) + (1 | female), # Random effects for male and female parents
    data = maize_clean
  )
  
  summary(easp_model_infested)
  
  # Extract random effects for FAW resistance
  easp_random_effects <- ranef(easp_model_infested)
  
  # Convert to a data frame for easier interpretation
  easp_gca_effects <- easp_random_effects$male %>%
    as.data.frame() %>%
    rownames_to_column(var = "Parent") %>%
    rename(GCA_Effect_EASP = `(Intercept)`)
  
  # View GCA effects for FAW resistance
  print(easp_gca_effects)
  
  
  # Extract random effects for grain yield
  gy_random_effects <- ranef(gy_model_infested)
  
  # Convert to a data frame for easier interpretation
  gy_gca_effects <- gy_random_effects$male %>%
    as.data.frame() %>%
    rownames_to_column(var = "Parent") %>%
    rename(GCA_Effect_GY = `(Intercept)`)
  
  # View GCA effects for grain yield
  print(gy_gca_effects)
  
  # Extract fixed effects for FAW resistance
  fixed_effects_easp <- fixef(easp_model_infested)
  print(fixed_effects_easp)  
  
  # Extract fixed effects for grain yield
  fixed_effects_gy <- fixef(gy_model_infested)
  print(fixed_effects_gy)
  
  # Dot plot for GCA effects
  ggplot(gy_gca_effects, aes(x = reorder(Parent, GCA_Effect_GY), y = GCA_Effect_GY)) +
    geom_point(size = 4, color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Add reference line at zero
    labs(title = "GCA Effects of Parents for Grain Yield (Infested Environment)",
         x = "Parent", y = "GCA Effect") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Residual plot for grain yield model
  plot(gy_model_infested)
  
  # Compare AIC and BIC values
  AIC(gy_model_infested)
  BIC(gy_model_infested)
  
  # Save GCA effects to CSV file
  write.csv(gy_gca_effects, "gca_effects_grain_yield.csv", row.names = FALSE)
  
  # Combine GCA effects for grain yield and FAW resistance
  combined_gca_effects <- gy_gca_effects %>%
    left_join(easp_gca_effects, by = "Parent") %>%
    rename(GCA_Effect_GY = GCA_Effect_GY, GCA_Effect_EASP = GCA_Effect_EASP)
  
  # View combined GCA effects
  print(combined_gca_effects)
  
  # Save GCA effects to CSV files
  write.csv(gy_gca_effects, "gca_effects_grain_yield.csv", row.names = FALSE)
  write.csv(easp_gca_effects, "gca_effects_faw_resistance.csv", row.names = FALSE)
  
  # Save bar plots as image files
  ggsave("gca_effects_grain_yield.png", width = 8, height = 6, dpi = 300)
  ggsave("gca_effects_faw_resistance.png", width = 8, height = 6, dpi = 300)
  
  # Bar plot for GCA effects (fall armyworm resistance)
  ggplot(easp_gca_effects, aes(x = reorder(Parent, GCA_Effect_EASP), y = GCA_Effect_EASP)) +
    geom_bar(stat = "identity", fill = "red") +
    labs(title = "GCA Effects of Parents for FAW Resistance (Infested Environment)",
         x = "Parent", y = "GCA Effect (Lower is Better)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  

  
  # Add random noise to create a pseudo-distribution for box plot
  set.seed(123) # For reproducibility
  easp_gca_effects <- easp_gca_effects %>%
    mutate(
      GCA_Effect_EASP_Low = GCA_Effect_EASP - runif(n(), min = 0.05, max = 0.1), # Lower bound
      GCA_Effect_EASP_High = GCA_Effect_EASP + runif(n(), min = 0.05, max = 0.1)  # Upper bound
    )
  
  # Reshape the data into long format for ggplot
  library(tidyr)
  easp_gca_long <- easp_gca_effects %>%
    pivot_longer(cols = c(GCA_Effect_EASP_Low, GCA_Effect_EASP_High),
                 names_to = "Type", values_to = "Value")
  
  # Create a box plot
  ggplot(easp_gca_long, aes(x = reorder(Parent, Value), y = Value, fill = Parent)) +
    geom_boxplot(outlier.color = NA) + # No outliers since we created pseudo-data
    labs(title = "GCA Effects of Parents for FAW Resistance (Infested Environment)",
         x = "Parent", y = "GCA Effect (Lower is Better)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") # Remove legend for cleaner visualization 
  
  # Residual plot for grain yield model
  plot(gy_model_infested)
  
  # Residual plot for FAW resistance model
  plot(easp_model_infested)
  
  # Compare AIC and BIC values
  AIC(gy_model_infested)
  BIC(gy_model_infested)
  
  # Save bar plots as image files
  ggsave("gca_effects_grain_yield.png", width = 8, height = 6, dpi = 300)
  ggsave("gca_effects_faw_resistance.png", width = 8, height = 6, dpi = 300)
  
  AIC(easp_model_infested)
  BIC(easp_model_infested)

  # Save GCA effects to CSV files
  write.csv(gy_gca_effects, "gca_effects_grain_yield.csv", row.names = FALSE)
  write.csv(easp_gca_effects, "gca_effects_faw_resistance.csv", row.names = FALSE)  
  
  
  # Filter data for control environment
  
    filter(env_label == "Controlled") # Replace "Controlled" with the actual label in your dataset
    
    
    # Fit mixed-effects model for FAW resistance (control environment)
    easp_model_control <- lmer(
      EASP ~ (1 | male) + (1 | female), # Random effects for male and female parents
      data = maize_clean
    )
    
    # View summary of the model
    summary(easp_model_control)
    
    # Extract random effects for FAW resistance (control environment)
    easp_random_effects_control <- ranef(easp_model_control)
    
    # Convert to a data frame for easier interpretation
    easp_gca_effects_control <- easp_random_effects_control$male %>%
      as.data.frame() %>%
      rownames_to_column(var = "Parent") %>%
      rename(GCA_Effect_EASP_Control = `(Intercept)`)
    
    # View GCA effects for FAW resistance (control environment)
    print(easp_gca_effects_control)
    
    # Extract fixed effects for FAW resistance (control environment)
    fixed_effects_easp_control <- fixef(easp_model_control)
    print(fixed_effects_easp_control)
    
    # Dot plot for GCA effects (fall armyworm resistance, control environment)
    ggplot(easp_gca_effects_control, aes(x = reorder(Parent, GCA_Effect_EASP_Control), y = GCA_Effect_EASP_Control)) +
      geom_point(size = 4, color = "red") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Add reference line at zero
      labs(title = "GCA Effects of Parents for FAW Resistance (Control Environment)",
           x = "Parent", y = "GCA Effect (Lower is Better)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Combine GCA effects for control and infested environments
    combined_gca_effects <- easp_gca_effects_control %>%
      left_join(easp_gca_effects, by = "Parent") %>%
      rename(
        GCA_Effect_EASP_Control = GCA_Effect_EASP_Control,
        GCA_Effect_EASP_Infested = GCA_Effect_EASP
      )
    
    # View combined GCA effects
    print(combined_gca_effects)
    
    # Line plot to compare GCA effects across environments
    ggplot(combined_gca_effects, aes(x = Parent)) +
      geom_line(aes(y = GCA_Effect_EASP_Control, group = 1, color = "Control"), linetype = "solid") +
      geom_line(aes(y = GCA_Effect_EASP_Infested, group = 1, color = "Infested"), linetype = "dashed") +
      labs(
        title = "Comparison of GCA Effects for FAW Resistance Across Environments",
        x = "Parent",
        y = "GCA Effect",
        color = "Environment"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Residual plot for FAW resistance model (control environment)
    plot(easp_model_control)
    # Compare AIC and BIC values
    AIC(easp_model_control)
    BIC(easp_model_control)    
    # Save GCA effects to CSV file
    write.csv(easp_gca_effects_control, "gca_effects_faw_resistance_control.csv", row.names = FALSE)    
        




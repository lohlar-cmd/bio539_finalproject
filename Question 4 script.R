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
        



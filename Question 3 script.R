

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
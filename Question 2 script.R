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

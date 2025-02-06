# Data Manipulation and Multivariate Statistics with Penguins Dataset
# Author: Mauricio Mardones
# Course: Introduction to Data Analysis with R (University Level)

# --------------------------
# 1. Load Required Libraries
# --------------------------
# Install if not available
if (!requireNamespace("palmerpenguins", quietly = TRUE)) {
  install.packages("palmerpenguins")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library(palmerpenguins)  # Dataset
library(ggplot2)         # Visualization
library(dplyr)           # Data manipulation

# --------------------------
# 2. Load and Explore Data
# --------------------------
# Load the penguins dataset
penguins <- palmerpenguins::penguins

# View the first few rows of data
head(penguins)

# Check structure and summary
str(penguins)
summary(penguins)

# --------------------------
# 3. Handle Missing Data
# --------------------------
# Identify missing values
sum(is.na(penguins))

# Remove rows with missing data
penguins_clean <- na.omit(penguins)

# --------------------------
# 4. Data Manipulation
# --------------------------
# Select columns of interest
penguins_subset <- penguins_clean %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

# Create a new variable (bill ratio)
penguins_subset <- penguins_subset %>%
  mutate(bill_ratio = bill_length_mm / bill_depth_mm)

# Filter data for a single species (Adelie)
penguins_adelie <- penguins_subset %>%
  filter(species == "Adelie")

# --------------------------
# 5. Descriptive Statistics
# --------------------------
# Mean and standard deviation for bill length
mean(penguins_clean$bill_length_mm)
sd(penguins_clean$bill_length_mm)

# Summary statistics by species
penguins_summary <- penguins_clean %>%
  group_by(species) %>%
  summarize(
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE),
    sd_flipper_length = sd(flipper_length_mm, na.rm = TRUE)
  )
print(penguins_summary)

# --------------------------
# 6. Basic Visualization
# --------------------------
# Scatter plot of bill length vs bill depth
scatter_plot <- ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Bill Length vs Bill Depth by Species",
    x = "Bill Length (mm)",
    y = "Bill Depth (mm)"
  )
print(scatter_plot)

# --------------------------
# 7. Multivariate Statistics (PCA)
# --------------------------
# Perform Principal Component Analysis (PCA)
# Select only numeric columns for PCA
penguins_numeric <- penguins_clean %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

# Ensure there are no missing values
penguins_numeric <- na.omit(penguins_numeric)

# PCA analysis
penguins_pca <- prcomp(penguins_numeric, scale. = TRUE)

# Print PCA results
summary(penguins_pca)

# Plot PCA
pca_plot <- ggplot(as.data.frame(penguins_pca$x), aes(x = PC1, y = PC2)) +
  geom_point(aes(color = penguins_clean$species)) +
  labs(
    title = "PCA of Penguin Morphological Traits",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal()
print(pca_plot)

# --------------------------
# End of Script
# --------------------------

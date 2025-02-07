# Script: Descriptive Statistics with Penguins Dataset

# =============================
# Load Data
# =============================

# Load necessary package
install.packages("palmerpenguins")
library(palmerpenguins)

# Load the dataset
penguins <- palmerpenguins::penguins

# Remove rows with missing values
penguins_clean <- na.omit(penguins)

# =============================
# Summary of the Dataset
# =============================

# General overview of the dataset
summary(penguins_clean)

# View first few rows of the dataset
head(penguins_clean)

# =============================
# Measures of Central Tendency
# =============================

# Mean of flipper length
mean_flipper <- mean(penguins_clean$flipper_length_mm)
mean_flipper

# Median of flipper length
median_flipper <- median(penguins_clean$flipper_length_mm)
median_flipper

# =============================
# Measures of Spread
# =============================

# Range of body mass
range_body_mass <- range(penguins_clean$body_mass_g)
range_body_mass

# Variance and standard deviation of body mass
var_body_mass <- var(penguins_clean$body_mass_g)
sd_body_mass <- sd(penguins_clean$body_mass_g)
var_body_mass
sd_body_mass

# =============================
# Frequency Tables for Categorical Data
# =============================

# Frequency of species
species_freq <- table(penguins_clean$species)
species_freq

# =============================
# Relationships Between Variables
# =============================

# Covariance between bill length and bill depth
cov_bill <- cov(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)
cov_bill

# Correlation between bill length and bill depth
cor_bill <- cor(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)
cor_bill


# =============================
# Conclusion
# =============================

# This script covers basic descriptive statistics, measures of central tendency, and relationships between variables.
# Visualizations were created using base R functions without external libraries.

# Script: Descriptive Statistics with Penguins Dataset (Tidyverse)

# =============================
# Load Packages
# =============================

# Install and load necessary packages
install.packages("tidyverse")
library(tidyverse)

# Load the dataset
penguins <- palmerpenguins::penguins

# Remove rows with missing values
penguins_clean <- drop_na(penguins)

# =============================
# Summary of the Dataset
# =============================

# General overview of the dataset
penguins_clean %>% summary()

# Glimpse of the dataset structure
penguins_clean %>% glimpse()

# =============================
# Measures of Central Tendency
# =============================

# Mean of flipper length
penguins_clean %>% summarise(mean_flipper = mean(flipper_length_mm))

# Median of flipper length
penguins_clean %>% summarise(median_flipper = median(flipper_length_mm))

# =============================
# Measures of Spread
# =============================

# Range of body mass
penguins_clean %>% summarise(range_body_mass = range(body_mass_g))

# Variance and standard deviation of body mass
penguins_clean %>% summarise(
  var_body_mass = var(body_mass_g),
  sd_body_mass = sd(body_mass_g)
)

# =============================
# Frequency Tables for Categorical Data
# =============================

# Frequency of species
penguins_clean %>% count(species)

# =============================
# Relationships Between Variables
# =============================

# Covariance between bill length and bill depth
penguins_clean %>% summarise(cov_bill = cov(bill_length_mm, bill_depth_mm))

# Correlation between bill length and bill depth
penguins_clean %>% summarise(cor_bill = cor(bill_length_mm, bill_depth_mm))



# =============================
# Conclusion
# =============================

# This script covers descriptive statistics and basic visualizations using tidyverse functions, with clear steps for beginners.
# Students are introduced to summarizing data, calculating basic statistics, and creating plots using efficient and readable code.


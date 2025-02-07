# Script: Exploratory Data Analysis (EDA) with Penguins Dataset (Base R)

# =============================
# Load Necessary Packages
# =============================

# Install the palmerpenguins package if necessary
if (!require("palmerpenguins")) install.packages("palmerpenguins")

# Load the dataset
library(palmerpenguins)

data("penguins")

# Remove rows with missing values
penguins_clean <- na.omit(penguins)

# =============================
# Basic Data Overview
# =============================

# Structure of the dataset
str(penguins_clean)

# Summary of the dataset
summary(penguins_clean)

# Dimensions of the dataset
cat("Number of rows:", nrow(penguins_clean), "\n")
cat("Number of columns:", ncol(penguins_clean), "\n")

# =============================
# Checking Relationships Between Variables
# =============================

# Covariance between bill length and bill depth
cov_bill <- cov(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)
cat("Covariance (Bill Length vs Bill Depth):", cov_bill, "\n")

# Correlation between bill length and bill depth
cor_bill <- cor(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)
cat("Correlation (Bill Length vs Bill Depth):", cor_bill, "\n")

# =============================
# Detection of Outliers
# =============================

# Boxplot to visually detect outliers in body mass
boxplot(penguins_clean$body_mass_g, main = "Boxplot of Body Mass", ylab = "Body Mass (g)")

# Identify specific outliers
outliers <- boxplot.stats(penguins_clean$body_mass_g)$out
cat("Outliers in body mass:", outliers, "\n")

# Script: Exploratory Data Analysis (EDA) with Penguins Dataset

# =============================
# Load Packages
# =============================

# Install and load necessary packages
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# Load the dataset
penguins <- palmerpenguins::penguins

# Remove rows with missing values
penguins_clean <- drop_na(penguins)

# =============================
# Initial Data Exploration
# =============================

# View first few rows of the dataset
head(penguins_clean)

# Check the structure of the dataset
glimpse(penguins_clean)

# Dimensions of the dataset
cat("Number of rows:", nrow(penguins_clean), "\n")
cat("Number of columns:", ncol(penguins_clean), "\n")

# =============================
# Distribution of Variables
# =============================

# Distribution of numerical variables
summary(select(penguins_clean, where(is.numeric)))

# Count observations for each species
penguins_clean %>% count(species)

# =============================
# Identify Missing Values
# =============================

# Count missing values per column in the original dataset
colSums(is.na(penguins))

# =============================
# Exploring Relationships Between Variables
# =============================

# Covariance and correlation between bill length and bill depth
cov(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)
cor(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)

# Grouped summary statistics for flipper length by species
penguins_clean %>%
  group_by(species) %>%
  summarise(
    mean_flipper = mean(flipper_length_mm),
    sd_flipper = sd(flipper_length_mm),
    min_flipper = min(flipper_length_mm),
    max_flipper = max(flipper_length_mm)
  )

# =============================
# Outlier Detection
# =============================

# Simple check for outliers using IQR method for body mass
q1 <- quantile(penguins_clean$body_mass_g, 0.25)
q3 <- quantile(penguins_clean$body_mass_g, 0.75)
iqr_value <- q3 - q1
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value

penguins_clean %>%
  filter(body_mass_g < lower_bound | body_mass_g > upper_bound) %>%
  select(species, body_mass_g)


# =============================
# Conclusion
# =============================

# This script introduces students to the basic steps in exploratory data analysis (EDA), 
# focusing on variable distribution, relationships, outlier detection, and grouped statistics. 
# Visualizations provide additional insights, fostering a deeper understanding of the dataset's structure.

# =============================
# Reading Data from an Online CSV
# =============================

# Example URL (replace with a real data source when available)
csv_url <- "https://people.sc.fsu.edu/~jburkardt/data/csv/airtravel.csv"


# Reading the CSV file directly from the web
csv_data <- read.csv(csv_url)

# Display the first few rows of the loaded data
head(csv_data)

# =============================
# Conclusion
# =============================

# This script provides a simple guide to conducting EDA using base R functions, suitable for introductory courses.
# Topics include data structure exploration, basic statistics, detection of outliers, and visualization techniques.

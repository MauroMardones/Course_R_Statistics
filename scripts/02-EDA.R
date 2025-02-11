# Exploratory Data Analysis (EDA) with Penguins Dataset (Base R)

# =============================
# Load Necessary Packages
# =============================

# Verificar e instalar el paquete "palmerpenguins" si es necesario
if (!require("palmerpenguins")) install.packages("palmerpenguins")
library(palmerpenguins)

# =============================
# Load and Clean Data
# =============================

# Cargar el dataset de penguins
penguins <- palmerpenguins::penguins

# Mostrar las primeras filas del conjunto de datos
cat("Primeras filas del dataset:\n")
print(head(penguins))

# Dimensiones del dataset
dim(penguins)

nrow(penguins)
ncol(penguins)

# Verificar valores faltantes
print(colSums(is.na(penguins)))

# Remover filas con valores faltantes
penguins_clean <- na.omit(penguins)

# =============================
# Basic Data Overview
# =============================

# Estructura del dataset limpio
cat("\nEstructura del dataset:\n")
str(penguins_clean)

# Resumen estadístico de las variables numéricas
cat("\nResumen estadístico de variables numéricas:\n")
print(summary(select(penguins_clean, where(is.numeric))))

# Nombres de las columnas
cat("\nNombres de las columnas:\n")
print(names(penguins_clean))

# Conteo de observaciones por especie

table(penguins_clean$species)

# =============================
# Relationships Between Variables
# =============================

# Covarianza y correlación entre el largo y ancho del pico
cov_bill <- cov(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)
cor_bill <- cor(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)


# =============================
# Outlier Detection
# =============================

# Identificar outliers en body_mass_g usando el método del rango intercuartílico (IQR)
q1 <- quantile(penguins_clean$body_mass_g, 0.25)
q3 <- quantile(penguins_clean$body_mass_g, 0.75)
iqr_value <- q3 - q1
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value


# Filtrar y mostrar outliers
outliers <- penguins_clean %>%
  filter(body_mass_g < lower_bound | body_mass_g > upper_bound) %>%
  select(species, body_mass_g)

print(outliers)

# =============================
# Conclusion
# =============================


# Exploratory Data Analysis (EDA) with Penguins Dataset

# =============================
# Load Necessary Packages
# =============================

# Check and install necessary packages if not already installed
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# =============================
# Load and Clean Data
# =============================

# Load the dataset
penguins <- palmerpenguins::penguins

# Remove rows with missing values
penguins_clean <- drop_na(penguins)

# Check dataset dimensions
cat("\nDimensions of the cleaned dataset:", nrow(penguins_clean), "rows and", ncol(penguins_clean), "columns\n")

# =============================
# Basic Data Overview
# =============================

# View the first few rows
cat("\nFirst few rows of the dataset:\n")
print(head(penguins_clean))

# Dataset structure
cat("\nDataset structure:\n")
str(penguins_clean)

# Summary statistics
cat("\nSummary statistics:\n")
summary(select(penguins_clean, where(is.numeric)))

# Column names
cat("\nColumn names:\n")
print(names(penguins_clean))

# =============================
# Measures of Central Tendency
# =============================

# Mean and median of flipper length
mean_flipper <- mean(penguins_clean$flipper_length_mm)
median_flipper <- median(penguins_clean$flipper_length_mm)

cat("\nMean of flipper length:", mean_flipper)
cat("\nMedian of flipper length:", median_flipper)

penguins_clean %>% 
  summarise(
    mean_flipper = mean(flipper_length_mm),
    median_flipper = median(flipper_length_mm)
  ) %>% print()

# =============================
# Measures of Spread
# =============================

# Range, variance, and standard deviation of body mass
range_body_mass <- range(penguins_clean$body_mass_g)
var_body_mass <- var(penguins_clean$body_mass_g)
sd_body_mass <- sd(penguins_clean$body_mass_g)

cat("\nRange of body mass:", range_body_mass)
cat("\nVariance of body mass:", var_body_mass)
cat("\nStandard deviation of body mass:", sd_body_mass)

penguins_clean %>%
  group_by(species) %>% 
  summarise(
    range_body_mass = paste(range(body_mass_g), collapse = " to "),
    var_body_mass = var(body_mass_g),
    sd_body_mass = sd(body_mass_g)
  ) %>% print()

# =============================
# Frequency Tables for Categorical Data
# =============================

# Frequency of species
cat("\nFrequency of species:\n")
print(table(penguins_clean$species))

penguins_clean %>% 
  count(species) %>% 
  print()

# =============================
# Relationships Between Variables
# =============================

# Covariance and correlation between bill length and bill depth
cov_bill <- cov(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)
cor_bill <- cor(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)

cat("\nCovariance between bill length and bill depth:", cov_bill)
cat("\nCorrelation between bill length and bill depth:", cor_bill)

penguins_clean %>% 
  summarise(
    cov_bill = cov(bill_length_mm, bill_depth_mm),
    cor_bill = cor(bill_length_mm, bill_depth_mm)
  ) %>% print()

# =============================
# Outlier Detection
# =============================

# Identify outliers in body mass using the interquartile range (IQR) method
q1 <- quantile(penguins_clean$body_mass_g, 0.25)
q3 <- quantile(penguins_clean$body_mass_g, 0.75)
iqr_value <- q3 - q1
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value

outliers <- penguins_clean %>% 
  filter(body_mass_g < lower_bound | body_mass_g > upper_bound) %>% 
  select(species, body_mass_g)

cat("\nOutliers in body mass:\n")
print(outliers)


# =============================
# Detect extrem values
# =============================

# Z-score computation for body mass
z_scores <- scale(penguins_clean$body_mass_g)
penguins_clean$outliers <- abs(z_scores) > 2
penguins_clean %>% filter(outliers)




# =============================
# Comparision between groups simple
# =============================

# Summary statistics for two specific species
penguins_clean %>% 
  filter(species %in% c("Adelie", "Gentoo")) %>%
  group_by(species) %>%
  summarise(
    mean_body_mass = mean(body_mass_g),
    sd_body_mass = sd(body_mass_g)
  )

# =============================
# Statistical Test t~Test
# =============================

# Filter data for Adelie and Gentoo penguins
penguins_subset <- penguins_clean %>%
  filter(species %in% c("Adelie", "Gentoo"))

# Perform a t-test to compare body mass between Adelie and Gentoo penguins
t_test_result <- t.test(body_mass_g ~ species, 
                        data = penguins_subset)

# Display test results
t_test_result

# Interpretation
# The null hypothesis (H0) assumes that the mean body mass of Adelie and Gentoo penguins is the same.
# The alternative hypothesis (H1) assumes there is a significant difference between the two means.
# Key points from t_test_result:
#   
# p-value: If p < 0.05, we reject H0, indicating a significant difference in the body mass of these species.
# Confidence interval: Provides the range within which the true difference in means lies.
# t-statistic: Indicates the magnitude of the difference relative to variability.

# # =============================
# Comparision between groups ANOVA
# =============================

# One-way ANOVA
anova_body_mass <- aov(bill_length_mm ~ species, data = penguins_clean)
summary(anova_body_mass)

# =============================
# Model Linear Regresion simple
# =============================

# Simple linear regression model
model <- lm(body_mass_g ~ flipper_length_mm, data = penguins_clean)
summary(model)

# =============================
# Conclusion
# =============================

#This script demonstrates exploratory data analysis (EDA) using both Base R and Tidyverse, including cleaning, descriptive statistics, and relationships between variables.")


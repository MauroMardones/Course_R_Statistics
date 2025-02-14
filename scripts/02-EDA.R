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
head(penguins, 10)

# Dimensiones del dataset
dim(penguins)

nrow(penguins)
ncol(penguins)

# Verificar valores faltantes
colSums(is.na(penguins))

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

# Summary statistics
summary(select(penguins_clean, where(is.numeric)))

# Column names
print(names(penguins_clean))

# Conteo de observaciones por especie

table(penguins_clean$species)

# =============================
# Central trends and dispersion measure
# =============================

mean(penguins_clean$flipper_length_mm)
median(penguins_clean$flipper_length_mm)
sd(penguins_clean$flipper_length_mm)
range(penguins_clean$flipper_length_mm)
IQR(penguins_clean$flipper_length_mm)

# ====================================================
# Cumulative Statistics
# ====================================================
# Cumulative sum and product of bill depth
cumsum_bill_depth <- cumsum(penguins_clean$bill_depth_mm)
cumprod_bill_depth <- cumprod(penguins_clean$bill_depth_mm)

print(head(cumsum_bill_depth, 5))
print(head(cumprod_bill_depth, 5))
# =============================
# Relationships Between Variables
# =============================

# Covarianza y correlación entre el largo y ancho del pico
cov_bill <- cov(penguins_clean$bill_length_mm,
                penguins_clean$bill_depth_mm,
                use = "everything",
                method = c("pearson", "kendall", "spearman"))
cor_bill <- cor(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm)

# =============================
# Outliers Handling
# =============================

# Detectar outliers con IQR

#El IQR es la diferencia entre el tercer cuartil (Q3, el percentil 75) y
# el primer cuartil (Q1, el percentil 25). Es una medida de dispersión 
# que indica el rango central donde se ubica el 50% de los datos.
q1 <- quantile(penguins_clean$body_mass_g, 0.25)
q3 <- quantile(penguins_clean$body_mass_g, 0.75)
iqr_value <- q3 - q1
lower_bound <- q1 - 0.5 * iqr_value
upper_bound <- q3 + 0.5 * iqr_value

outliers <- penguins_clean %>% 
  filter(body_mass_g < lower_bound | body_mass_g > upper_bound) %>% 
  select(species, body_mass_g)

cat("\nOutliers en body mass:\n")
print(outliers)



# =============================
# Detect extrem values
# =============================

# Z-score computation for body mass
z_scores <- scale(penguins_clean$body_mass_g)
# El comando scale() en R normaliza la variable body_mass_g restando 
# la media y dividiendo por la desviación estándar, 
# generando los valores de Z-score para cada observación.

penguins_clean$outliers <- abs(z_scores) > 2

# Aquí, creas una nueva columna en el dataframe penguins_clean 
# llamada outliers. Para cada valor de body_mass_g, marcas si su Z-score es mayor que 
# 2 o menor que -2. Los valores cuyo Z-score absoluto es mayor que 2 se consideran outliers 
# (por lo general, se considera que un Z-score fuera del rango [−2,2] es indicativo de un valor atípico).


penguins_clean %>% filter(outliers)

# Con este paso, utilizas dplyr para filtrar las filas del dataframe donde 
# la columna outliers es TRUE. Es decir, estás seleccionando las observaciones 
# donde el Z-score de body_mass_g es mayor a 2 o menor a -2, lo que indica que son outliers.
# 
# Outliers: Los outliers identificados son principalmente individuos de la especie Gentoo, 
# con valores de body_mass_g relativamente altos (por encima de los 5850 g),
# con valores de masa corporal entre 5850 g y 6300 g en los años 2007 a 2009.

# =============================
# EDA Tidyverse
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
cat("\nDimensions of the cleaned dataset:", 
    nrow(penguins_clean), "rows and", 
    ncol(penguins_clean), "columns\n")

# =============================
# Basic Data Overview
# =============================

# Dataset structure
glimpse(penguins_clean)

# Descripción rápida con `skimr`
skim(penguins_clean)


# =============================
# Manipulación Exhaustiva de Datos
# =============================

# Seleccionar columnas específicas
selected_data <- select(penguins_clean, species, island, body_mass_g)
head(selected_data)

# Filtrar filas con condiciones múltiples
filtered_data <- filter(penguins_clean,
                        species == "Chinstrap", 
                        island != "Biscoe")
head(filtered_data)

# Reordenar filas
arranged_data <- arrange(penguins_clean, desc(bill_length_mm))
head(arranged_data)

# Crear nuevas variables
mutated_data <- mutate(penguins_clean, 
                       body_mass_kg = body_mass_g / 1000,
                       bmi = body_mass_g / (flipper_length_mm / 100)^2)
head(select(mutated_data, body_mass_g, body_mass_kg, bmi))

penguins_body_ratio <- penguins_clean %>%
  mutate(bill_mass_ratio = bill_length_mm / body_mass_g)

cat("\nFirst few rows of new variable 'bill_mass_ratio':\n")
print(head(penguins_body_ratio$bill_mass_ratio))


# Agrupar y resumir
grouped_summary <- penguins_clean %>%
  group_by(species, sex, island) %>%
  summarise(
    mean_mass = mean(body_mass_g),
    sd_mass = sd(body_mass_g),
    min_mass = min(body_mass_g),
    max_mass = max(body_mass_g),
    count = n()
  )
print(grouped_summary)

# Resumir múltiples variables a la vez
penguins_clean %>%
  summarise(across(where(is.numeric), 
                   list(mean = mean, sd = sd)))

# =============================
# Manipular data frames
# =============================

# Pivotar datos (cambiar entre formato ancho y largo)
long_format <- penguins_clean %>%
  pivot_longer(cols = c(bill_length_mm, 
                        bill_depth_mm, 
                        flipper_length_mm, 
                        body_mass_g),
               names_to = "measurement", 
               values_to = "value")

head(long_format)

#(Revisar!!)
wide_format <- long_format %>%
  dplyr::group_by(species, island, sex, year, measurement) %>%
  dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = measurement, values_from = value)

head(wide_format)

# =============================
# Juntar DF
# =============================

# Crear dataset extra para ejemplos de join

extra_data <- data.frame(
  species = c("Adelie", "Chinstrap", "Gentoo"),
  conservation_status = c("High Concern", "Near Threatened", "Least Concern")
)

# Left join
joined_data <- left_join(penguins_clean, 
                         extra_data, 
                         by = "species")
head(joined_data)

# Inner join
inner_joined <- inner_join(penguins_clean, 
                           extra_data, 
                           by = "species")
head(inner_joined)


# =============================
# Relaciones entre Variables
# =============================

# Matriz de correlación
cor_matrix <- select(penguins_clean, where(is.numeric)) %>% 
  cor() 
print(cor_matrix)

# Pair plot con GGally
ggpairs(select(penguins_clean, where(is.numeric)))

# =============================
# Pruebas Estadísticas
# =============================

# T-Test: Adelie vs Gentoo
adelie_gentoo <- filter(penguins_clean, 
                        species %in% c("Adelie", "Gentoo"))
t_test_result <- t.test(body_mass_g ~ species, 
                        data = adelie_gentoo)
print(t_test_result)

# ANOVA
anova_result <- aov(body_mass_g ~ species, 
                    data = penguins_clean)
summary(anova_result)

# =============================
# Regresión Lineal
# =============================

# Regresión lineal simple
model_simple <- lm(body_mass_g ~ flipper_length_mm, 
                   data = penguins_clean)
summary(model_simple)


# =============================
# Frequency Tables for Categorical Data
# =============================
table(penguins_clean$species)

prop.table(table(penguins_clean$species))

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

penguins_clean %>% 
  summarise(
    cov_bill = cov(bill_length_mm, bill_depth_mm),
    cor_bill = cor(bill_length_mm, bill_depth_mm)
  ) %>% print()

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
# PCA
# =============================

# Instala los paquetes si no los tienes
# install.packages(c("palmerpenguins", "ggplot2", "dplyr", "factoextra"))

# Carga los paquetes
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(factoextra)

# Seleccionar variables numéricas y eliminar NAs
penguins_numeric <- penguins %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  na.omit()
# Escalar los datos
penguins_scaled <- scale(penguins_numeric)

## Paso 5: Realizar el PCA
  
# Realizar el PCA
penguins_pca <- prcomp(penguins_scaled, center = TRUE, scale. = TRUE)

# Ver el resumen del PCA
summary(penguins_pca)

# Proporción de varianza**: Cuánta varianza explica cada componente.
# Varianza acumulada**: La suma acumulativa de varianza explicada.
 
#  Veamos los **autovalores** (varianza explicada por cada componente) y el **scree plot**:

# Scree plot
fviz_eig(penguins_pca)

# Los **autovalores** indican la cantidad de varianza explicada por cada componente.
# El **scree plot** ayuda a determinar el número óptimo de componentes a retener, 
#buscando el "codo" de la gráfica.
 
# Paso 7: Cargar y visualizar las variables
  
# Cargas de las variables
fviz_pca_var(penguins_pca,
             col.var = "contrib", # Colorear por contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)       # Evitar solapamiento de etiquetas


# Las flechas muestran las variables originales.
# La dirección y longitud indican la contribución y correlación con los componentes.
# Variables cercanas entre sí están correlacionadas.

## Paso 8: Visualizar los individuos (pingüinos)
  
# Agregar información de especies
penguins_pca_data <- data.frame(penguins_pca$x, 
                                species = penguins$species[!is.na(penguins$bill_length_mm)])

# Gráfico de individuos
ggplot(penguins_pca_data, aes(PC1, PC2, color = species)) +
  geom_point(size = 3) +
  labs(title = "PCA de Pingüinos",
       x = "Componente Principal 1",
       y = "Componente Principal 2") +
  theme_minimal()

# Este gráfico muestra la distribución de las observaciones (pingüinos) 
# en el espacio de los dos primeros componentes principales.
# Los colores representan las especies, permitiendo ver 
# si hay separación o solapamiento entre ellas.

 
# 1. **Varianza Explicada**:
#   - Los primeros dos componentes suelen explicar la mayor parte de la varianza. 
# Si juntos explican más del 70-80%, el PCA es una buena representación de los datos en menor dimensión.
# 
# 2. **Contribución de las Variables**:
#   - Observando las **cargas**, se puede interpretar qué variables están más 
# correlacionadas con cada componente.
# 
# 3. **Agrupamiento por Especies**:
#   - Si las especies se agrupan claramente en el gráfico de individuos,
# esto sugiere que las medidas morfológicas (longitud del pico, aletas, etc.) son útiles para diferenciar entre especies de pingüinos.

# =============================
# Model Linear Regresion simple
# =============================

# Simple linear regression model
model <- lm(body_mass_g ~ 
              flipper_length_mm, 
            data = penguins_clean)
summary(model)

# Plot the regression line
ggplot(penguins_clean, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Linear Regression: Body Mass vs Flipper Length")

# lm(formula = body_mass_g ~ flipper_length_mm, data = penguins_clean)

# This indicates that the **body mass** (dependent variable) is being modeled as a function of
# **flipper length** (independent variable) using the `lm()` function.
#
#
# ### **2. Residuals:**

# Min       1Q   Median       3Q      Max
# -1057.33  -259.79   -12.24   242.97  1293.89
# ```
# - **Residuals** represent the differences between the observed values (body mass)
#and the predicted values from the regression model.
# - The residuals range from -1057.33 to 1293.89.
# - **1st Quartile (1Q)**: -259.79 (lower bound of the data distribution).
# - **Median**: -12.24 (the middle value).
# - **3rd Quartile (3Q)**: 242.97 (upper bound of the data distribution).
# - **Max**: 1293.89 (the largest residual).
#
# Ideally, residuals should be evenly distributed around 0 for a good model fit.
# The spread of residuals suggests some variance, but there's no major sign of non-linearity in this output.
#
# ---
#
#   ### **3. Coefficients:**

# Estimate Std. Error t value Pr(>|t|)
# (Intercept)       -5872.09     310.29  -18.93   <2e-16 ***
#   flipper_length_mm    50.15       1.54   32.56   <2e-16 ***
#   ```
# - **Intercept**: -5872.09
# - This is the estimated value of **body mass** when **flipper length** is 0 mm.
#In a real-world context, this intercept doesn’t make sense because a flipper length of 0
#mm is unrealistic. It’s just a mathematical value.
#
# - **Flipper_length_mm coefficient**: 50.15
# - For every 1 mm increase in **flipper length**, **body mass** increases by
# approximately **50.15 grams**. This is the **slope** of the regression line.
#
# - **Standard Error**: 310.29 (intercept) and 1.54 (flipper length coefficient)
# - This measures the accuracy of the coefficient estimates.
# Smaller values indicate more precise estimates.
#
# - **t-value**:  -18.93 (Intercept) and 32.56 (flipper length coefficient)
# - The t-value tests the hypothesis that the coefficient is significantly different from zero.
# The larger the t-value, the stronger the evidence against the null hypothesis.
#
# - **Pr(>|t|)**:  < 2e-16 (both coefficients)
# - This is the **p-value** for each coefficient. A value below **0.05**
# indicates statistical significance, meaning there's strong evidence that
# the coefficient is not equal to zero. Here, both the intercept and the slope are
# **highly significant** with p-values much smaller than 0.05.
#
# ---
#
# ### **4. Model Summary:**
# ```
# Residual standard error: 393.3 on 331 degrees of freedom
# Multiple R-squared:  0.7621,	Adjusted R-squared:  0.7614
# F-statistic:  1060 on 1 and 331 DF,  p-value: < 2.2e-16
# ```
#
# - **Residual Standard Error**: 393.3
#   - This is the average distance that the observed values deviate from the
# regression line. A lower value indicates a better fit, although this needs to
# be interpreted relative to the scale of **body mass**.
#
# - **Multiple R-squared**: 0.7621
#   - This indicates that about **76.21%** of the variability in **body mass**
# can be explained by the **flipper length**. A value closer to 1 suggests a better fit of the model.
#
# - **Adjusted R-squared**: 0.7614
#   - This is similar to R-squared but adjusts for the number of predictors in the model.
# It is used to assess the model’s fit when there are multiple predictors,
# although in this case, we have only one predictor.
#
# - **F-statistic**: 1060 on 1 and 331 degrees of freedom, p-value: < 2.2e-16
#   - The **F-statistic** tests whether at least one predictor variable has a
# non-zero coefficient (i.e., the model is a good fit). Here, the F-statistic is very large,
# and the p-value is extremely small, indicating that the model explains the data well.
#
# ---
#
# ### **Interpretation:**
#
# - **Relationship**: There is a **strong, positive relationship** between **flipper length** and **body mass**. As flipper length increases by 1 mm, **body mass** increases by approximately **50.15 grams**.
# - **Model Fit**: The **Multiple R-squared value** (0.7621) suggests that the model explains about 76% of the variance in **body mass** based on **flipper length**.
# - **Significance**: Both the intercept and the slope are highly significant, with **p-values** near **0** (less than 0.05), suggesting that the relationship between **flipper length** and **body mass** is statistically significant.
# - **Error**: The **residual standard error** of 393.3 suggests that the predictions of body mass from this model could be off by around 393 grams on average. This might be large compared to the range of body masses (which is typically less than 10000 grams), so there might still be other factors affecting **body mass**.
#

# Regresión lineal múltiple
model_multiple <- lm(body_mass_g ~ flipper_length_mm + 
                       bill_length_mm + 
                       bill_depth_mm, 
                     data = penguins_clean)
summary(model_multiple)


# Check residuals vs fitted values plot to assess model fit
ggplot(data = penguins_clean, aes(x = model_multiple$fitted.values, y = model_multiple$residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values (Predicted Body Mass)",
    y = "Residuals"
  ) +
  theme_minimal() +
  geom_smooth(method = "lm", color = "red", linetype = "dashed")

# Checking the residuals for normality
ggplot(data = penguins_clean, aes(x = model_multiple$residuals)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Residuals Distribution",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal()

# Plotting actual vs predicted values
ggplot(penguins_clean, aes(x = model_multiple$fitted.values, y = body_mass_g)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Actual vs Predicted Body Mass",
    x = "Predicted Body Mass",
    y = "Actual Body Mass"
  ) +
  theme_minimal()

## Interpretation
#   
#   ### **Model Overview:**
#   The model you ran predicts `body_mass_g` (body mass of penguins) based on three predictors:
#   1. `flipper_length_mm` (flipper length in mm),
# 2. `bill_length_mm` (bill length in mm),
# 3. `bill_depth_mm` (bill depth in mm).
# 
# ### **1. Residuals:**
# The residuals represent the differences between the observed and predicted values for the `body_mass_g`. The values provided are:
#   
#   - **Min:** -1051.37 (the smallest residual),
# - **1st Quartile (1Q):** -284.50 (25th percentile of residuals),
# - **Median:** -20.37 (50th percentile of residuals),
# - **3rd Quartile (3Q):** 241.03 (75th percentile of residuals),
# - **Max:** 1283.51 (the largest residual).
# 
# These values suggest that the residuals are somewhat spread out, 
# with the median being close to zero, which is expected. The range of residuals shows some
# relatively large errors, particularly on the high side (positive residuals).
# 
# ### **2. Coefficients:**
# The coefficients represent the estimated effect of each predictor variable on the outcome 
# (`body_mass_g`). Here’s a breakdown of each coefficient:
#   
#   - **Intercept:** -6445.476
# - This is the estimated body mass when all predictor variables (flipper length, bill length, and bill depth) 
# are zero. In practice, the intercept may not always have a meaningful interpretation but is necessary 
# for the linear model calculation.
# 
# - **Flipper length (`flipper_length_mm`):** 50.762
# - For every 1 mm increase in `flipper_length_mm`, the body mass (`body_mass_g`) 
# increases by approximately 50.76 grams. This predictor is highly significant with a very low p-value (`< 2e-16`), 
#indicating a strong relationship with body mass.
# 
# - **Bill length (`bill_length_mm`):** 3.293
# - For every 1 mm increase in `bill_length_mm`, the body mass (`body_mass_g`)
# increases by approximately 3.29 grams. However, the p-value for this coefficient is 0.540,
# which is **not statistically significant** (p-value > 0.05). This means bill length is 
# not a strong predictor of body mass in this model.
# 
# - **Bill depth (`bill_depth_mm`):** 17.836
# - For every 1 mm increase in `bill_depth_mm`, the body mass (`body_mass_g`) increases 
# by approximately 17.84 grams. However, the p-value for this coefficient is 0.198,
# which is **also not statistically significant** (p-value > 0.05). Bill depth does not 
# significantly contribute to predicting body mass in this model.
# 
# ### **3. Residual Standard Error:**
# - The **residual standard error** (393) is the average amount by which the model's predictions differ from the actual observed values. It gives a measure of how well the model fits the data. A smaller value is better, and 393 suggests that the model has moderate prediction error.
# 
# ### **4. R-squared and Adjusted R-squared:**
# - **Multiple R-squared:** 0.7639
#    - This means that about 76.4% of the variability in body mass (`body_mass_g`) can be explained by the three predictor variables (flipper length, bill length, and bill depth). This is a relatively high proportion of variance explained, indicating a good fit.
#    
# - **Adjusted R-squared:** 0.7618
#    - This value is adjusted for the number of predictors in the model, and it’s very close to the R-squared value, indicating that the inclusion of the predictors is adding value to the model and not overfitting.
# 
# ### **5. F-statistic:**
# - The **F-statistic** (354.9) tests whether the model as a whole is significant (i.e., whether the predictors are jointly useful for predicting body mass). The associated **p-value** (`< 2.2e-16`) is extremely small, meaning that at least one of the predictors is significantly related to the outcome variable. In other words, the model is highly significant.
# 
# ### **Summary:**
# 
# - **Flipper length (`flipper_length_mm`)** is the most important predictor of penguin body mass, with a statistically significant relationship.
# - **Bill length** and **bill depth** are not significant predictors in this model, as their p-values are much higher than 0.05.
# - The model explains a high percentage (76.4%) of the variability in body mass.
# - The overall model is statistically significant, with a very low p-value for the F-statistic.
# 
# This model can be useful for predicting penguin body mass based on flipper length, but the inclusion of bill length and bill depth does not add much value for this specific outcome.

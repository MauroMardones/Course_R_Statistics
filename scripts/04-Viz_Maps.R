# Visualization and Statistical Analysis with Penguins Dataset in Base R and tidyverse

# ==============================
# Base R Visualizations
# ==============================

# Load penguins dataset
if (!require("palmerpenguins")) install.packages("palmerpenguins")
library(palmerpenguins)

data("penguins")

# Remove rows with missing values
penguins_clean <- na.omit(penguins)

# Histogram: Flipper Length Distribution
hist(penguins_clean$bill_depth_mm,
     main = "Histogram of Flipper Length",
     xlab = "Flipper Length (mm)",
     col = "lightblue",
     border = "black")

# Scatter plot: Bill Length vs Bill Depth
plot(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm,
     main = "Bill Length vs Bill Depth",
     xlab = "Bill Length (mm)",
     ylab = "Bill Depth (mm)",
     pch = 19, col = "orange")

# Boxplot: Body Mass by Species
boxplot(body_mass_g ~ species,
        data = penguins_clean,
        main = "Boxplot of Body Mass by Species",
        xlab = "Species",
        ylab = "Body Mass (g)",
        col = c("lightgreen", "lightpink", "lightblue"))

# Linear Regression in Base R
# Relationship between body mass and flipper length
lm_model <- lm(body_mass_g ~ flipper_length_mm, data = penguins_clean)
summary(lm_model)

# Plot regression line
plot(penguins_clean$flipper_length_mm, penguins_clean$body_mass_g,
     main = "Regression: Body Mass vs Flipper Length",
     xlab = "Flipper Length (mm)", ylab = "Body Mass (g)",
     pch = 19, col = "skyblue")
abline(lm_model, col = "red", lwd = 2)

# ==============================
# tidyverse Visualizations
# ==============================

# Load necessary package
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# Histogram: Flipper Length
penguins_clean %>%
  ggplot(aes(x = flipper_length_mm, fill = species)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("lightgreen", "lightblue", "lightpink")) +
  labs(title = "Histogram of Flipper Length by Species",
       x = "Flipper Length (mm)",
       y = "Frequency") +
  theme_minimal()

# Scatter plot: Bill Length vs Bill Depth
penguins_clean %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_color_manual(values = c("orange", "purple", "skyblue")) +
  labs(title = "Bill Length vs Bill Depth",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)") +
  theme_light()

# Boxplot: Body Mass by Species
penguins_clean %>%
  ggplot(aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(outlier.color = "red", alpha = 0.5) +
  scale_fill_manual(values = c("lightcoral", "lightcyan", "lightgoldenrod")) +
  labs(title = "Boxplot of Body Mass by Species",
       x = "Species",
       y = "Body Mass (g)") +
  theme_classic()

# Linear Regression in tidyverse
lm_model_tidy <- penguins_clean %>%
  lm(body_mass_g ~ flipper_length_mm, data = .)
summary(lm_model_tidy)

# ### Interpretación del Modelo de Regresión Lineal
# 
#   La relación evaluada es entre la masa corporal de los pingüinos (*body_mass_g*) y la longitud de las aletas (*flipper_length_mm*).
# 
# 1. **Intersección (Intercepto):** -5872.09.  
# Esto indica el valor estimado de la masa corporal cuando la longitud de las aletas es cero. Aunque carece de significado físico, actúa como un punto de referencia matemático.
# 
# 2. **Coeficiente de Pendiente:** 50.15.  
# Por cada milímetro adicional en la longitud de la aleta, la masa corporal aumenta en promedio 50.15 gramos.
# 
# 3. **Errores Estándar:**  
#   El error estándar del coeficiente de pendiente es 1.54, mostrando precisión en la estimación.
# 
# 4. **Valores p:**  
#   Ambos coeficientes tienen valores p menores que 0.001 (***), indicando significancia estadística.
# 
# 5. **Bondad del Ajuste:**  
#   - **R² múltiple:** 0.7621, lo que sugiere que el 76.21% de la variabilidad en la masa corporal es explicada por la longitud de las aletas.  
# - **Error estándar residual:** 393.3 gramos, representando la dispersión de los puntos alrededor de la línea de regresión.
# 
# 6. **F-Statistic:**  
#   La prueba F es altamente significativa con un p-valor < 2.2e-16, lo que confirma que el modelo tiene capacidad explicativa.

# Plot regression with tidyverse
penguins_clean %>%
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = "skyblue", size = 3) +
  geom_smooth(method = "loess",
              color = "red", se = TRUE) +
  labs(title = "Linear Regression: Body Mass vs Flipper Length",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)") +
  theme_minimal()

# Multivariate Analysis Example (PCA)

#El análisis multivariado (PCA) con la base de datos penguins tiene sentido porque 
#esta contiene múltiples variables cuantitativas que describen diferentes características 
#físicas de los pingüinos (longitud y profundidad del pico, longitud de las aletas y peso corporal). 
#Estas variables están correlacionadas y pueden proporcionar información conjunta sobre la variabilidad 
#morfológica de las especies. 
#El PCA permite reducir la dimensionalidad de los datos, identificar patrones, 
#y visualizar cómo las especies pueden agruparse en función de sus características morfológicas principales.

# Base R PCA
pca_data <- penguins_clean[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")]
pca_result <- prcomp(na.omit(pca_data), scale. = TRUE)
summary(pca_result)

# PCA Plot in Base R
biplot(pca_result, main = "PCA of Penguins Data")

# PCA with tidyverse and factoextra
if (!require("factoextra")) install.packages("factoextra")
library(factoextra)

penguins_clean %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  drop_na() %>%
  prcomp(scale = TRUE) %>%
  fviz_pca_biplot(title = "PCA Biplot of Penguins")

#   
#   1. **Ejes principales:**  
#   - El eje horizontal (Dim1) explica el 68.6% de la variación en los datos.
# - El eje vertical (Dim2) explica el 19.5% de la variación.
# Juntos, estos ejes capturan aproximadamente el 88.1% de la variabilidad total de los datos.
# 
# 2. **Puntos individuales:**  
#   Cada punto representa una observación (individuo o muestra de pingüino) en función de las combinaciones de sus características principales después del PCA. Los números parecen representar IDs de observaciones.
# 
# 3. **Vectores (flechas azules):**  
#   Los vectores indican la dirección y contribución de las variables originales (como `bill_length_mm`, `bill_depth_mm`, etc.) a las componentes principales:
#   - La longitud de las flechas muestra la importancia de esa variable para explicar la variabilidad en los datos.
# - El ángulo entre los vectores sugiere correlaciones:
#   - Flechas en la misma dirección indican una alta correlación positiva.
# - Flechas en direcciones opuestas indican correlación negativa.
# - Flechas perpendiculares indican que las variables no están correlacionadas.
# 
# 4. **Interpretación específica de los vectores:**  
#   - `bill_length_mm` parece estar positivamente relacionado con Dim1 y tiene un fuerte peso en esta dimensión.
# - `bill_depth_mm` apunta en una dirección diferente, lo que sugiere una menor correlación con las otras variables.
# - `flipper_length_mm` y `body_mass_g` también tienen un peso importante en la variabilidad explicada.
# 
# 5. **Clustering:**  
#   Hay grupos visibles de puntos. Estos podrían reflejar diferentes especies o categorías en los datos (si esa información está disponible).
# 

# Instalación y carga del paquete

library(ggplot2)
library(cluster)

# Seleccionar solo las columnas numéricas necesarias (removiendo NAs)
data <- na.omit(penguins[, c("bill_length_mm", "bill_depth_mm", 
                             "flipper_length_mm", "body_mass_g")])

### **2. Escalado de los datos**  
# Es importante escalar los datos antes del análisis de clúster.

scaled_data <- scale(data)

  ### **3. Selección del número óptimo de clústeres (Método Elbow)**  
  

wss <- sapply(1:10, function(k) {
  kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss
})

# Gráfico del codo
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de clusters",
     ylab = "Suma total de cuadrados dentro de los clusters")

### **4. Aplicación del algoritmo k-means**
  
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
data$cluster <- as.factor(kmeans_result$cluster)

### **5. Visualización de clústeres**  
#Podemos graficar los datos en función de las dos primeras dimensiones principales (PCA) para visualizar los grupos.


library(factoextra)

# Visualización en un espacio PCA
fviz_cluster(kmeans_result, data = scaled_data,
             geom = "point", ellipse.type = "convex",
             ggtheme = theme_minimal())

### **6. Evaluación de los resultados**  
#Puedes comparar los clústeres obtenidos con especies (si esa información está disponible) para evaluar la separación efectiva.


## Maps

# Instalar paquetes necesarios si no están instalados
install.packages(c("sf", "ggplot2", "dplyr", "ggspatial", "rnaturalearth", "rnaturalearthdata", "terra", "rasterVis", "ncdf4", "lubridate", "jsonlite", "httr"))

# Cargar librerías
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(rasterVis)
library(lubridate)
library(jsonlite)
library(httr)

# Obtener el mapa de Chile
chile <- ne_states(country = "Chile", returnclass = "sf")

# Filtrar para la Región de Los Lagos, que incluye Puerto Montt
region_puerto_montt <- chile %>%
  filter(name_en == "Los Lagos")

# Coordenadas aproximadas de Puerto Montt
lat_center <- -41.469
lon_center <- -72.942

# Generar puntos aleatorios cercanos a Puerto Montt
set.seed(42)  # Semilla para reproducibilidad
n_points <- 50
points_data <- data.frame(
  lon = runif(n_points, lon_center - 0.8, lon_center + 0.8),
  lat = runif(n_points, lat_center - 0.8, lat_center + 0.8),
  respiration_rate = rnorm(n_points, mean = 10, sd = 2)  # Tasa de respiración aleatoria
)

# Convertir a objeto espacial sf
points_sf <- st_as_sf(points_data, coords = c("lon", "lat"), crs = 4326)

# Plot del mapa con puntos y atributos
plot <- ggplot(data = region_puerto_montt) +
  geom_sf(fill = "lightblue") +
  geom_sf(data = points_sf, aes(color = respiration_rate), size = 3) +
  scale_color_viridis_c() +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true") +
  labs(
    title = "Puntos Aleatorios en Puerto Montt",
    subtitle = "Tasa de respiración de animales (simulada)",
    color = "Respiration Rate"
  ) +
  theme_minimal()

print(plot)


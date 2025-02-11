# Visualization and Statistical Analysis with Penguins Dataset in Base R and tidyverse

# ==============================
# Base R Visualizations
# ==============================

# Load penguins dataset
if (!require("palmerpenguins")) install.packages("palmerpenguins")
library(palmerpenguins)

data("penguins")

# Load and Clean Data
penguins <- na.omit(palmerpenguins::penguins)

### Visualization Techniques

## Base R Plots

# 1. Histogram of body mass
hist(penguins$body_mass_g, 
     main = "Histogram of Body Mass", 
     xlab = "Body Mass (g)", 
     col = "skyblue", 
     border = "white")

# 2. Boxplot of body mass by species
boxplot(body_mass_g ~ species, data = penguins, 
        main = "Body Mass by Species", 
        ylab = "Body Mass (g)", 
        col = c("tomato", "lightgreen", "lightblue"))

# 3. Scatter plot of flipper length vs body mass
plot(penguins$flipper_length_mm, penguins$body_mass_g, 
     main = "Flipper Length vs Body Mass", 
     xlab = "Flipper Length (mm)", 
     ylab = "Body Mass (g)", 
     pch = 19, col = "darkorange")

# 4. Bar plot of species counts
species_counts <- table(penguins$species)
barplot(species_counts, 
        main = "Counts of Penguin Species", 
        col = c("purple", "cyan", "gold"), 
        ylab = "Count")

# 5. Pie chart of species distribution
pie(species_counts, 
    main = "Species Distribution", 
    col = c("purple", "cyan", "gold"))

# 6. Density plot of bill length
plot(density(penguins$bill_length_mm), 
     main = "Density Plot of Bill Length", 
     xlab = "Bill Length (mm)", 
     col = "darkgreen")

# 7. Pairs plot for numeric variables
pairs(penguins[, sapply(penguins, is.numeric)], 
      main = "Pairs Plot for Penguins Data")

# 8. Stripchart of body mass by species
stripchart(body_mass_g ~ species, data = penguins, 
           vertical = TRUE, 
           main = "Stripchart of Body Mass by Species", 
           col = c("red", "blue", "green"))

# 9. Scatter plot with regression line
plot(penguins$bill_length_mm, penguins$bill_depth_mm, 
     main = "Bill Length vs Bill Depth", 
     xlab = "Bill Length (mm)", 
     ylab = "Bill Depth (mm)", 
     pch = 19, col = "blue")
abline(lm(bill_depth_mm ~ bill_length_mm, data = penguins), 
       col = "red", lwd = 2)


# ==============================
# tidyverse Visualizations
# ==============================

# Load necessary package
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# 10. Histogram using ggplot2
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Body Mass", x = "Body Mass (g)")

# 11. Boxplot of body mass by species
ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  labs(title = "Body Mass by Species", y = "Body Mass (g)")

# 12. Scatter plot of flipper length vs body mass
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  labs(title = "Flipper Length vs Body Mass", 
       x = "Flipper Length (mm)", 
       y = "Body Mass (g)")

# 13. Bar plot of species counts
ggplot(penguins, aes(x = species, fill = species)) +
  geom_bar() +
  labs(title = "Counts of Penguin Species")

# 14. Density plot of bill length
ggplot(penguins, aes(x = bill_length_mm, fill = species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Bill Length", x = "Bill Length (mm)")

# 15. Violin plot of body mass by species
ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_violin() +
  labs(title = "Violin Plot of Body Mass by Species", y = "Body Mass (g)")

# 16. Faceted scatter plot
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  facet_wrap(~species) +
  labs(title = "Bill Length vs Bill Depth by Species")

# 17. Line plot example (dummy cumulative data)
cumulative_data <- penguins %>% 
  group_by(species) %>% 
  mutate(cumulative_mass = cumsum(body_mass_g)) %>% 
  slice_head(n = 10)

ggplot(cumulative_data, aes(x = row_number(), y = cumulative_mass, color = species)) +
  geom_line() +
  labs(title = "Cumulative Body Mass by Species", x = "Observation", y = "Cumulative Mass (g)")

# 18. Heatmap example
penguins_numeric <- penguins %>% select(where(is.numeric))
cor_matrix <- cor(penguins_numeric, use = "complete.obs")

ggplot(melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(title = "Heatmap of Numeric Variable Correlations")

# 19. Dot plot
ggplot(penguins, aes(x = body_mass_g, y = species)) +
  geom_dotplot(binaxis = "x", stackdir = "center", dotsize = 0.5) +
  labs(title = "Dot Plot of Body Mass by Species")

# 20. Interactive plot using plotly
if (!require("plotly")) install.packages("plotly")
library(plotly)

p <- ggplot(penguins, aes(x = bill_length_mm,
                          y = flipper_length_mm, 
                          color = species)) +
  geom_point() +
  labs(title = "Interactive Plot Example")+
  theme_bw()
ggplotly(p)


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


# The multivariate analysis (PCA) with the penguins dataset makes sense because  
# it contains multiple quantitative variables that describe different physical  
# characteristics of the penguins (bill length and depth, flipper length, and body weight).  
# These variables are correlated and can provide joint information about the morphological  
# variability of the species.  
# PCA allows reducing data dimensionality, identifying patterns,  
# and visualizing how species can be grouped based on their main morphological characteristics. 



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
# 1. **Main Axes:**  
# - The horizontal axis (Dim1) explains 68.6% of the variation in the data.  
# - The vertical axis (Dim2) explains 19.5% of the variation.  
# Together, these axes capture approximately 88.1% of the total variability in the data.  
#  
# 2. **Individual Points:**  
# Each point represents an observation (individual or penguin sample) based on the combinations of its main characteristics after the PCA.  
# The numbers seem to represent observation IDs.  
#  
# 3. **Vectors (blue arrows):**  
# The vectors indicate the direction and contribution of the original variables (such as `bill_length_mm`, `bill_depth_mm`, etc.) to the principal components:  
# - The length of the arrows shows the importance of that variable in explaining data variability.  
# - The angle between the vectors suggests correlations:  
#   - Arrows in the same direction indicate a high positive correlation.  
#   - Arrows in opposite directions indicate a negative correlation.  
#   - Perpendicular arrows indicate no correlation between variables.  
#  
# 4. **Specific Interpretation of Vectors:**  
# - `bill_length_mm` appears to be positively related to Dim1 and has a strong weight in this dimension.  
# - `bill_depth_mm` points in a different direction, suggesting a lower correlation with the other variables.  
# - `flipper_length_mm` and `body_mass_g` also have a significant weight in explaining variability.  
#  
# 5. **Clustering:**  
# There are visible clusters of points. These could reflect different species or categories in the data (if that information is available).  


# call package necessary

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
fviz_cluster(kmeans_result, 
             data = scaled_data,
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


# Convertir a objeto espacial sf
points_sf <- st_as_sf(points_data, coords = c("lon", "lat"), crs = 4326)

# ================================
# Mapa de puntos básicos
ggplot(data = region_puerto_montt) +
  geom_sf(fill = "lightblue") +
  geom_sf(data = points_sf, aes(color = respiration_rate), size = 3) +
  scale_color_viridis_c() +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true") +
  labs(title = "Mapa de Puntos Aleatorios") +
  theme_minimal()

# ================================
# Mapa de calor
ggplot(data = region_puerto_montt) +
  geom_sf(fill = "lightgrey") +
  stat_density_2d(data = points_data, aes(x = lon, y = lat, fill = ..level..), geom = "polygon") +
  scale_fill_viridis_c(option="C") +
  labs(title = "Mapa de Calor de Tasas de Respiración") +
  theme_minimal()

# ================================
# Mapa de puntos escalados por atributo
ggplot(data = region_puerto_montt) +
  geom_sf(fill = "lightgreen") +
  geom_point(data = points_data, aes(x = lon, y = lat, size = respiration_rate, color = respiration_rate)) +
  scale_color_viridis_c() +
  labs(title = "Mapa de Puntos Escalados por Tasa de Respiración") +
  theme_minimal()

# ================================
# Mapa con líneas de contorno
ggplot(data = region_puerto_montt) +
  geom_sf(fill = "lightyellow") +
  stat_density_2d(data = points_data, aes(x = lon, y = lat, color = ..level..), geom = "density2d") +
  scale_color_viridis_c() +
  labs(title = "Mapa con Líneas de Contorno") +
  theme_minimal()

# ================================
# Mapa por cuadrantes
points_data <- points_data %>%
  mutate(
    lat_group = cut(lat, breaks = 4),
    lon_group = cut(lon, breaks = 4)
  )

ggplot(points_data, aes(x = lon, y = lat, color = respiration_rate)) +
  geom_point(size = 3) +
  facet_grid(lat_group ~ lon_group) +
  scale_color_viridis_c() +
  labs(title = "Mapa Facetado por Cuadrantes") +
  theme_minimal()


# ================================
# Cargar shapefile en R
# ruta_shapefile <- "ruta/del/archivo.shp"
# shapefile_data <- st_read(ruta_shapefile)
# plot(st_geometry(shapefile_data))

# ================================
# Cargar archivo Excel con datos georreferenciados
# ruta_excel <- "ruta/del/archivo.xlsx"
# datos_excel <- read_excel(ruta_excel)
# sf_datos <- st_as_sf(datos_excel, coords = c("lon", "lat"), crs = 4326)
# ggplot() +
#   geom_sf(data = region_puerto_montt, fill = "lightgrey") +
#   geom_sf(data = sf_datos, color = "red", size = 3) +
#   theme_minimal()




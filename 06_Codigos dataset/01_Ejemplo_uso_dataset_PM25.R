###############################################################
# Ejemplo de uso de los datos del dataset LATAM_PM2.5_1km
# Objetivo: Mostrar cómo leer, visualizar y extraer valores
#           de los mapas anuales de PM2.5 predicho.
###############################################################

# Librerias
library(raster)      # lectura y manejo de archivos raster
library(leaflet)     # mapa interactivo
library(sf)          # para trabajar con puntos definidos por el usuario

# 1. Directorio donde se encuentran las imágenes (.tif)
directorio <- "tu_directorio"
setwd(directorio)

# 2. Listar todas las imágenes disponibles en el directorio
archivos <- list.files(pattern = "\\.tif$", full.names = TRUE)
print(archivos)

# 3. Abrir un archivo individual (ejemplo: mapa del año 2015 para Santiago)
PM25_2015 <- raster("XGB_PM2.5_Y_2015_ST_V1.1.tif")

# 4. Visualización básica
plot(PM25_2015, main = "PM2.5 Anual - Año 2015 (Santiago)")

# 5. Extraer el valor de PM2.5 en un punto especifico
# Coordenadas de ejemplo (latitud, longitud)
punto <- data.frame(lon = -70.6506, lat = -33.4372)  # Santiago centro
punto_sf <- st_as_sf(punto, coords = c("lon", "lat"), crs = 4326)

# Extraer valor (la imagen ya está en WGS84)
valor_PM25 <- extract(PM25_2015, punto)
print(paste("Valor de PM2.5 en el punto elegido:", round(valor_PM25,2), "µg/m³"))

# 6. Crear un mapa interactivo 
leaflet() %>%
  addTiles() %>%
  addRasterImage(PM25_2015, opacity = 0.7) %>%
  addMarkers(lng = punto$lon, lat = punto$lat,
             popup = paste("PM2.5:", round(valor_PM25, 2), "µg/m³"))

# 7. Leer todas las imagenes del directorio como un Stack ----------
stack_PM25 <- stack(archivos)

# Mostrar nombres de las capas
names(stack_PM25)

# Plotear todas las capas juntas
plot(stack_PM25)

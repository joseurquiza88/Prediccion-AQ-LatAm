#directorio = "D:/Josefina/Proyectos/ProyectoChile/CH/modelos/Salidas/SalidasAnuales/01-XGB-CV-M1-190625-CH/base"
###############################################################
# Ejemplo de uso de los datos del dataset LATAM_PM2.5_1km
# Objetivo: Mostrar cómo leer, visualizar y extraer valores
#           de los mapas anuales de PM2.5 predicho.
###############################################################

# Librerias
library(raster)      # lectura y manejo de archivos raster
library(leaflet)     # mapa interactivo
library(sf)          # para trabajar con puntos definidos por el usuario

# -------------------------------------------------------------
# 1. Directorio donde se encuentran las imágenes (.tif)
directorio <- "tu_directorio"
setwd(directorio)


# -------------------------------------------------------------
# 2. Listar todas las imágenes disponibles en el directorio
archivos <- list.files(pattern = "tif", full.names = TRUE)
print(archivos)

PM25_2015 <- raster("XGB_PM2.5_Y_2015_ST_V1.1.tif")


# -------------------------------------------------------------
# 4. Metricas basicas del raster

media_dominio <- mean(values(PM25_2015), na.rm = TRUE)
sd_dominio <- sd(values(PM25_2015), na.rm = TRUE)
print(c("Media:", round(media_dominio,2)))
print(c("Desviación estándar:", round(sd_dominio,2)))

# -------------------------------------------------------------
# 5. Visualización básica
plot(PM25_2015, main = "PM2.5 Anual - Año 2015 (Santiago)")


# -------------------------------------------------------------
# 6. Extraer el valor de PM2.5 en un punto especifico
# Coordenadas de ejemplo (latitud, longitud)
punto <- data.frame(lon = -70.6506, lat = -33.4372)  # Santiago centro
punto_sf <- st_as_sf(punto, coords = c("lon", "lat"), crs = 4326)

# Extraer valor (la imagen ya está en WGS84)
valor_PM25 <- extract(PM25_2015, punto)
print(paste("Valor de PM2.5 en el punto elegido:", round(valor_PM25,2), "µg/m³"))


# -------------------------------------------------------------
# 7. Crear un mapa interactivo 
# Crear paleta de colores basada en los valores del raster

# Crear paleta de colores personalizada
pal_colores <- colorNumeric(
  palette = colorRampPalette(c("#1a9850", "#ffffbf", "#d73027"))(100),
  domain = values(PM25_2015),  # rango de valores del raster
  na.color = "transparent"
)

# Mapa interactivo con leyenda
leaflet() %>%
  addTiles() %>%
  addRasterImage(PM25_2015, colors = pal_colores, opacity = 0.7) %>%
  addMarkers(lng = punto$lon, lat = punto$lat,
             popup = paste("PM2.5:", round(valor_PM25, 2), "µg/m³")) %>%
  addLegend(
    pal = pal_colores,
    values = values(PM25_2015),
    title = "PM2.5 (µg/m³)",
    position = "bottomright"
  )


# -------------------------------------------------------------
# 8. Leer todas las imagenes del directorio como un Stack 
stack_PM25 <- stack(archivos)

# Mostrar nombres de las capas
names(stack_PM25)

# Plotear todas las capas juntas
plot(stack_PM25)


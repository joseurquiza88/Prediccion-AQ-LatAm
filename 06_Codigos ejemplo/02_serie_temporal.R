###############################################################
# Ejemplo de uso de los datos del dataset LATAM_PM2.5_1km
# Objetivo: Hacer una serie temporal anual de un punto de monitoreo 
# Ejemplo SINCA Estacion O'higgins
###############################################################

# Librerias
library(raster)      # lectura y manejo de archivos raster
library(leaflet)     # mapa interactivo
library(sf)          # para trabajar con puntos definidos por el usuario
library(tidyverse) 
library(ggplot2)
# -------------------------------------------------------------
# 1. Directorio donde se encuentran las imágenes (.tif)
directorio <- "tu_directorio"
setwd(directorio)


# -------------------------------------------------------------
# 2. Listar todas las imágenes disponibles en el directorio
archivos <- list.files(pattern = "tif", full.names = TRUE)
print(archivos)


# -------------------------------------------------------------
# 3. Leer todas las imagenes del directorio como un Stack de datos
stack_PM25 <- stack(archivos)


# -------------------------------------------------------------
# 4. Definir el punto de interés ((Estacion O'higgins))
punto <- data.frame(lon = -70.66082895, lat = -33.46415783)
punto_sf <- st_as_sf(punto, coords = c("lon","lat"), crs = 4326)


# -------------------------------------------------------------
# 5. Extraer valor de cada capa del stack
valores <- extract(stack_PM25, punto_sf)
print(valores)


# -------------------------------------------------------------
# 6. Setear el dataset y transformar en dataframe
colnames(valores) <- names(stack_PM25)

df <- valores %>%
  as.data.frame() %>%
  pivot_longer(
    cols = everything(),
    names_to = "Capa",   # nombre de la capa o año
    values_to = "PM25"   # valor de PM2.5
  )%>%
  mutate(year = as.numeric(str_extract(Capa, "\\d{4}")))

print(df)


# -------------------------------------------------------------
# 7. Generar serie temporal anual



ggplot(df, aes(x = year, y = PM25)) +
  geom_line(color = "#225ea8", size = 1) +       # línea de la serie
  geom_point(color = "#0c2c84", size = 3) +       # puntos para cada año
  labs(
    title = "Evolución anual de PM2.5",
    x = "Año",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_classic()+
  scale_x_continuous(breaks = df$year)    # mostrar todos los años en el eje X


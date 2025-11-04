
# Poblacion AMS recortada en QGIS
# Gridded Population of the World, Version 4 (GPWv4): Population Density, Revision 11 (2020)

poblacion_AMS <- raster("D:/Josefina/Proyectos/ProyectoChile/CH/shapes/Comunas_AreaMetropolitanaSantiago/Poblacion_30sec_AMS_recortada.tif")

values(poblacion_AMS)
summary(poblacion_AMS)
# Hay muchos valores NA porque? donde estan ubicados?
plot(is.na(poblacion_AMS), 
     main = "Ubicación de los valores NA",
     col = c("transparent", "red"))

# Son los alrededores esta ok la imagen

# Remover valores NA y sumar
total_poblacion <- sum(values(poblacion_AMS), na.rm = TRUE)
total_poblacion

# Calcular área real de cada píxel en km2
pixel_area <- area(poblacion_AMS) # en km2

# Calcular población total ajustada por el área
poblacion_total_real <- cellStats(poblacion_AMS * pixel_area, stat = 'sum', na.rm = TRUE)

poblacion_total_real

#Ojo!!
# Censo 2024: RMS tenía 7.400.741 hab.
# El producto me da 8.672.396
# Recuadro tesis 7.518.280

# Recuadro considerado para la tesis

poblacion_tesis <- raster("D:/Josefina/Proyectos/Tesis/Poblacion/densidad_poblacion_CH.tif")
# Poblacion real en el recuadro de la tesis
# Calcular área real de cada píxel en km2
pixel_area_tesis <- area(poblacion_tesis) # en km2
# Calcular población total ajustada por el área
poblacion_total_real_tesis <- cellStats(poblacion_tesis * pixel_area_tesis, stat = 'sum', na.rm = TRUE)

poblacion_total_real_tesis

# Resumen Ojo!!

# Censo 2024: RMS tenía 7.400.741 hab.
# El producto me da 8.672.396
# Recuadro tesis 7.518.280



############################################################
# COMPARACIÓN DE RASTERS DE POBLACIÓN
# Objetivo: conservar solo los píxeles de poblacion_AMS 
# que no están presentes en poblacion_tesis
############################################################

# Limpiar entorno y cargar librerías
rm(list = ls())


# --- 1. Cargar los rasters ---
poblacion_AMS <- raster("D:/Josefina/Proyectos/ProyectoChile/CH/shapes/Comunas_AreaMetropolitanaSantiago/Poblacion_30sec_AMS_recortada.tif")
poblacion_tesis <- raster("D:/Josefina/Proyectos/Tesis/Poblacion/densidad_poblacion_CH.tif")

# --- 2. Comprobar resoluciones y extensión ---
print(res(poblacion_AMS))
print(res(poblacion_tesis))
print(extent(poblacion_AMS))
print(extent(poblacion_tesis))

# --- 3. Verificar cuántos píxeles tienen datos en cada raster ---
cat("Pixeles con datos en AMS:", sum(!is.na(values(poblacion_AMS))), "\n")
cat("Pixeles con datos en Tesis:", sum(!is.na(values(poblacion_tesis))), "\n")

# --- 4. Re-alinear grids (por si hay ligeras diferencias sub-píxel) ---
poblacion_tesis_res <- resample(poblacion_tesis, poblacion_AMS, method = "ngb")

# --- 5. Crear raster con píxeles exclusivos de AMS ---
poblacion_AMS_exclusiva <- poblacion_AMS
poblacion_AMS_exclusiva[!is.na(poblacion_tesis_res)] <- NA

# --- 6. Visualizar ---
plot(poblacion_AMS_exclusiva, main = "Píxeles exclusivos de AMS (no incluidos en Tesis)")

# --- 7. Verificar resultado con tabla cruzada ---
tabla <- crosstab(!is.na(poblacion_AMS), !is.na(poblacion_tesis_res))
print(tabla)

# --- 8. Contar píxeles exclusivos ---
pixeles_exclusivos <- sum(!is.na(values(poblacion_AMS_exclusiva)))
cat("Pixeles exclusivos de AMS:", pixeles_exclusivos, "\n")

# --- 9. Verificar Guardar resultado ---
writeRaster(poblacion_AMS_exclusiva,
            "D:/Josefina/Proyectos/ProyectoChile/CH/shapes/Comunas_AreaMetropolitanaSantiago/Poblacion_AMS_exclusiva.tif",
            overwrite = TRUE)

# --- 10. Controlar poblacion ---

# Censo 2024: RMS tenía 7.400.741 hab.
# El producto me da 8.672.396
# Recuadro tesis 7.518.280


# Calcular área real de cada píxel en km2
pixel_AMS_exclusiva <- area(poblacion_AMS_exclusiva) # en km2
# Calcular población total ajustada por el área
poblacion_total_AMS_exclusiva<- cellStats(poblacion_AMS_exclusiva * pixel_AMS_exclusiva, stat = 'sum', na.rm = TRUE)
### Dif producto AMS y recuadro tesis: 8672396-7518280 = 1154116
poblacion_total_AMS_exclusiva ## 1154116
 # % del recuadro de la tesis

(7518280/8672396)*100
#Esta ok el calculo :)


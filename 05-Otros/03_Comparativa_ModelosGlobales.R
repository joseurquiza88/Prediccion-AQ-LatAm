#######################################################################
## OBJETIVO: Extraccion de las concentraciones en los sitios de 
# mediciones de los # mapas obtenidos con modelos globales. 
# Luego se compara con las mediciones de cada uno de los sitios
#######################################################################


# Datos de PM25 Modelado a comparar
# https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V5.GL.05.02
# hay ditintas versiones del producto., por ahora uso esta
# https://wustl.app.box.com/v/ACAG-V5GL04-GWRPM25/folder/230734516599?page=5

#Github codigos ejemplo
# https://github.com/pmbusch/PM25-Satellite-Chile/tree/main

# Sitio
estacion <- "CH"
year<-2022
# Archivo donde estan los nombres de las estciones y las coordenadas
data_estacciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))
data_estacciones <- data_estacciones[data_estacciones$Considerado=="SI",]
data_estacciones <- data_estacciones[data_estacciones$tipo=="referencia",]
# el csv tiene una columna que se llama long y otra lat
monitors<- data_estacciones[, c("long", "lat")]

# directorio donde estan los archivos .nc
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/Comparativas_resultados/PM_WEI/",year,"/",sep="")

setwd(dir)
# Lista de los archivos en formato .nc
id <- list.files(path = dir,
                 pattern = "*.nc",
                 full.names = FALSE)

# Deberia tener 12, son mensuales los datos
print(length(id))

df_rbind <- data.frame()

# Recorro toda la carpeta con los archivos .nc y extriago los datos 
# en las coordenadas de las estaciones
for (i in 1:length(id)){
  print(i)
  file <- id[i]
    #conviete en raster
  # mp <- raster(file,varname = "GWRPM25",band=1)
  mp <- raster(file)
  #mp_chile <- crop(mp, extent(-76,-66, -54.85, -17.5))
  # Extraccion valores de las estaciones
  extracted_values <- raster::extract(mp, monitors, method="bilinear") # bilinear interpolates from the four nearest cell
  
  ## Unir datos de concentraciones con los datos de extraidos
  puntos_con_valores <- data_estacciones %>%
    mutate(extracted_values = extracted_values)
  # info extra para agregar al dataframe
  puntos_con_valores$monthYear <- substr(file, 16,21)
  puntos_con_valores$producto <- substr(file, 1,6)
  df_rbind <- rbind(df_rbind,puntos_con_valores)
}
View(df_rbind)

#Guardamos csv
save_dir <- "D:/Josefina/Proyectos/ProyectoChile/CH/Comparativas_resultados/PM_WEI/"
#write.csv(df_rbind,paste(save_dir,"data_PM-WUSTL-",year,".csv",sep=""))
write.csv(df_rbind,paste(save_dir,"data_PM-WEI-",year,".csv",sep=""))
setwd(save_dir)


############################################################################
############################################################################
## Unimos con datos de monitoreo de los sitios
# Se hace una validacion con los sitios de moniteoro
# Prueba para CH

setwd("D:/Josefina/Proyectos/ProyectoChile/CH/Comparativas_resultados/")
df_WUSTL <- read.csv("PM_wustl/data_PM-WUSTL-TOT.csv")
df_WEI <- read.csv("PM_WEI/data_PM-WEI-TOT.csv")
df_modelado <- read.csv("PM_modelado/data_PM-Modelado-TOT.csv")
# Promedios mensuales por estacion
df_SINCA <- read.csv("D:/Josefina/Proyectos/ProyectoChile/CH/proceed/06_estaciones/CH_estaciones.csv")
df_SINCA$date <- as.Date(df_SINCA$date,format = "%d/%m/%Y")

# Agrupamos datos por mes
df_SINCA_mes <- df_SINCA %>%
  mutate(mes = floor_date(date, "month")) %>%  
  group_by(estacion, mes) %>%
  summarise(media_SINCA = mean(Registros.completos, na.rm = TRUE)) %>%  # reemplaz√° "valor" por tu variable de inter√©s
  ungroup()
#Setear formato de dataframe mensual de las mediciones
#todos empiezan con dia 01 
df_SINCA_mes$date <- as.Date(df_SINCA_mes$mes,format = "%Y-%m-%d")

###
#Setear formato de dataframe mensual del modelo global
df_WUSTL$date <- as.Date(df_WUSTL$date,format = "%Y-%m-%d")
df_WEI$date <- as.Date(df_WEI$date,format = "%Y-%m-%d")
df_modelado$date <- as.Date(df_modelado$date,format = "%Y-%m-%d")

####
#Merge mediciones vs mi modelo
df_merge_modeladoSinca <- merge(df_modelado, df_SINCA_mes, by = c("date", "estacion"), all.x = TRUE)
df_merge_modeladoSinca$date<-as.Date(df_merge_modeladoSinca$date,format = "%Y-%m-%d")

####
#Merge datos anteriores vs el modelo global por dia y estaciones
df_merge_WUSTLModSinca <- merge(df_merge_modeladoSinca, df_WUSTL, by = c("date", "estacion"), all.x = TRUE)
df_merge_WUSTLModSinca <- df_merge_WUSTLModSinca[complete.cases(df_merge_WUSTLModSinca$extracted_values),]
df_merge_WUSTLModSinca_comp <- df_merge_WUSTLModSinca[complete.cases(df_merge_WUSTLModSinca$media_SINCA),]

# Mejoramos el dataframe
df_merge_WUSTLModSinca <- data.frame(date = df_merge_WUSTLModSinca$date,
                                     estacion = df_merge_WUSTLModSinca$estacion,
                                     SINCA = df_merge_WUSTLModSinca$media_SINCA,
                                     Model = df_merge_WUSTLModSinca$valor_raster,
                                     WUSTL = df_merge_WUSTLModSinca$extracted_values)
#Vemos si hay datos faltantes
df_merge_WUSTLModSinca <- df_merge_WUSTLModSinca [complete.cases(df_merge_WUSTLModSinca),]

# Asegurar que el data frame sea tibble
df_merge_WUSTLModSinca <- as_tibble(df_merge_WUSTLModSinca)

# Regresiones lineales: Model ~ SINCA y WUSTL ~ SINCA
modelo1 <- lm(Model ~ SINCA, data = df_merge_WUSTLModSinca)
modelo2 <- lm(WUSTL ~ SINCA, data = df_merge_WUSTLModSinca)

# Resumen
resumen1 <- glance(modelo1)
resumen2 <- glance(modelo2)

# Coeficientes
coef1 <- coef(modelo1)
coef2 <- coef(modelo2)

# Calcular las predicciones
pred_1 <- predict(modelo1)
pred_2 <- predict(modelo2)

# Calcular Bias y RMSE para cada modelo
bias_1 <- mean(pred_1 - df_merge_WUSTLModSinca$Model)
bias_2 <- mean(pred_2 - df_merge_WUSTLModSinca$WUSTL)

rmse_1 <- sqrt(mean((pred_1 - df_merge_WUSTLModSinca$Model)^2))
rmse_2 <- sqrt(mean((pred_2 - df_merge_WUSTLModSinca$WUSTL)^2))

# Armar data frame con etiquetas para los textos
etiquetas <- data.frame(
  modelo = c("Model", "WUSTL"),
  x = c(-Inf, -Inf),
  y = c(Inf, Inf),
  label = c(
    paste0("y = ", round(coef1[2], 2), "x + ", round(coef1[1], 2),
           "/nR2 = ", round(resumen1$r.squared, 2), 
           #", p = ", signif(resumen1$p.value, 2),
           "/nBias = ", round(bias_1, 4), "/nRMSE = ", round(rmse_1, 2)),
    paste0("y = ", round(coef2[2], 2), "x + ", round(coef2[1], 2),
           "/nR2 = ", round(resumen2$r.squared, 2), 
           #", p = ", signif(resumen2$p.value, 2),
           "/nBias = ", round(bias_2, 4), "/nRMSE = ", round(rmse_2, 2))
  )
)

# Transformar los datos para ggplot
df_plot <- df_merge_WUSTLModSinca %>%
  pivot_longer(cols = c(Model, WUSTL), names_to = "modelo", values_to = "y")

# Plot con facetas, SINCA como eje X
ggplot(df_plot, aes(x = SINCA, y = y)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  facet_wrap(~modelo, nrow = 1) +
  geom_text(data = etiquetas, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.1, size = 3.5, color = "black") +
  labs(
    #title = "Regresiones lineales: Model / WUSTL vs SINCA",
    x = "SINCA",
    y = "Model"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(hjust = 0.5)
  )

##########################################
########################################
df_merge_WEIModSinca <- merge(df_merge_modeladoSinca, df_WEI, by = c("date", "estacion"), all.x = TRUE)

df_merge_WEIModSinca <- data.frame(date = df_merge_WEIModSinca$date,
                                   estacion = df_merge_WEIModSinca$estacion,
                                   SINCA = df_merge_WEIModSinca$media_SINCA,
                                   Model = df_merge_WEIModSinca$valor_raster,
                                   WEI = df_merge_WEIModSinca$extracted_values)

df_merge_WEIModSinca <- df_merge_WEIModSinca [complete.cases(df_merge_WEIModSinca),]


############################################################################
############################################################################
#### Validacion con el raster completo pixel a pixel

#Path con todos los archivos del modelo global
dir_WUSTL <- "D:/Josefina/Proyectos/ProyectoChile/ModelosGlobales/WUSTL/"
setwd(dir_WUSTL)

# Lista de archivos .nc
id_WUSTL <- list.files(path = dir_WUSTL,
                       pattern = "*.nc",
                       full.names = FALSE)

cat("Cantidad de archivos WUSTL:", length(id_WUSTL), "\n")

# Extraer fechas del nombre (formato YYYYMM)
fechas_WUSTL <- substr(id_WUSTL, 26, 31)
date_WUSTL <- as.Date(paste0(fechas_WUSTL, "01"), format = "%Y%m%d")

# Crear dataframe con nombre y fecha
df_WUSTL <- data.frame(archivo_WUSTL = id_WUSTL, fecha_WUSTL = date_WUSTL)

# ---------------------------------------------------------------
#Modelo propio por centro urbano
estacion <- "CH"
modelo <- "01-XGB-CV-M1-190625-CH"
# Directorio con los valores mensuales del modelo selccionado
dir_modelo <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion,
                     "/modelos/salidas/SalidasMensuales/", modelo, "/")
setwd(dir_modelo)

# Lista de archivos .tif
id_modelo <- list.files(path = dir_modelo,
                        pattern = "*.tif",
                        full.names = FALSE)

cat("Cantidad de archivos del modelo propio:", length(id_modelo), "\n")

# Extraer fechas (formato MM-YYYY)
fechas_modelo <- substr(id_modelo, 9, 15)
date_modelo <- as.Date(paste0("01-", fechas_modelo), format = "%d-%m-%Y")

# Crear dataframe con nombre y fecha
df_modelo <- data.frame(archivo_modelo = id_modelo, fecha_modelo = date_modelo)


# ---------------------------------------------------------------
# Como no estan en el mismo orden, hay que emparejar los archivos 
# fecha

df_match <- inner_join(df_modelo, df_WUSTL,
                       by = c("fecha_modelo" = "fecha_WUSTL"))

cat("Fechas coincidentes encontradas:", nrow(df_match), "\n")
print(df_match)


# ---------------------------------------------------------------
# Recorremos carpeta para obtener la info de todo el dominio del
# centro urbano seleccionado
df_rbind <- data.frame()


for (i in 1:nrow(df_match)) {
  cat("Procesando mes:", format(df_match$fecha_modelo[i], "%Y-%m"), "\n")
  
  # Leer los rasters 
  raster_modelo <- raster(paste0(dir_modelo, df_match$archivo_modelo[i]))
  raster_WUSTL <- raster(paste0(dir_WUSTL, df_match$archivo_WUSTL[i]))
  
  # Recortar WUSTL al extent del modelo
  raster_WUSTL_recortado <- crop(raster_WUSTL, extent(raster_modelo))
  
  # Ajustar resoluciÛn (bilinear para valores continuos)
  raster_WUSTL_resample <- resample(raster_WUSTL_recortado, raster_modelo, method = "bilinear")
  
  # Convertir ambos en data frame
  df <- as.data.frame(stack(raster_modelo, raster_WUSTL_resample), xy = FALSE, na.rm = TRUE)
  colnames(df) <- c("modelo", "WUSTL")
  
  # Agregar columnas con las fechas de ambos modelos
  df$fecha_modelo <- df_match$fecha_modelo[i]
  # df$fecha_WUSTL <- df_match$fecha_WUSTL[i]
  
  # Agregar al dataset total
  df_rbind <- rbind(df_rbind, df)
  
}


# ---------------------------------------------------------------
# Calcular mÈtricas para el mes
fit <- lm(modelo ~ WUSTL, data = df_rbind)
r2 <- summary(fit)$r.squared
rmse <- sqrt(mean((df_rbind$modelo - df_rbind$WUSTL)^2))

r2
rmse

# ---------------------------------------------------------------
#Guardar csv
dir_resultados <- "D:/Josefina/Proyectos/ProyectoChile/ModelosGlobales/resultados/"
write.csv(df_rbind, paste(dir_resultados,"comparativa_pixel_",estacion, ".csv",sep=""))






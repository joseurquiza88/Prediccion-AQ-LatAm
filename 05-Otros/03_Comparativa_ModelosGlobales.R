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
#year<-2022
# Archivo donde estan los nombres de las estciones y las coordenadas
data_estacciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))
data_estacciones <- data_estacciones[data_estacciones$Considerado=="SI",]
data_estacciones <- data_estacciones[data_estacciones$tipo=="referencia",]
# el csv tiene una columna que se llama long y otra lat
monitors<- data_estacciones[, c("long", "lat")]

# directorio donde estan los archivos .nc
#dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/Comparativas_resultados/PM_WEI/",year,"/",sep="")
dir <- "D:/Josefina/Proyectos/ProyectoChile/ModelosGlobales/WUSTL/"
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
  puntos_con_valores$monthYear <- substr(file, 26,31)
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

# Datos extraido de los mapas propios generados
df_modelado <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/Comparativas_resultados/PM_modelado/data_PM-Modelado-TOT_",estacion,".csv",sep=""))
df_WUSTL <-df_rbind
# Promedios mensuales por estacion
df_estaciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/06_estaciones/",estacion,"_estaciones.csv",sep=""))
df_estaciones$date <- as.Date(df_estaciones$date,format = "%d/%m/%Y")
# solo para CH
df_estaciones$mean <- df_estaciones$Registros.completos
# Agrupamos datos por mes
df_estaciones_mes <- df_estaciones %>%
  mutate(mes = floor_date(date, "month")) %>%  
  group_by(estacion, mes) %>%
  summarise(media_SINCA = mean(mean, na.rm = TRUE)) %>%  # reemplaz√° "valor" por tu variable de inter√©s
  ungroup()
#Setear formato de dataframe mensual de las mediciones
#todos empiezan con dia 01 
df_estaciones_mes$date <- as.Date(df_estaciones_mes$mes,format = "%Y-%m-%d")

###
#Setear formato de dataframe mensual del modelo global
df_WUSTL$date <- as.Date(paste0(df_WUSTL$monthYear, "01"), format = "%Y%m%d")
# df_WEI$date <- as.Date(df_WEI$date,format = "%Y-%m-%d")
df_modelado$date <- as.Date(df_modelado$date,format = "%Y-%m-%d")

####
#Merge mediciones vs mi modelo
df_merge_modelado_Estacion <- merge(df_modelado, df_estaciones_mes, by = c("date", "estacion"), all.x = TRUE)
df_merge_modelado_Estacion$date<-as.Date(df_merge_modelado_Estacion$date,format = "%Y-%m-%d")

####
#Merge datos anteriores vs el modelo global por dia y estaciones
df_merge_WUSTLModEstacion <- merge(df_merge_modelado_Estacion, df_WUSTL, by = c("date", "estacion"), all.x = TRUE)
df_merge_WUSTLModEstacion <- df_merge_WUSTLModEstacion[complete.cases(df_merge_WUSTLModEstacion$extracted_values),]
df_merge_WUSTLModEstacion_comp <- df_merge_WUSTLModEstacion[complete.cases(df_merge_WUSTLModEstacion$media_SINCA),]

# Mejoramos el dataframe
df_merge_WUSTLModEstacion <- data.frame(date = df_merge_WUSTLModEstacion_comp$date,
                                     estacion = df_merge_WUSTLModEstacion_comp$estacion,
                                     mediciones = df_merge_WUSTLModEstacion_comp$media_SINCA,
                                     Model = df_merge_WUSTLModEstacion_comp$valor_raster,
                                     WUSTL = df_merge_WUSTLModEstacion_comp$extracted_values)
#Vemos si hay datos faltantes
df_merge_WUSTLModEstacion <- df_merge_WUSTLModEstacion [complete.cases(df_merge_WUSTLModEstacion),]

# Asegurar que el data frame sea tibble
df_merge_WUSTLModEstacion <- as_tibble(df_merge_WUSTLModEstacion)

# Regresiones lineales: Model ~ SINCA y WUSTL ~ Mediciones
modelo1 <- lm(Model ~ mediciones, data = df_merge_WUSTLModEstacion)
modelo2 <- lm(WUSTL ~ mediciones, data = df_merge_WUSTLModEstacion)
modelo3 <- lm(Model~ WUSTL, data = df_merge_WUSTLModEstacion)
r2_modelo1 <- summary(modelo1)$r.squared
r2_modelo2 <- summary(modelo2)$r.squared
r2_modelo3 <- summary(modelo3)$r.squared
r2_modelo1
r2_modelo2
r2_modelo3


# Coeficientes
coef1 <- coef(modelo1)
coef2 <- coef(modelo2)

# Calcular las predicciones
pred_1 <- predict(modelo1)
pred_2 <- predict(modelo2)
pred_3 <- predict(modelo3)
# Calcular Bias y RMSE para cada modelo
bias_1 <- mean(pred_1 - df_merge_WUSTLModEstacion$Model)
bias_2 <- mean(pred_2 - df_merge_WUSTLModEstacion$WUSTL)

rmse_1 <- sqrt(mean((pred_1 - df_merge_WUSTLModEstacion$Model)^2))
rmse_2 <- sqrt(mean((pred_2 - df_merge_WUSTLModEstacion$WUSTL)^2))
rmse_3 <- sqrt(mean((pred_3 - df_merge_WUSTLModEstacion$WUSTL)^2))

rmse_1
rmse_2
rmse_3






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






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
estacion <- "BA"

#year<-2022
# Archivo donde estan los nombres de las estciones y las coordenadas
data_estacciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))
data_estacciones <- data_estacciones[data_estacciones$Considerado=="SI",]
#data_estacciones <- data_estacciones[data_estacciones$tipo=="referencia",]
# el csv tiene una columna que se llama long y otra lat
monitors<- data_estacciones[, c("long", "lat")]

# directorio donde estan los archivos .nc
#dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/Comparativas_resultados/PM_WEI/",year,"/",sep="")
modeloGlobal <- "WEI"# WUSTL
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/ModelosGlobales/",modeloGlobal,"/",sep="")

setwd(dir)
# Lista de los archivos en formato .nc
id <- list.files(path = dir,
                 pattern = "*.nc",
                 full.names = FALSE)

# Deberia tener 12, son mensuales los datos
print(length(id))

df_rbind <- data.frame()
df_rbind_bi <- data.frame()
i<-1
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
  extracted_values <- raster::extract(mp, monitors)#, method="bilinear") # bilinear interpolates from the four nearest cell
  extracted_values_bi <- raster::extract(mp, monitors, method="bilinear")
  ## Unir datos de concentraciones con los datos de extraidos
  puntos_con_valores <- data_estacciones %>%
    mutate(extracted_values = extracted_values)
  
  puntos_con_valores_bi <- data_estacciones %>%
    mutate(extracted_values_bi = extracted_values_bi)
  # info extra para agregar al dataframe
  ##--- WUSTL
  # puntos_con_valores$monthYear <- substr(file, 26,31)
  # puntos_con_valores$producto <- substr(file, 1,6)
  
  ##--- WEI
  puntos_con_valores$monthYear <- substr(file, 16,21)
  puntos_con_valores$producto <- substr(file, 1,10)
  puntos_con_valores_bi$monthYear <- substr(file, 16,21)
  puntos_con_valores_bi$producto <- substr(file, 1,10)
  
  df_rbind <- rbind(df_rbind,puntos_con_valores)
  df_rbind_bi <- rbind(df_rbind_bi,puntos_con_valores_bi)
}
View(df_rbind)

#Guardamos csv
save_dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/Comparativas_resultados/PM_",modeloGlobal,"/",sep="")

write.csv(df_rbind,paste(save_dir,"data_PM-",modeloGlobal,"_",estacion,".csv",sep=""))
setwd(save_dir)

write.csv(df_rbind_bi,paste(save_dir,"data_PM-",modeloGlobal,"_",estacion,"_BI.csv",sep=""))
setwd(save_dir)

############################################################################
############################################################################
## Unimos con datos de monitoreo de los sitios
# Se hace una validacion con los sitios de moniteoro
estacion <- "BA"
modeloGlobal <- "WEI"# WUSTL
# Datos extraido de los mapas propios generados. Estan diarios hacer media mensual
df_modelado <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/Comparativas_resultados/PM_modelado/data_PM-Modelado-TOT_",estacion,".csv",sep=""))
df_modelado$date <- as.Date(df_modelado$date,format = "%Y-%m-%d")
df_modeloGlobal <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/Comparativas_resultados/PM_",modeloGlobal,"/data_PM-",modeloGlobal,"_",estacion,".csv",sep=""))

# Promedios mensuales por estacion
df_estaciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/06_estaciones/",estacion,"_estaciones.csv",sep=""))
df_estaciones$date <- as.Date(df_estaciones$date,format = "%d/%m/%Y")#"%Y-%M-%d")#
# solo para CH
#df_estaciones$mean <- df_estaciones$Registros.completos
# Agrupamos datos por mes
df_estaciones_mes <- df_estaciones %>%
  mutate(mes = floor_date(date, "month")) %>%  
  group_by(estacion, mes) %>%
  summarise(media = mean(mean, na.rm = TRUE)) %>%  # reemplazá "valor" por tu variable de interés
  ungroup()

df_modelado_mes <- df_modelado %>%
  mutate(mes = floor_date(date, "month")) %>%  
  group_by(estacion, mes) %>%
  summarise(media = mean(valor_raster, na.rm = TRUE)) %>%  # reemplazá "valor" por tu variable de interés
  ungroup()



#Setear formato de dataframe mensual de las mediciones
#todos empiezan con dia 01 
df_estaciones_mes$date <- as.Date(df_estaciones_mes$mes,format = "%Y-%m-%d")
#df_estaciones_mes<- df_estaciones_mes[complete.cases(df_estaciones_mes$mes),]
###
#Setear formato de dataframe mensual del modelo global
df_modeloGlobal$date <- as.Date(paste0(df_modeloGlobal$monthYear, "01"), format = "%Y%m%d")
# df_WEI$date <- as.Date(df_WEI$date,format = "%Y-%m-%d")
df_modelado_mes$date <- as.Date(df_modelado_mes$mes,format = "%Y-%m-%d")

####
#Merge mediciones vs mi modelo
df_merge_modelado_Estacion <- merge(df_modelado_mes, df_estaciones_mes, by = c("date", "estacion"), all.x = TRUE)
df_merge_modelado_Estacion$date<-as.Date(df_merge_modelado_Estacion$date,format = "%Y-%m-%d")

####
#Merge datos anteriores vs el modelo global por dia y estaciones
df_merge_modeloGlobalModEstacion <- merge(df_merge_modelado_Estacion, df_modeloGlobal, by = c("date", "estacion"), all.x = TRUE)
df_merge_modeloGlobalModEstacion <- df_merge_modeloGlobalModEstacion[complete.cases(df_merge_modeloGlobalModEstacion$extracted_values),]
df_merge_modeloGlobalModEstacion_comp <- df_merge_modeloGlobalModEstacion[complete.cases(df_merge_modeloGlobalModEstacion$media.x),]
df_merge_modeloGlobalModEstacion_comp <- df_merge_modeloGlobalModEstacion_comp[complete.cases(df_merge_modeloGlobalModEstacion_comp$media.y),]
# Mejoramos el dataframe
df_merge_modeloGlobalModEstacion <- data.frame(date = df_merge_modeloGlobalModEstacion_comp$date,
                                     estacion = df_merge_modeloGlobalModEstacion_comp$estacion,
                                     mediciones = df_merge_modeloGlobalModEstacion_comp$media.y,
                                     Model = df_merge_modeloGlobalModEstacion_comp$media.x,
                                     modeloGlobal = df_merge_modeloGlobalModEstacion_comp$extracted_values)
#Vemos si hay datos faltantes
df_merge_modeloGlobalModEstacion <- df_merge_modeloGlobalModEstacion [complete.cases(df_merge_modeloGlobalModEstacion),]

# Asegurar que el data frame sea tibble
df_merge_modeloGlobalModEstacion <- as_tibble(df_merge_modeloGlobalModEstacion)

# Regresiones lineales: Model ~ SINCA y modeloGlobal ~ Mediciones
modelo1 <- lm(Model ~ mediciones, data = df_merge_modeloGlobalModEstacion)
modelo2 <- lm(modeloGlobal ~ mediciones, data = df_merge_modeloGlobalModEstacion)
modelo3 <- lm(Model~ modeloGlobal, data = df_merge_modeloGlobalModEstacion)
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
bias_1 <- mean(pred_1 - df_merge_modeloGlobalModEstacion$Model)
bias_2 <- mean(pred_2 - df_merge_modeloGlobalModEstacion$modeloGlobal)

rmse_1 <- sqrt(mean((pred_1 - df_merge_modeloGlobalModEstacion$Model)^2))
rmse_2 <- sqrt(mean((pred_2 - df_merge_modeloGlobalModEstacion$modeloGlobal)^2))
rmse_3 <- sqrt(mean((pred_3 - df_merge_modeloGlobalModEstacion$modeloGlobal)^2))

rmse_1
rmse_2
rmse_3






############################################################################
############################################################################
#### Validacion con el raster completo pixel a pixel
modeloGlobal <- "WEI"# WUSTL
#Path con todos los archivos del modelo global
dir_modeloGlobal <- paste("D:/Josefina/Proyectos/ProyectoChile/ModelosGlobales/",modeloGlobal,"/",sep="")
setwd(dir_modeloGlobal)

# Lista de archivos .nc
id_modeloGlobal <- list.files(path = dir_modeloGlobal,
                       pattern = "*.nc",
                       full.names = FALSE)

cat("Cantidad de archivos modeloGlobal:", length(id_modeloGlobal), "\n")

# Extraer fechas del nombre (formato YYYYMM)
# fechas_modeloGlobal <- substr(id_modeloGlobal, 26, 31) WUSTL
fechas_modeloGlobal <- substr(id_modeloGlobal, 16, 21) #WEI

date_modeloGlobal <- as.Date(paste0(fechas_modeloGlobal, "01"), format = "%Y%m%d")

# Crear dataframe con nombre y fecha
df_modeloGlobal <- data.frame(archivo_modeloGlobal = id_modeloGlobal, fecha_modeloGlobal = date_modeloGlobal)

# ---------------------------------------------------------------
#Modelo propio por centro urbano
estacion <- "BA"
modelo <- "01-ET-CV-M1-170625-BA"
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

df_match <- inner_join(df_modelo, df_modeloGlobal,
                       by = c("fecha_modelo" = "fecha_modeloGlobal"))

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
  raster_modeloGlobal <- raster(paste0(dir_modeloGlobal, df_match$archivo_modeloGlobal[i]))
  
  # Recortar modeloGlobal al extent del modelo
  raster_modeloGlobal_recortado <- crop(raster_modeloGlobal, extent(raster_modelo))
  
  # Ajustar resoluci?n (bilinear para valores continuos)
  raster_modeloGlobal_resample <- resample(raster_modeloGlobal_recortado, raster_modelo, method = "bilinear")
  
  # Convertir ambos en data frame
  df <- as.data.frame(stack(raster_modelo, raster_modeloGlobal_resample), xy = FALSE, na.rm = TRUE)
  colnames(df) <- c("modelo", "modeloGlobal")
  
  # Agregar columnas con las fechas de ambos modelos
  df$fecha_modelo <- df_match$fecha_modelo[i]
  # df$fecha_modeloGlobal <- df_match$fecha_modeloGlobal[i]
  
  # Agregar al dataset total
  df_rbind <- rbind(df_rbind, df)
  
}


# ---------------------------------------------------------------
# Calcular m?tricas para el mes
fit <- lm(modelo ~ modeloGlobal, data = df_rbind)
r2 <- summary(fit)$r.squared
rmse <- sqrt(mean((df_rbind$modelo - df_rbind$modeloGlobal)^2))
cor(x=df_rbind$modelo, y=df_rbind$modeloGlobal)
r2
rmse

# ---------------------------------------------------------------
#Guardar csv
dir_resultados <- "D:/Josefina/Proyectos/ProyectoChile/ModelosGlobales/resultados/"
write.csv(df_rbind, paste(dir_resultados,"comparativa_pixel_",estacion,"_",modeloGlobal, ".csv",sep=""))



###############################################################################
###############################################################################
#Comparativa entre los 2 modelos pixel a pixel
# Periodo para ambos 2017-2022 mensual

## Cofiguracion de los dos modelos
base_dir <- "D:/Josefina/Proyectos/ProyectoChile/ModelosGlobales/"
modelos_globales <- c("WEI", "WUSTL")
estacion <- "BA"
modelo_local <- "01-XGB-CV-M1-200525-SP"
modelo_local <- "01-ET-CV-M1-260525-MD"
modelo_local <- "01-XGB-CV-M1-290525-MX"
modelo_local <- "01-ET-CV-M1-170625-BA"
# Directorio del modelo local
dir_modelo_local <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion,
                           "/modelos/salidas/SalidasMensuales/", modelo_local, "/")

# Extension de referencia: la del modelo propio
raster_ref <- raster(list.files(dir_modelo_local, pattern = "\\.tif$", full.names = TRUE)[1])
plot(raster_ref)

# Funcion para cargar y preparar cada modelo global

preparar_modelo_global <- function(nombre_modelo, pos_ini, pos_fin) {
  dir_modelo <- paste0(base_dir, nombre_modelo, "/")
  archivos <- list.files(path = dir_modelo, pattern = "\\.nc$", full.names = FALSE)
  
  fechas <- substr(archivos, pos_ini, pos_fin)
  fechas <- as.Date(paste0(fechas, "01"), format = "%Y%m%d")
  
  data.frame(archivo = archivos,
             fecha = fechas,
             modelo = nombre_modelo,
             dir = dir_modelo)
}

# Cargar ambos modelos
df_WEI   <- preparar_modelo_global("WEI", 16, 21)
df_WUSTL <- preparar_modelo_global("WUSTL", 26, 31)


# Setear modelo local como referencia
archivos_local <- list.files(path = dir_modelo_local, pattern = "\\.tif$", full.names = FALSE)
fechas_local <- substr(archivos_local, 9, 15)
fechas_local <- as.Date(paste0("01-", fechas_local), format = "%d-%m-%Y")
df_local <- data.frame(archivo = archivos_local, fecha = fechas_local)

# Emparejar fechas de los tres modelos (2017-2022)

df_WEI_join <- inner_join(df_local, df_WEI, by = "fecha")
df_WUSTL_join <- inner_join(df_local, df_WUSTL, by = "fecha")

fechas_comunes <- intersect(df_WEI_join$fecha, df_WUSTL_join$fecha)
cat("Fechas coincidentes:", length(fechas_comunes), "\n")


# Procesar y comparar pixel a pixel
# ---------------------------------------------------------------
df_list <- list()  # Usar lista para acumular resultados y luego hacer rbind

for (fecha_actual in fechas_comunes) {
  fecha_actual <- as.Date(fecha_actual)  # Asegurarse que sea Date
  cat("Procesando mes:", format(fecha_actual, "%Y-%m"), "\n")
  
  # Rasters locales y globales
  r_local <- raster(paste0(dir_modelo_local, df_local$archivo[df_local$fecha == fecha_actual]))
  r_WEI <- raster(paste0(df_WEI$dir[1], df_WEI$archivo[df_WEI$fecha == fecha_actual]))
  r_WUSTL <- raster(paste0(df_WUSTL$dir[1], df_WUSTL$archivo[df_WUSTL$fecha == fecha_actual]))
  
  # Recortar al dominio del modelo local
  r_WEI_crop <- crop(r_WEI, extent(r_local))
  r_WUSTL_crop <- crop(r_WUSTL, extent(r_local))
  
  # Ajustar resolucion
  r_WEI_res <- resample(r_WEI_crop, r_local, method = "bilinear")
  r_WUSTL_res <- resample(r_WUSTL_crop, r_local, method = "bilinear")
  
  # Stack y pasar a dataframe
  df_temp <- as.data.frame(stack(r_local, r_WEI_res, r_WUSTL_res), xy = FALSE, na.rm = TRUE)
  colnames(df_temp) <- c("ModeloLocal", "WEI", "WUSTL")
  df_temp$fecha <- fecha_actual
  
  # Guardar en la lista
  df_list[[as.character(fecha_actual)]] <- df_temp
}

# Unir todos los dataframes
df_rbind <- do.call(rbind, df_list)
df_rbind2<- df_rbind[complete.cases(df_rbind),]

# Guardar resultados
write.csv(df_rbind, "Comparacion_WEI_WUSTL_pixel_a_pixel.csv", row.names = FALSE)

# Metricas

fit <- lm(WEI ~ WUSTL, data = df_rbind)
r2 <- summary(fit)$r.squared
rmse <- sqrt(mean((df_rbind$WEI - df_rbind$WUSTL)^2))
#cor(x=df_rbind$WEI, y=df_rbind$WEI)
r2
rmse




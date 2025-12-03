#######################################################################
## OBJETIVO: Generacion de mapas mensuales a partir de los datos diarios
##
#######################################################################
# Revisar como estan guardados los mapas diarios

# Definir el directorio donde estan los archivos raster
rm(list = ls())
month <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- "2015"
i<-1
tipoModelo<- "XGB"
modelo <- "01-XGB-CV-M1-190625-CH"
estacion <- "CH"
for (i in 1:length(month)){
  print(i)
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasDiarias/",modelo,"/",year,"/",month[i],sep="")
  setwd(dir_salida)
  
  # Lista de archivos raster 
  lista_raster <- list.files(pattern = "*.tif")
  lista_raster_recorte <- lista_raster
  len <- length(lista_raster_recorte)
  print(c(month[i],len))
  # Cargar los rasters en un RasterStack
  raster_stack <- stack(lista_raster_recorte)
  
  # Calcular el promedio mensual y desviacion estandar
  promedio_mensual <- calc(raster_stack, fun = mean, na.rm = TRUE)
  #sd_mensual <- calc(raster_stack, fun = sd, na.rm = TRUE)
  # Calcular el coeficiente de variacion
  #coef_Var <- (sd_mensual / promedio_mensual) * 100
  #plot(promedio_mensual)
  modelo_2 <- substr(lista_raster[1],14,34)
  # Guardar el resultado en un nuevo archivo raster
  dir_salida_tiff <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasMensuales/",modelo,sep="")
  nombre <- paste(dir_salida_tiff,"/base/",tipoModelo,"_PM2.5_M_",month[i],"-",year,"_",estacion,"_V1.1.tif",sep="")
  # writeRaster(promedio_mensual, filename = paste(dir_salida_tiff,"mensual_",month[i],"-",year,"-",modelo_2,".tif",sep=""), format = "GTiff", overwrite = TRUE)
  writeRaster(promedio_mensual, filename = nombre, format = "GTiff", overwrite = TRUE)
}
XGB_PM2.5_M_01-2024_ST_V1.1
"XGB_PM2.5_M_12-2020_SP_V1.1.tif"


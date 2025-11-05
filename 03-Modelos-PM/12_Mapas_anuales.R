#######################################################################
## OBJETIVO: Generacion de mapas anuales a partir de los datos diarios
##
#######################################################################
# Revisar como estan guardados los mapas diarios

rm(list = setdiff(ls(), "df_rbind"))
# Definir el directorio donde estan los datos
year<-2024
#year <- c(2015, 2016,2017,2018,2019,2020,2021,2022,2023)
i<-1
modelo <- "01-RF-CV-M1-170625-CH"
estacion <- "CH"
# Si queremos los datos de todos los años, recorremos todas las carpetas
# Sino no es necesario
for (i in 1:length(year)){
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasDiarias/",modelo,"/",year,"/",sep="")
  
  setwd(dir_salida)
  
  # Lista de archivos raster para el año de interes
  lista_raster <- list.files(pattern = "*.tif") 
  lista_raster_recorte <- lista_raster
  # Cuantos datos hay en la carpeta, revisar!
  len <- length(lista_raster_recorte)
  print(c(year[i],len))
  # Cargar los rasters en un RasterStack
  raster_stack <- stack(lista_raster_recorte)
  
  # Calcular el promedio anual
  promedio_anual <- calc(raster_stack, fun = mean, na.rm = TRUE)
  #Otras metricas
  #sd_anual <- calc(raster_stack, fun = sd, na.rm = TRUE)
  # Calcular el coeficiente de variacion
  #coef_Var <- (sd_anual / promedio_anual) * 100
  
  #Nombre
  modelo2 <- substr(lista_raster[1],15,35)
  # Guardar el resultado en un nuevo archivo raster
  # dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasAnuales/Salida_03-XGB_cv_M1-041024/"
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasAnuales/",modelo,"/",sep="")
  #Guardararchivo tiff raster
  writeRaster(promedio_anual, filename = paste(dir_salida,"Promedio_anual_",year[i],"-",modelo,".tif",sep=""), format = "GTiff", overwrite = TRUE)
}



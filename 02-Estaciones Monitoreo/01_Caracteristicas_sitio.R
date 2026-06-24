# Objetivo ----
# Explorar las Caracteristicas de los sitios de estudio (SP, ST, BA, MD, MX)
estacion <- "SP"
numRaster <- "01"

# Tamaño del Dominio y ubicacion ----
grilla <- raster(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/rasterTemplate/",numRaster,"_raster_template.tif",sep=""))

# Numero de estaciones consideradas ----
estaciones_test<- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_1/M1_test_",estacion,".csv",sep=""))
estaciones_train<- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_1/M1_train_",estacion,".csv",sep=""))

unique(estaciones_test$estacion)
length(unique(estaciones_test$estacion))

unique(estaciones_train$estacion)
length(unique(estaciones_train$estacion))

dataset_merge_comp <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/proceed/merge_tot/", estacion, "merge_comp.csv", sep = ""))
dataset_merge_comp$date <- as.Date(dataset_merge_comp$date)
fechas_por_estacion <- dataset_merge_comp %>%
  group_by(estacion) %>%
  summarise(
    fecha_inicial = min(date, na.rm = TRUE),
    fecha_final = max(date, na.rm = TRUE)
  )

fechas_por_estacion
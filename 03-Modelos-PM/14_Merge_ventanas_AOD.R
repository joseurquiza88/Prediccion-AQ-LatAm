

estacion <- "CH"

data_test <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_1/M1_test_",estacion,".csv",sep=""))
data_train <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_1/M1_train_",estacion,".csv",sep=""))
# Formato fecha
data_test$date<- as.POSIXct(as.character(data_test$date), format ="%Y-%m-%d")# "%d/%m/%Y")#
data_train$date<- as.POSIXct(as.character(data_train$date), format ="%Y-%m-%d")# "%d/%m/%Y")#

data_3km <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/00_MAIAC/MAIAC-3KM_",estacion,".csv",sep=""))
data_5km <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/00_MAIAC/MAIAC-5KM_",estacion,".csv",sep=""))

# Formato fecha
data_3km$date <- as.POSIXct(as.character(data_3km$date), format = "%Y%j")
data_5km$date <- as.POSIXct(as.character(data_5km$date), format = "%Y%j")

#Merge
merged_data_3km_test <- merge(data_test, data_3km, by = c("date", "ID"), all.x = TRUE)
merged_data_3km_test <- merged_data_3km_test[complete.cases(merged_data_3km_test$aod_550),]

# Renombrar columnas nuevas (solo si sabés sus nombres)
merged_data_3km_test <- merged_data_3km_test %>%
  rename(
    aod_550_3km = aod_550,
    aod_470_3km = aod_470
  )


merged_data_5km_test <- merge(merged_data_3km_test, data_5km, by = c("date", "ID"), all.x = TRUE)
merged_data_5km_test <- merged_data_5km_test[complete.cases(merged_data_5km_test$aod_550),]

# Renombrar columnas nuevas (solo si sabés sus nombres)
merged_data_5km_test <- merged_data_5km_test %>%
  rename(
    aod_550_5km = aod_550,
    aod_470_5km = aod_470
  )

write.csv(merged_data_5km_test, paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/modelo_ventanas/M1_test_",estacion,".csv",sep=""))

#############################
############################
#Merge
merged_data_3km_train <- merge(data_train, data_3km, by = c("date", "ID"), all.x = TRUE)
merged_data_3km_train <- merged_data_3km_train[complete.cases(merged_data_3km_train$aod_550),]

# Renombrar columnas nuevas (solo si sabés sus nombres)
merged_data_3km_train <- merged_data_3km_train %>%
  rename(
    aod_550_3km = aod_550,
    aod_470_3km = aod_470
  )


merged_data_5km_train <- merge(merged_data_3km_train, data_5km, by = c("date", "ID"), all.x = TRUE)
merged_data_5km_train <- merged_data_5km_train[complete.cases(merged_data_5km_train$aod_550),]

# Renombrar columnas nuevas (solo si sabés sus nombres)
merged_data_5km_train <- merged_data_5km_train %>%
  rename(
    aod_550_5km = aod_550,
    aod_470_5km = aod_470
  )

write.csv(merged_data_5km_train, paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/modelo_ventanas/M1_train_",estacion,".csv",sep=""))

#######################################################################
## OBJETIVO: Hacer pruebas de los modelos de ML para seleccionar
# los hiperparametros a traves del grid Search
## suelen tardar bastante
#######################################################################

#########################################################################################
### ----- Modelo SVR con búsqueda de hiperparámetros -----
#########################################################################################

library(e1071)

# Parametros iniciales
estacion <- "BA"
modelo <- "1"

# Directorios
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

# Lectura de datos
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Definir un grid de parámetros a explorar
set.seed(123)

tune_result <- tune.svm(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek,
  data = train_data,
  type = "eps-regression",
  kernel = "radial",
  # combinaciones de parametros a probar
  cost = c(0.1, 1, 10, 100),
  epsilon = c(0.01, 0.1, 0.2, 0.5),
  gamma = c(0.001, 0.01, 0.1, 1)
)

# Resultado del tuning
summary(tune_result)

# Mejor combinación encontrada
best_params <- tune_result$best.parameters
print(best_params)

######
# Entrenar el modelo final con los mejores hiperparametros

svr_model <- tune_result$best.model

####
# Evaluacion del modelo

resultados_SVR <- evaluar_modelo(
  modelo = svr_model,
  datos_test = test_data,
  variable_real = "PM25",
  tipoModelo = "SVR",
  y_test = NA
)

print(resultados_SVR)


### Guardar el modelo
setwd(paste0("D:/Josefina/Proyectos/Tesis/", estacion, "/modelos/"))
save(svr_model, file = paste0("01-SVR-M", modelo, "-tuned-", estacion, ".RData"))



###############################################################################
###############################################################################
### ----- Modelo ET con búsqueda de hiperparametros -----
##############################################################################
library(caret)
library(ranger)

# Parametros iniciales
estacion <- "BA"
modelo <- "1"

# Directorios
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

# Lectura de datos
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

### Control con validacion cruzada

control <- trainControl(
  method = "cv",        # validacion cruzada
  number = ,           # 10-fold CV, si tarda probar con menos 
  search = "grid",
  verboseIter = TRUE
)

### Definir grid 

grid_ET <- expand.grid(
  mtry = c(3, 5, 7, 9),               # numero de variables candidatas por split
  splitrule = "extratrees",           # fuerza el modo Extra Trees
  min.node.size = c(1, 3, 5, 10)      # tamaño mínimo de nodo
)

###
# Entrenamiento con busqueda en la grilla

modelo_ranger <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + SO2SMASS_dia + 
    SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean + d2m_mean + 
    v10_mean + u10_mean + tp_mean + t2m_mean + DEM + dayWeek,
  data = train_data,
  method = "ranger",
  trControl = control,
  tuneGrid = grid_ET,
  importance = 'impurity',
  metric = "RMSE"
)

##
# Mejores hiperparametros

print(modelo_ranger)
print(modelo_ranger$bestTune)

# Evaluacion final con el set de testeo
resultados_ET <- evaluar_modelo(
  modelo = modelo_ranger,
  datos_test = test_data,
  variable_real = "PM25",
  tipoModelo = "ET",
  y_test = NA
)

print(resultados_ET)

# Guardar modelo
setwd(paste0("D:/Josefina/Proyectos/Tesis/", estacion, "/modelos/"))
save(modelo_ranger, file = paste0("01-ET-M", modelo, "-tuned-", estacion, ".RData"))


#########################################################################################
### ----- Modelo Random Forest con busqueda de hiperparametros -----
#########################################################################################

library(caret)
estacion <- "CH"
modelo <- "1"

# Directorio de trabajo
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/", sep = "")
setwd(dir)

# Cargar datasets
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Control entrenamiento con cv cruzada 

control <- trainControl(
  method = "cv",      # validacion cruzada
  number = 5,         # 5-fold CV
  search = "grid",
  verboseIter = TRUE
)

#####
#Definicion de hiperparamentros

grid_RF <- expand.grid(
  mtry = c(3, 5, 7, 9)  # numero de variables candidatas por split
)


### Entrenamiento con grid search 

set.seed(123)

modelo_RF <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + t2m_mean +
    SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean +
    d2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek,
  data = train_data,
  method = "rf",
  trControl = control,
  tuneGrid = grid_RF,
  importance = TRUE,
  metric = "RMSE"
)

# Mejores hiperametros
print(modelo_RF)
print(modelo_RF$bestTune)

# Evaluacion con el set de testeo
resultados_RF <- evaluar_modelo(
  modelo = modelo_RF,
  datos_test = test_data,
  variable_real = "PM25",
  tipoModelo = "RF",
  y_test = NA
)

print(resultados_RF)

# Guardar el modelo
setwd(paste0("D:/Josefina/Proyectos/Tesis/", estacion, "/modelos/"))
save(modelo_RF, file = paste0("01-RF-M", modelo, "-tuned-", estacion, ".RData"))


#########################################################################################
### ----- Modelo XGBoost con búsqueda de hiperparámetros -----
#########################################################################################

library(caret)
library(xgboost)

estacion <- "BA"
modelo <- "1"

# Directorio de trabajo
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

# Cargar datasets
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Variables predictoras
predictors <- c("AOD_055", "ndvi", "BCSMASS_dia","DUSMASS_dia", "sp_mean",
                "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                "d2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM", "dayWeek")

# Control de entrenamiento de cv
control <- trainControl(
  method = "cv",   # validacion cruzada
  number = 5,      # 5-fold CV
  search = "grid",
  verboseIter = TRUE
)

# Definicion de hiperparamentros

grid_XGB <- expand.grid(
  nrounds = c(500, 1000, 1500),      # numero de iteraciones
  max_depth = c(3, 6, 9),            # profundidad maxima de los arboles
  eta = c(0.01, 0.1, 0.3),           # learning rate
  gamma = c(0, 1, 5),                 # regularizacion
  colsample_bytree = c(0.6, 0.8, 1), # proporción de variables por arbol
  min_child_weight = c(1, 3, 5),
  subsample = c(0.6, 0.8, 1)
)

# Entrenamiento con grid search

set.seed(123)
modelo_XGB <- train(
  x = train_data[, predictors],
  y = train_data$PM25,
  method = "xgbTree",
  trControl = control,
  tuneGrid = grid_XGB,
  metric = "RMSE"
)

# Mejor combinacion de hiperparametros

print(modelo_XGB)
print(modelo_XGB$bestTune)

# Evaluacion con el dataset
resultados_XGB <- evaluar_modelo(
  modelo = modelo_XGB,
  datos_test = test_data,
  variable_real = "PM25",
  tipoModelo = "XGB",
  y_test = test_data$PM25
)

print(resultados_XGB)


# Guardar modelo 
setwd(paste0("D:/Josefina/Proyectos/Tesis/", estacion, "/modelos/"))
save(modelo_XGB, file = paste0("01-XGB-M", modelo, "-tuned-", estacion, ".RData"))

#######################################################################
## OBJETIVO: Contruccion de modelos Predictivos de PM2.5 con CV aleatorio
##
#######################################################################

#funcion para evaluar modelos
evaluar_modelo <- function(modelo, datos_test, variable_real = "PM25",tipoModelo,y_test) {
  predicciones <- predict(modelo, newdata = datos_test)
  
  if(tipoModelo=="XGB"){
    valores_reales <- y_test
  }
  else{
    valores_reales <- datos_test[[variable_real]]
  }
  # Extraer los valores reales de la variable objetivo
  df <- data.frame(predicciones=predicciones, valores_reales=valores_reales)
  df <- df[df$predicciones>0,]
  # Calcular metricas
  r2 <- cor(df$predicciones, df$valores_reales)^2
  pearson <- cor(df$valores_reales, df$predicciones, method = "pearson")
  rmse <- sqrt(mean((df$predicciones - df$valores_reales)^2))
  bias <- mean(df$predicciones - df$valores_reales)
  
  # Resultados
  resultados <- data.frame(
    R2 = round(r2, 5),
    Pearson = round(pearson, 3),
    RMSE = round(rmse, 3),
    Bias = round(bias, 3),
    Min_Pred = round(min(df$predicciones), 3),
    Max_Pred = round(max(df$predicciones), 3)
  )
  
  return(resultados)
}

##############################################################################
##############################################################################
##############################################################################
### ----- Modelo predictivo SVR   -----
estacion <-"BA"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
# dataset
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

# Definimos el control de entrenamiento con CV = 10
ctrl <- trainControl(method = "cv", number = 10,
                     savePredictions = "final",
                     verboseIter = TRUE)

# ntrenamiento del modelo SVR con validacion cruzada en train_data
# Seteamos semilla
set.seed(123)
#Modelo predictivo
modelo_cv_svr <- train(PM25 ~  ndvi + BCSMASS_dia + DUSMASS_dia + #
                         SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
                         d2m_mean  +t2m_mean +
                         v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
                       data = train_data,
                       method = "svmRadial",
                       trControl = ctrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 5) 
10:12
#Desempe?o
resultados_SVR_cv <- evaluar_modelo(modelo=modelo_cv_svr, datos_test=test_data, variable_real = "PM25",tipoModelo="SVR",y_test=NA)
print(resultados_SVR_cv)
# Guardar modelo
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_cv_svr, file=paste("01-SVR-CV-M",modelo,"-150626-sAOD_",estacion,".RData",sep=""))
# lo cargmamos
load("01-SVR-CV-M1-200525-SP.RData")
load("01-SVR-CV-M1-210625-sAODCH.RData")
load("01-SVR-CV-M1-210625-sAOD-BA.RData")
load("01-SVR-CV-M1-260525-MD.RData")
load("01-SVR-CV-M1-290525_MX.RData")
load("01-SVR-CV-M1-150626-sAOD_MD.RData")

# Hiperparametros optimos encontrados por CV
cat("Hiperparmetros optimos:\n")
print(modelo_cv_svr$bestTune)

# Todas las combinaciones de hiperparámetros evaluadas y sus metricas
cat("\nResultados de todas las combinaciones de hiperparámetros:\n")
print(svr_model$results)
print(resultados_SVR_cv$results)
##############################################################################
##############################################################################
##############################################################################
### ----- Modelo predictivo ET   ----
estacion <-"MX"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
#dataset de entrada
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

# Entrenamiento con validacion cruzada de 10 pliegues
modelo_ranger <- train(
  PM25 ~ ndvi + BCSMASS_dia + DUSMASS_dia + #
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean  +t2m_mean +
    v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 10),  
  tuneGrid = data.frame(
    mtry = 5,                   # valor fijo
    splitrule = "extratrees",  # Extra Trees
    min.node.size = 5          # valor fijo
  ),
  importance = 'impurity'
)

modelo_ET_cv <- modelo_ranger

# Evaluacion del desempe?o
resultados_ET_cv <- evaluar_modelo(modelo=modelo_ET_cv, datos_test=test_data, variable_real = "PM25",tipoModelo="ET",y_test=NA)

print(resultados_ET_cv)

# Guardar modelo
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_ET_cv, file=paste("01-ET-CV-M",modelo,"-150626-sAOD_",estacion,".RData",sep=""))

# Cargar modelo
load("01-ET-CV-M1-200525-SP.RData")
load("01-ET-M1-170625-CH.RData")
load("01-ET-CV-M1-170625-BA.RData")
load("01-ET-CV-M1-260525-MD.RData")
load("01-ET-CV-M1-290525_MX.RData")

# Hiperparametros elegidos
cat("Hiperparametros seleccionados:\n")
print(modelo_ET_cv$bestTune)
print(modelo_ranger$bestTune)
print(modelo_et_spatial$bestTune)
# Resumen del modelo final
cat("\nDetalles del modelo final:\n")
# print(modelo_ET_cv$finalModel)

# Numero de arboles realmente entrenados
cat("\Numero de arboles:\n")
print(modelo_ET_cv$finalModel$num.trees)

# Parametros de nodo minimo
cat("\nMin node size:\n")
print(modelo_ET_cv$finalModel$min.node.size)

# Variables candidatas en cada split
cat("\nMtry:\n")
print(modelo_ET_cv$finalModel$mtry)

# Regla de division usada
cat("\nSplit rule:\n")
print(modelo_ET_cv$finalModel$splitrule)




##############################################################################
##############################################################################
##############################################################################
### ----- Modelo predictivo Random Forest   ----
estacion <-"SP"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

train_control <- trainControl(
  method = "cv",          # Metodo de validacion cruzada
  number = 10,            # Numero de pliegues 
  verboseIter = TRUE,     # Mostrar progreso de entrenamiento
  allowParallel = TRUE    # Permitir procesamiento paralelo
)

start_time <- Sys.time()
modelo_RF_cv <- train(  PM25 ~  ndvi + BCSMASS_dia + DUSMASS_dia + #
                          SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
                          d2m_mean  +t2m_mean +
                          v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
                        data = train_data,
                        trControl = train_control,importance = TRUE)
# Setear cuando empieza y termina el modelo, duracion
end_time <- Sys.time()
print(end_time - start_time)
# Evaluar el desempe?o
resultados_RF_cv <- evaluar_modelo(modelo=modelo_RF_cv, datos_test=test_data, variable_real = "PM25",tipoModelo="RF",y_test=NA)


print(resultados_RF_cv)

# Guardar modelo
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_RF_cv, file=paste("01-RF-CV-M",modelo,"-160626-sAOD_",estacion,".RData",sep=""))
# Cagar modelo
load("01-RF-CV-M1-200525-SP.RData")
load("01-RF-CV-M1-170625-CH.RData")
load("01-RF-CV-M1-170625-BA.RData")
load("01-RF-CV-M1-260525-MD.RData")
load("01-RF-CV-M1-290525-MX.RData")
# Imprimir los hiperparametros ajustados por caret
cat("Mejor combinacion de hiperparametros segun validacion cruzada:\n")
print(modelo_RF_cv$bestTune)

# Acceder al modelo final (objeto randomForest) para ver parametros adicionales
cat("\nhiperparametros del modelo final (randomForest):\n")
cat("Numero de arboles (ntree):", modelo_RF_cv$finalModel$ntree, "\n")
cat("Numero de variables consideradas por division (mtry):", modelo_RF_cv$finalModel$mtry, "\n")
cat("Tama?o minimo de nodos terminales (nodesize):", modelo_RF_cv$finalModel$nodesize, "\n")
cat("Numero maximo de nodos (maxnodes):", modelo_RF_cv$finalModel$maxnodes, "\n")



##############################################################################
##############################################################################
##############################################################################
### ----- Modelo predictivo XGB   ----

estacion <-"CH"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
#dataset de entrenamiento y testeo
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

train_data_2022<- train_data[year(train_data$date) != 2022,]
test_data_2022<- test_data[year(test_data$date) != 2022,]

#dataset
#15 vars
X <- train_data_2022[ , c( "AOD_055",
                      "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                      "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                      "d2m_mean", "v10_mean","t2m_mean", #"sp_mean",
                      "u10_mean", "tp_mean","DEM",
                      "dayWeek")]


y <- train_data_2022$PM25

X_test <- test_data_2022[ ,c( "AOD_055",
                              "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                              "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                              "d2m_mean", "v10_mean","t2m_mean", #"sp_mean",
                              "u10_mean", "tp_mean","DEM",
                              "dayWeek")]#

y_test<- test_data_2022$PM25
# Convertir a matrices xgboost
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Especificar los parametros del modelo

params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  # Tarea de regresion
  eval_metric = "rmse",             # Metrica para evaluacion
  eta = 0.3,                       # Tasa de aprendizaje
  max_depth = 6,                   # Profundidad maxima de los arboles
  gamma = 0,                       # Regularizacion L2
  subsample = 0.8,                 # Proporcion de datos para entrenamiento
  colsample_bytree = 1,            # Proporcion de caracteristicas para entrenamiento
  min_child_weight = 1
)

# Realizar validacion cruzada
#Esto es lo que mas tarda!! igual en comparacion con rf tarda mucho menos
# porque??
cv_results  <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 2000,                   # Numero de rondas de boosting
  nfold = 10,                        # Numero de pliegues para la validacion cruzada
  early_stopping_rounds = 20,       # Detener si no mejora
  verbose = TRUE                    # Mostrar progreso
)


# Obtener el numero optimo de rondas
best_nrounds <- cv_results$best_iteration

# Ajustar el modelo con el numero optimo de rondas
xgb_cv_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds
)


## convertir a matrix
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
resultados_XGB <- evaluar_modelo(modelo=xgb_cv_model, datos_test=dtest, variable_real = "PM25",tipoModelo="XGB",y_test=y_test)
print(resultados_XGB)
# Guardar modelo
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(xgb_cv_model, file=paste("01-XGB-CV-M",modelo,"-190625_",estacion,"_2022.RData",sep=""))

# Crgar modelo
load("01-XGB-CV-M1-200525-SP.RData")
load("01-XGB-CV-M1-190625-CH.RData")
load("01-XGB-CV-M1-190625-BA.RData")
load("01-XGB-CV-M1-260525-MD.RData")
load("02-XGB-CV-M1-230625-sAOD-MX.RData")
load("01-XGB-CV-M1-190625_CH_2022.RData")

cat("Hiperparametros del modelo XGB:\n")
print(params)

cat("\nNumero optimo de rondas de boosting:\n")
print(best_nrounds)
print(xgb_cv_model$params)

cat("\nNumero de rondas usadas:\n")
print(xgb_cv_model$niter)

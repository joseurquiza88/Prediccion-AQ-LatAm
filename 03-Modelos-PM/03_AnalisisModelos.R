
#######################################################################
## OBJETIVO: Contruir modelos para predecir PM2.5
##
######################################################################## 
# Preparacion de datos: seleccio de variables
estacion <- "MX"
data_com <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp.csv",sep=""))
names(data_com)
#Poner en formato la fecha
data_com$date <- as.POSIXct(as.character(data_com$date), format = "%Y-%m-%d")
# Agregamos numero de dia
data_com$dayWeek <- wday(data_com$date, week_start = 1)
unique(year(data_com$date))
# Nos quedamos solo con los datos 2015-2023
data_com<- data_com[year(data_com$date) != 2024,]
# Verificamos
unique(year(data_com$date))

#Generamos modelo lineal multiple con todas las variables  (17 Vars)
modelo <- lm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + OCSMASS_dia+
               SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
               d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek,
             data = data_com)

vif(modelo)
sort(vif(modelo), decreasing = TRUE)
a<- data.frame(vif(modelo))
a <- 

car::vif(modelo)
summary(modelo)

#########################################################
## Modelos predictivos de ML pero sin CV
#########################################################
# Se contruye una funcion para evaluar todos los modelos
# Entrenamiento ==> set de Entrenamiento
# Testeo ==> set de testeo
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
  # Calcular m?tricas
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


#########################################################################################
#########################################################################################
### ----- Modelo SVR  -----
library(e1071)  # para usar svm
estacion <-"BA"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
# Set de datos para entrenar y validar
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

# Se crea el modelo
svr_model <- svm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                   SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean +sp_mean + #+t2m_mean
                   d2m_mean   +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
                 data = train_data,
                 type = "eps-regression",  # regresion epsilon-SVR
                 kernel = "radial",        # Otra alernativa "linear" o "polynomial"
                 cost = 10,                # parametro de penalizacion (ajustable)
                 epsilon = 0.1)            # margen de tolerancia (ajustable)

# Evaluar el desempeño
resultados_SVR <- evaluar_modelo(modelo=svr_model, datos_test=test_data, variable_real = "PM25",tipoModelo="SVR",y_test=NA)

print(resultados_SVR)

# Guardar el modelo
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(svr_model, file=paste("01-SVR-M",modelo,"-170625",estacion,".RData",sep=""))


#########################################################################################
#########################################################################################
### ----- Modelo ET  -----
library(caret)
library(ranger)
estacion <-"BA"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

# Entrenamiento simple sin busqueda de hiperparametros ni CV
modelo_ranger <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + +t2m_mean 
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean  +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "none"),  # sin validacion cruzada
  tuneGrid = data.frame(
    mtry = 5,                # elegir un valor fijo, por ejemplo 5
    splitrule = "extratrees", # Extra Trees
    min.node.size = 5        
  ),
  importance = 'impurity'
)

# Evaluar el desempeño
resultados_ET <- evaluar_modelo(modelo=modelo_ranger, datos_test=test_data, variable_real = "PM25",tipoModelo="ET",y_test=NA)

print(resultados_ET)

#Guardar modelo
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_ranger, file=paste("01-ET-M",modelo,"-170625",estacion,".RData",sep=""))


#########################################################################################
#########################################################################################
### ----- Modelo Random Forest  -----
estacion <-"CH"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
#Dataset de entrenamiento y testeo
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
control <- trainControl(method = "none")
# Entrenar el modelo una sola vez
modelo_RF <- train(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + +t2m_mean +
                     SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean +# sp_mean +
                     d2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
                   data = train_data,
                   method = "rf",
                   trControl = control,
                   importance = TRUE)

# Evaluar el desempeño
resultados_RF <- evaluar_modelo(modelo=modelo_RF, datos_test=test_data, variable_real = "PM25",tipoModelo="RF",y_test=NA)

print(resultados_RF)
# Guardar modelo
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_RF, file=paste("01-RF-M",modelo,"-170625",estacion,".RData",sep=""))


#########################################################################################
#########################################################################################
### ----- Modelo Extreme Gradient Boosting  -----

library(xgboost)
library(Matrix)
### ----- XGB   -----
estacion <-"BA"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
#Dataset de entrenamiento y testeo
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
#Generamos dataframe 
X <- train_data[ , c( "AOD_055",
                      "ndvi", "BCSMASS_dia","DUSMASS_dia", "sp_mean",#"t2m_mean",
                      "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                       "d2m_mean", "v10_mean",
                        "u10_mean", "tp_mean","DEM",
                      "dayWeek")]#


y <- train_data$PM25

X_test <- test_data[ ,c( "AOD_055",
                         "ndvi", "BCSMASS_dia","DUSMASS_dia", "sp_mean",#"t2m_mean",
                         "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                         "d2m_mean", "v10_mean",
                         "u10_mean", "tp_mean","DEM",
                         "dayWeek")]#
y_test<- test_data$PM25
# Convertir a matrices xgboost
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Especificar los parametros del modelo
params <- list(
  booster = "gbtree", 
  objective = "reg:squarederror",  # Tarea de regresion
  eval_metric = "rmse",             # Metrica para evaluacion
  eta = 0.3,      #0.1chat                  # Tasa de aprendizaje
  max_depth = 6,                    # Profundidad maxima de los arboles
  gamma = 0,                        # Regularizacion L2
  subsample = 0.8,                  # Proporcion de datos para entrenamiento
  colsample_bytree = 1, #0.8            # Proporcion de caracterasticas para entrenamiento
  min_child_weight = 1 
)

# Ajustar el modelo
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 2000#,      #2000    # Numero de rondas de boosting
  #early_stopping_rounds = 10  # Detener el entrenamiento si no mejora
)

#Generacion de  de la matriz
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
resultados_XGB <- evaluar_modelo(modelo=xgb_model, datos_test=dtest, variable_real = "PM25",tipoModelo="XGB",y_test=y_test)

print(resultados_XGB)
# Guardar modelo
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(xgb_model, file=paste("01-XGB-M",modelo,"-170625",estacion,".RData",sep=""))

#########################################################################################
#########################################################################################


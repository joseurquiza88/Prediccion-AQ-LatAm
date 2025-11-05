
#######################################################################
## OBJETIVO: Modelos Predictivos de PM2.5 para ver la importancia del 
# uso/NO uso de AOD
# similar al codido de imporancia de variables!
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


#######################################
### ----- Modelo predictivo SVR   -----
estacion <-"MX"
modelo <- "1"
# datos de modelo
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

#Definimos el control de entrenamiento con CV = 10
ctrl <- trainControl(method = "cv", number = 10,
                     savePredictions = "final",
                     verboseIter = TRUE)
# Es para ver el horario de comienzo y fin del modelo, para ver cuanto tarda
start_time <- Sys.time()
# Entrenamiento del modelo SVR con validacion cruzada en train_data
set.seed(123)
modelo_cv_svr <- train(PM25 ~ AOD_055+ ndvi + BCSMASS_dia + 
                         DUSMASS_dia + SO2SMASS_dia + SO4SMASS_dia + 
                         SSSMASS_dia + blh_mean + sp_mean +
                         d2m_mean + t2m_mean +v10_mean +
                         u10_mean + tp_mean + DEM +
                         dayWeek,
                       data = train_data,
                       method = "svmRadial",
                       trControl = ctrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 5) 

# Horario fin del entrenamiento
end_time <- Sys.time()
# Ver cuanto tardo
print(end_time - start_time)
# Evaluacion del desempe�o
resultados_SVR_cv <- evaluar_modelo(modelo=modelo_cv_svr, datos_test=test_data,
                                    variable_real = "PM25",tipoModelo="SVR",y_test=NA)
print(resultados_SVR_cv)

# Guardar modelo sin AOD
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_cv_svr, file=paste("02-SVR-CV-M",modelo,"-210525-sAOD-",estacion,".RData",sep=""))


##############################################################################
##############################################################################
##############################################################################
### ----- Modelo predictivo ET   -----
estacion <-"SP"
modelo <- "1"
# datos
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

# Entrenamiento con validacion cruzada de 10 pliegues
modelo_ranger <- train( ####AOD_055
  PM25 ~  ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek, 
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 10),  # Validacion cruzada 10-fold
  tuneGrid = data.frame(
    mtry = 5,                   # valor fijo
    splitrule = "extratrees",  # Extra Trees
    min.node.size = 5          # valor fijo
  ),
  importance = 'impurity'
)
# cambio d enombre para mejor entendimiento
modelo_ET_cv <- modelo_ranger
# Desempe�o
resultados_ET_cv <- evaluar_modelo(modelo=modelo_ET_cv, datos_test=test_data,
                                    variable_real = "PM25",tipoModelo="ET",y_test=NA)
print(resultados_ET_cv)

# Guardar modelo
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_ET_cv, file=paste("02-ET-CV-M",modelo,"-210525-sAOD-",estacion,".RData",sep=""))

##############################################################################
##############################################################################
##############################################################################
### ----- Modelo predictivo RF   -----
estacion <-"MX"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

train_control <- trainControl(
  method = "cv",          # Metodo de validacion cruzada
  number = 10,            # Numero de pliegues para la validacion cruzada
  verboseIter = TRUE,     # Mostrar progreso de entrenamiento
  allowParallel = TRUE    # Permitir procesamiento paralelo
)
# Es para ver el horario de comienzo y fin del modelo, para ver cuanto tarda
start_time <- Sys.time()
##########AOD_055 
modelo_RF_cv <- train(PM25  ~ AOD_055+ ndvi + BCSMASS_dia + #DUSMASS_dia +
                         SO2SMASS_dia + SO4SMASS_dia + 
                        SSSMASS_dia + blh_mean + sp_mean +
                        d2m_mean + t2m_mean +v10_mean, #+
                         #tp_mean,# + DEM + 
                        #dayWeek, #u10_mean +
                      data = train_data, method = "rf",
                      trControl = train_control,importance = TRUE)
# Horario fin del entrenamiento.
end_time <- Sys.time()
# Ver cuanto tardo
print(end_time - start_time)
resultados_RF_cv <- evaluar_modelo(modelo=modelo_RF_cv, datos_test=test_data,
                                   variable_real = "PM25",tipoModelo="RF",y_test=NA)
print(resultados_RF_cv)
# Guardar

setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_RF_cv, file=paste("02-RF-CV-M",modelo,"-210525-sAOD-",estacion,".RData",sep=""))


##############################################################################
##############################################################################
##############################################################################
### ----- Modelo predictivo XGB   -----

estacion <-"SP"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))


X <- train_data[ , c( #"AOD_055",
                      "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                      "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                      "sp_mean", "d2m_mean", "v10_mean",
                      "u10_mean", "tp_mean",
                      "dayWeek")]#


y <- train_data$PM25

X_test <- test_data[ ,c(#"AOD_055",
                        "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                        "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                        "sp_mean", "d2m_mean", "v10_mean",
                        "u10_mean", "tp_mean",
                        "dayWeek")]#
y_test<- test_data$PM25
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
# porque?
cv_results  <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 2000,                   # Numero de rondas de boosting
  nfold = 10,                        # Numero de pliegues para la validacion cruzada
  early_stopping_rounds = 20,       # Detener si no mejora
  verbose = TRUE                    # Mostrar progreso
)


# Obtener el numero otimo de rondas
best_nrounds <- cv_results$best_iteration

# Ajustar el modelo con el numero optimo de rondas
xgb_cv_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds
)

#Matriz del tipo xgb
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
#Evaluar los resultados
resultados_XGB <- evaluar_modelo(modelo=xgb_cv_model, datos_test=dtest, 
                                 variable_real = "PM25",tipoModelo="XGB",y_test=y_test)
print(resultados_XGB)
# Guardar
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(xgb_cv_model, file=paste("02-XGB-CV-",modelo,"-210525-sAOD",estacion,".RData",sep=""))



##############################################################################
##########################################################################
#  Colores por sitio 
#SP: "#005a32"
#ST: "#fd8d3c"
#BA: "#99000d"
#MD: "#023858"
#LP: "#ce1256"
#MD: "#6a51a3"

# Plot regresion lineal +metricas
estacion <-"SP"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
dir <- paste("D:/Josefina/Proyectos/tesis/",estacion,"/modelos/",sep="")
setwd(dir)
# para seleccionar el modelo
list.files(pattern = "RF")
#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"

#Cargo los modelos
load("01-RF-CV-M1-200525-SP.RData" )# Sin AOD
modelo_rf_AOD <- modelo_RF_cv 
load("02-RF-CV-M1-210525-sAOD-SP.RData")# Con AOD
modelo_rf_sinAOD <- modelo_RF_cv  # Guardar con un nuevo nombre

# Solo para XGB
predicciones <- predict(xgb_cv_model, newdata = test_data)
X_test <- test_data[ ,c( #"AOD_055",
  "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
  "SO2SMASS_dia",  "SO4SMASS_dia", "SSSMASS_dia","blh_mean", 
  "sp_mean","d2m_mean", "v10_mean", "u10_mean", "tp_mean","dayWeek"
 )]#
y_test<- test_data$PM25
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
predicciones <- predict(xgb_cv_model, newdata = dtest)

#Hacer df con la info de interes
df_combinado <- data.frame(pred=predicciones, real=test_data$PM25)
# Solor para xgb
resultados_XGB <- evaluar_modelo(modelo=xgb_cv_model, datos_test=dtest, 
                                 variable_real = "PM25",tipoModelo="XGB",y_test=y_test)
print(resultados_XGB)

resultados_RF_cv_AOD <- evaluar_modelo(modelo=modelo_rf_AOD, datos_test=test_data,
                                   variable_real = "PM25",tipoModelo="RF",y_test=NA)

resultados_RF_cv_sAOD <- evaluar_modelo(modelo=modelo_rf_sinAOD, datos_test=test_data,
                                   variable_real = "PM25",tipoModelo="RF",y_test=NA)

df_combinado_sAOD <- data.frame(pred=predicciones, real=test_data$PM25)

# metricas de desempe�o para los modelos con/sin AOD
resultados_RF_cv
model_rf <- resultados_RF_cv
R2_model_rf <- resultados_RF_cv$R2
RMSE_model_rf <- resultados_RF_cv$RMSE
Bias_model_rf <- resultados_RF_cv$Bias
n_model_rf <- nrow(test_data)


resultados_RF_cv
model_rf <- resultados_XGB
R2_model_rf <- resultados_XGB$R2
RMSE_model_rf <- resultados_XGB$RMSE
Bias_model_rf <- resultados_XGB$Bias
n_model_rf <- nrow(test_data)


# Crear el plot de dispersin para comparar ambos modelos
plot_regresion_rf <- ggplot(df_combinado, aes(y = real, x= pred)) +
  geom_point(color = "#00441b", size = 1.5, alpha = 0.6) +  # Puntos de datos
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE,size = 0.6, linetype = "dashed") +  # Línea de regresión
  
  labs(
    x = "Observado",
    y = "Prediccion",
  ) +
  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(R2_model_rf, 2)), size = 3, color = "black")+ 
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(RMSE_model_rf, 2)), size = 3, color = "black")+ 
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(Bias_model_rf, 2)), size = 3, color = "black")+ 
  ggplot2::annotate("text",x = 130, y = 30,label = paste("n =", round(n_model_rf, 2)), size = 3, color = "black")+ 
 theme_classic()
plot_regresion_rf

############################################################
#########################################################
# Similar plot anterior
plot_regresion_rf <- ggplot() +
  geom_point(df_combinado, aes(y = real, x= pred),color = "#00441b", size = 1.5, alpha = 0.6) +  # Puntos de datos
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE,size = 0.6, linetype = "dashed") +  # Línea de regresión
  labs(
    x = "Observado",
    y = "Prediccion",

  ) +

  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(R2_model_rf, 2)), size = 3, color = "black")+ 
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(RMSE_model_rf, 2)), size = 3, color = "black")+ 
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(Bias_model_rf, 2)), size = 3, color = "black")+ 
  ggplot2::annotate("text",x = 130, y = 30,label = paste("n =", round(n_model_rf, 2)), size = 3, color = "black")+ 
  theme_classic() #+
plot_regresion_rf




###################################################
#####################################################
#################################################
#Funcion para evaluar modelos
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

estacion <-"MX"
modelo <- "1"
#datos
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
dir <- paste("D:/Josefina/Proyectos/tesis/",estacion,"/modelos/",sep="")
setwd(dir)

# de la carpeta donde estoy ubicada me muestra todos los archivos
# que tengan "RF" en el nombre
list.files(pattern = "RF")
#COLORES
#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",
# Cargar los modelos con/sin AOD de cada sitio

load("01-RF-CV-M1-290525-MX.RData" )
modelo_rf_AOD <- modelo_RF_cv
load("02-RF-CV-M1-240625-sAODMX.RData")
modelo_rf_sinAOD <- modelo_RF_cv  
## predicciones
predicciones_AOD <- predict(modelo_rf_AOD, newdata = test_data)
predicciones_sAOD <- predict(modelo_rf_sinAOD, newdata = test_data)

df_combinado <- data.frame(pred_AOD=predicciones_AOD, pred_sAOD=predicciones_sAOD,real=test_data$PM25)

## Metricas
resultados_RF_cv_AOD <- evaluar_modelo(modelo=modelo_rf_AOD, datos_test=test_data,
                                       variable_real = "PM25",tipoModelo="RF",y_test=NA)

resultados_RF_cv_sAOD <- evaluar_modelo(modelo=modelo_rf_sinAOD, datos_test=test_data,
                                        variable_real = "PM25",tipoModelo="RF",y_test=NA)

#resultados_RF_cv_AOD
R2_model_rf_AOD <- resultados_RF_cv_AOD$R2
RMSE_model_rf_AOD <- resultados_RF_cv_AOD$RMSE
Bias_model_rf_AOD <- resultados_RF_cv_AOD$Bias
n_model_rf_AOD <- nrow(test_data)

resultados_RF_cv_sAOD
R2_model_rf_sAOD <- resultados_RF_cv_sAOD$R2
RMSE_model_rf_sAOD <- resultados_RF_cv_sAOD$RMSE
Bias_model_rf_sAOD <- resultados_RF_cv_sAOD$Bias
n_model_rf_sAOD <- nrow(test_data)


# Crear el plot para comparar las metricas al usar/no usar el AOD
#como variable predictiva
plot_regresion_rf <- ggplot(df_combinado) +
  geom_point(aes(y = real, x= pred_sAOD),color =   "#41b6c4", alpha=0.9,size = 1.5,shape=20, ) +  # Puntos de datos
  geom_point(aes(y = real, x= pred_AOD),color = "#023858" ,   size = 1.5, shape=8) +  # Puntos de datos
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  geom_smooth(aes(y = real, x= pred_sAOD),method = "lm", color = "red", se = FALSE,size = 0.6, linetype = "dashed") +  # Linea de regresio
  geom_smooth(aes(y = real, x= pred_AOD),method = "lm", color = "red", se = FALSE,size = 0.6, linetype = "solid") 
  labs(
    x = "Observado",
    y = "Prediccion",
  ) +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )+
  ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 70,label = paste("sAOD"), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(R2_model_rf_sAOD, 2)), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(RMSE_model_rf_sAOD, 2)), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(Bias_model_rf_sAOD, 2)), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_rf_sAOD, 2)), size = 3, color = "black")+
  ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+
  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(R2_model_rf_AOD, 2)), size = 3, color = "black")+
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(RMSE_model_rf_AOD, 2)), size = 3, color = "black")+
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(Bias_model_rf_AOD, 2)), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_rf_AOD, 2)), size = 3, color = "black")+
  
  theme_classic() #+
plot_regresion_rf


# Objetivo ----
#Contruccion de modelos Predictivos de PM2.5 con CV temporal

# Algunas librerias
library(caret)
library(ranger)
#Funcion para evaluar el desempeño de los modelos
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

### Modelo predictivo SVR  temporal ----
estacion <- "MD"
modelo <- "1"
#Data 
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
train_data$year <- as.numeric(format(as.Date(train_data$date), "%Y"))

#Extraer años
years <- sort(unique(train_data$year))

index_list <- list()
indexOut_list <- list()
# Recorrer todos los años disponibles (2015-2023)
for (i in seq_along(years)) {
  test_year <- years[i]
  train_index <- which(train_data$year != test_year)
  test_index <- which(train_data$year == test_year)
  
  index_list[[i]] <- train_index
  indexOut_list[[i]] <- test_index
}

#Crear control de entrenamiento
train_control_temporal <- trainControl(
  method = "cv",
  number = length(years),
  index = index_list,
  indexOut = indexOut_list,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE
)

#Entrenamiento modelo
set.seed(123)
modelo_cv_svr_temporal <- train(
  PM25 ~ #AOD_055 + 
    ndvi + BCSMASS_dia + DUSMASS_dia + t2m_mean+ 
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean +v10_mean + u10_mean + tp_mean +DEM+   dayWeek,
  data = train_data,
  method = "svmRadial",
  trControl = train_control_temporal,
  preProcess = c("center", "scale"),
  tuneLength = 5
)
print(estacion)
#Guardar modelo
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_cv_svr_temporal, file=paste("01-SVR-CV-Temp_M",modelo,"-160626-sAOD_",estacion,".RData",sep=""))

# Metricas
resultados_svr_cv_Temporal <- evaluar_modelo(modelo=modelo_cv_svr_temporal, datos_test=test_data, variable_real = "PM25",tipoModelo="SVR",y_test=NA)
print(estacion)
print(resultados_svr_cv_Temporal)
# Metricas por año
df_metricas<- data.frame(modelo_cv_svr_temporal[["resample"]])
max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)
# Reescalar el RMSE para hacer el plot  doble eje
df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) /
  (max_rmse - min_rmse)
# Renombrar las muestras por los años
df_metricas$year <- recode(df_metricas$Resample,
                           "Resample1" = "2015",
                           "Resample2" = "2016",
                           "Resample3" = "2017",
                           "Resample4" = "2018",
                           "Resample5" = "2019",
                           "Resample6" = "2020",
                           "Resample7" = "2021",
                           "Resample8" = "2022",
                           "Resample9" = "2023",
                           # Deja las que no cambian fuera o ponelas igual a sí mismas
                           .default = df_metricas$Resample
)


df_metricas$year <- as.numeric(as.character(df_metricas$year))

# Plot de metricas
SVR_temporal<-ggplot(df_metricas, aes(x = year)) +
  geom_line(aes(y = Rsquared, color = "R2"), size = 1.2) +
  geom_point(aes(y = Rsquared, color = "R2"), size = 1.8) +
  geom_line(aes(y = rmse_escalado, color = "RMSE"), size = 1.2)+#, linetype = "dashed") +
  geom_point(aes(y = rmse_escalado, color = "RMSE"), size = 1.8)+#, shape = 1) +
  scale_y_continuous(
    name = "R2",
    sec.axis = sec_axis(~ . * (max_rmse - min_rmse) + min_rmse, name = "RMSE")
  ) +
  scale_x_continuous(breaks = 2015:2023) + 
  scale_color_manual(values = c("R" = "#2c7fb8",  "RMSE" = "#cb181d")) +
  labs(#title = "Evaluación del modelo",
    x = "SVR-Model", color = "") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.y.right = element_text(size = 16),  # Para RMSE
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(size = 14),  # Ticks del eje derecho
    legend.text = element_text(size = 14),
    legend.position = "none" 
  )


SVR_temporal


## Modelo predictivo ET  temporal ----
estacion <- "BA"
modelo <- "1"

#datos
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/", estacion,
                             "/modelos/ParticionDataSet/Modelo_", modelo,
                             "/M", modelo, "_train_", estacion, ".csv", sep=""))

test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/", estacion,
                            "/modelos/ParticionDataSet/Modelo_", modelo,
                            "/M", modelo, "_test_", estacion, ".csv", sep=""))


train_data$year <- as.numeric(format(as.Date(train_data$date), "%Y"))

# Años unicos
years <- sort(unique(train_data$year))

# Indices para generar el Leave-One-Year-Out
index_list <- list()
indexOut_list <- list()
# Recorre los años y crea los subconjuntos

for (i in seq_along(years)) {
  test_year <- years[i]
  train_index <- which(train_data$year != test_year)
  test_index <- which(train_data$year == test_year)
  
  index_list[[i]] <- train_index
  indexOut_list[[i]] <- test_index
}

# Control de entrenamiento para cv temporal
train_control_temporal <- trainControl(
  method = "cv",
  number = length(years),
  index = index_list,
  indexOut = indexOut_list,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Entrenamiento
modelo_et_temporal <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean + #t2m_mean
      DEM+
    d2m_mean   +v10_mean + u10_mean + tp_mean + dayWeek,
  data = train_data,
  method = "ranger",
  trControl = train_control_temporal,
  tuneGrid = data.frame(
    mtry = 5,
    splitrule = "extratrees",
    min.node.size = 5
  ),
  importance = "impurity"
)


#Guardar modelo
getwd()
save(modelo_et_temporal, file=paste("01-ET-CV-Temp_M",modelo,"-180625-",estacion,".RData",sep=""))

# Metricas globales
resultados_ET_cv_Temporal <- evaluar_modelo(modelo=modelo_et_temporal, datos_test=test_data, variable_real = "PM25",tipoModelo="ET",y_test=NA)
print(resultados_ET_cv_Temporal)

### metricas por año
df_metricas<- data.frame(modelo_et_temporal[["resample"]])
# Reescalar RMSE para hacer el plot
max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)
df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)
df_metricas$year <- recode(df_metricas$Resample,
                           "Resample1" = "2015",
                           "Resample2" = "2016",
                           "Resample3" = "2017",
                           "Resample4" = "2018",
                           "Resample5" = "2019",
                           "Resample6" = "2020",
                           "Resample7" = "2021",
                           "Resample8" = "2022",
                           "Resample9" = "2023",
                           # Deja las que no cambian fuera o ponelas igual a sí mismas
                           .default = df_metricas$Resample
)


df_metricas$year <- as.numeric(as.character(df_metricas$year))

# Plot metricas por año
ET_temporal<-ggplot(df_metricas, aes(x = year)) +
  geom_line(aes(y = Rsquared, color = "R2"), size = 1.2) +
  geom_point(aes(y = Rsquared, color = "R2"), size = 1.8) +
  geom_line(aes(y = rmse_escalado, color = "RMSE"), size = 1.2)+#, linetype = "dashed") +
  geom_point(aes(y = rmse_escalado, color = "RMSE"), size = 1.8)+#, shape = 1) +
  scale_y_continuous(
    name = "R2",
    sec.axis = sec_axis(~ . * (max_rmse - min_rmse) + min_rmse, name = "RMSE")
  ) +
  scale_x_continuous(breaks = 2015:2023) + 
  scale_color_manual(values = c("R2" = "#2c7fb8",  "RMSE" = "#cb181d")) +
  labs(
    x = "ET-Model", color = "") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.y.right = element_text(size = 16),  # Para RMSE
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(size = 14),  # Ticks del eje derecho
    legend.text = element_text(size = 14),
    legend.position = "none" 
  )
ET_temporal


## Modelo predictivo RF temporal ----
estacion <- "MX"
modelo <- "1"
#Data
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
train_data$year <- as.numeric(format(as.Date(train_data$date), "%Y"))

# Extraer los años
years <- sort(unique(train_data$year))

# Crear indices para Leave-One-Year-Out
index_list <- list()
indexOut_list <- list()
# Recorremos la lista con los años
for (i in seq_along(years)) {
  test_year <- years[i]
  train_index <- which(train_data$year != test_year)
  test_index <- which(train_data$year == test_year)
  
  index_list[[i]] <- train_index
  indexOut_list[[i]] <- test_index
}

# Control de entrenamiento para CV temporal
train_control_temporal <- trainControl(
  method = "cv",
  number = length(years),
  index = index_list,
  indexOut = indexOut_list,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE
)


# Entrenar modelo con CV temporal
rf_temporal_model <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + t2m_mean +
    DEM+
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + #sp_mean +
    d2m_mean   +v10_mean + u10_mean + tp_mean + dayWeek,
  data = train_data,
  method = "rf",
  trControl = train_control_temporal,
  tuneGrid = data.frame(mtry = 5),
  importance = TRUE
)


#Guardar
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(rf_temporal_model, file=paste("01-RF-CV-Temp_M",modelo,"-180625-",estacion,".RData",sep=""))
# Cargar modelos
load(file=paste("01-RF-CV-Temp_M",modelo,"-180625-",estacion,".RData",sep=""))
load(file=paste("01-RF-CV-Temp_M",modelo,"-270525-",estacion,".RData",sep=""))
load(file=paste("01-RF-CV-Temp_M",modelo,"-290525-",estacion,".RData",sep=""))

# Metricas globales
resultados_RF_cv_Temporal <- evaluar_modelo(modelo=rf_temporal_model, datos_test=test_data, variable_real = "PM25",tipoModelo="SVR",y_test=NA)
print(resultados_RF_cv_Temporal)

### metricas por año
df_metricas<- data.frame(modelo_RF_temporal[["resample"]])
#Min/Maz del RMSE para hacer plot
max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)
# Rescalado del RMSE con el Min/Max
df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)
df_metricas$year <- recode(df_metricas$Resample,
                           "Resample1" = "2015",
                           "Resample2" = "2016",
                           "Resample3" = "2017",
                           "Resample4" = "2018",
                           "Resample5" = "2019",
                           "Resample6" = "2020",
                           "Resample7" = "2021",
                           "Resample8" = "2022",
                           "Resample9" = "2023",
                           # Deja las que no cambian fuera o ponelas igual a sí mismas
                           .default = df_metricas$Resample
)

df_metricas$year <- as.numeric(as.character(df_metricas$year))
# Plot de metricas con dos ejes
ET_temporal<-ggplot(df_metricas, aes(x = year)) +
  geom_line(aes(y = Rsquared, color = "R2"), size = 1.2) +
  geom_point(aes(y = Rsquared, color = "R2"), size = 1.8) +
  geom_line(aes(y = rmse_escalado, color = "RMSE"), size = 1.2)+#, linetype = "dashed") +
  geom_point(aes(y = rmse_escalado, color = "RMSE"), size = 1.8)+#, shape = 1) +
  scale_y_continuous(
    name = "R²",
    sec.axis = sec_axis(~ . * (max_rmse - min_rmse) + min_rmse, name = "RMSE")
  ) +
  scale_x_continuous(breaks = 2015:2023) + 
  scale_color_manual(values = c("R2" = "#2c7fb8",  "RMSE" = "#cb181d")) +
  labs(
    x = " ", color = "") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.title.y.right = element_text(size = 14),  # Para RMSE
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.y.right = element_text(size = 14),  # Ticks del eje derecho
    legend.text = element_text(size = 10),
    legend.position = "none"  # opcional: ubica la leyenda arriba
  )
ET_temporal


## Modelo predictivo XGB  temporal ----
estacion <- "CH"
modelo <- "1"
#Data modelo
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
train_data$year <- as.numeric(format(as.Date(train_data$date), "%Y"))
#Variable
vars <- c("AOD_055",
          "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia" "sp_mean",
          "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
          "d2m_mean","v10_mean", "t2m_mean", 
          "u10_mean",  "tp_mean", "DEM",
          "dayWeek")
target <- "PM25"

# Dataframes para guardar predicciones
predicciones <- data.frame()
pred_entrenamiento <- data.frame()
anios <- sort(unique(train_data$year))

# El procesamiento es diferente a los modelos anteriores
for (test_year in anios) {
  cat("Procesando a?o:", test_year, "\n")
  # Generacion del fold de entrenamiento/testeo
  train_fold <- train_data %>% filter(year != test_year)
  test_fold <- train_data %>% filter(year == test_year)
  # Matriz necesaria par xgb a diferencia de los anteriores
  dtrain <- xgb.DMatrix(data = as.matrix(train_fold[, vars]), label = train_fold[[target]])
  dtest <- xgb.DMatrix(data = as.matrix(test_fold[, vars]), label = test_fold[[target]])
  # Modelo
  xgb_model <- xgboost(
    data = dtrain,
    objective = "reg:squarederror",
    nrounds = 100,
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    verbose = 0
  )
  
  # Predicciones para testeo
  preds_test <- predict(xgb_model, dtest)
  predicciones <- rbind(predicciones, data.frame(obs = test_fold[[target]], pred = preds_test, year = test_fold$year))
  # Predicciones para entrenamiento
  preds_train <- predict(xgb_model, dtrain)
  pred_entrenamiento <- rbind(pred_entrenamiento, data.frame(obs = train_fold[[target]], pred = preds_train,year=test_year))
}

# Metricas de evaluacion
eval_metrics <- function(obs, pred) {
  data.frame(
    # year = year,
    R2 = cor(obs, pred)^2,
    Pearson = cor(obs, pred, method = "pearson"),
    RMSE = rmse(obs, pred),
    MAE = mae(obs, pred),
    MAPE = mape(obs, pred) * 100,
    MSE = mse(obs, pred),
    MedAE = median(abs(obs - pred)),
    Min_Pred = min(pred),
    Max_Pred = max(pred),
    bias = mean(pred - obs)
  )
}
# Metricas por año
metricas_por_anio <- predicciones %>%
  group_by(year) %>%
  summarise(
    R2 = round(cor(obs, pred)^2,2),
    Pearson = round(cor(obs, pred, method = "pearson"),2),
    RMSE =round( rmse(obs, pred),2),
    MAE = round(mae(obs, pred),2),
    MAPE = round(mape(obs, pred) * 100,2),
    MSE = round(mse(obs, pred),2),
    MedAE = round(median(abs(obs - pred)),2),
    bias = mean(pred - obs),
    .groups = "drop"
  )

# Mostrar metricas por a?o
cat("\n### Metricas de validacion por a?o(Leave-One-Year-Out)\n")
print(round(metricas_por_anio, 2))
d <- data.frame(metricas_por_anio)

View(d)

metrics_train <- eval_metrics(pred_entrenamiento$obs, pred_entrenamiento$pred)
metrics_test <- eval_metrics(predicciones$obs, predicciones$pred)

cat("### TRAIN\n")
print(round(metrics_train, 2))

cat("\n### TEST\n")
print(round(metrics_test, 2))
e<- rbind(round(metrics_train,2),round(metrics_test,2))

# Guardar modelo final entrenado con todo el train_data
dtrain_final <- xgb.DMatrix(data = as.matrix(train_data[, vars]), label = train_data[[target]])
# Modelo final
modelo_xgb_final <- xgboost(
  data = dtrain_final,
  objective = "reg:squarederror",
  nrounds = 100,
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)

#Guardar
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_xgb_final, file = paste0("01-XGB-CV-Temp_M", modelo, "-180625-", estacion, ".RData"))
# Cargar
load(paste("01-ET-CV-Esp_M",modelo,"-180625-",estacion,".RData",sep=""))
load(paste("D:/Josefina/Proyectos/Tesis/CH/modelos/01-XGB-CV-Temp_M1-180625-CH.RData",sep=""))
# Crear el dataframe
resultados <- data.frame(
  anio = 2015:2023,
  R2 = c(0.45,0.51,0.49,0.49,0.47,0.64,0.54,0.55,0.47),
  RMSE = c(10.27,11.44,6.98,5.50,5.59,6.57,4.66,4.40,4.44)
)

#XGB PARA CH
resultados <- data.frame(
  anio = 2015:2023,
  R2 = c(0.73,0.66,0.76,0.78,0.74,0.68,0.81,0.76,0.79),
  RMSE = c(11.9,10.3,8.18,7.15,7.07,7.81,7.69,8.48,7.16)
)

resultados_largo <- resultados %>%
  pivot_longer(cols = c(R2, RMSE), names_to = "Metrica", values_to = "Valor")
resultados$anio <- df$year

#Plor lineas 
ggplot(resultados, aes(x = anio, y = R2)) +
  geom_line(stat = "identity", color = "#0570b0", fill = "#74a9cf") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = 2015:2023) +
  labs(x = "XGB Spatial", y = "R² ") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Esto es solo para el grafico, no modifica tus datos originales
max_rmse <- max(resultados$RMSE)
min_rmse <- min(resultados$RMSE)
#Reescalar el plot original
resultados$rmse_escalado <- (resultados$RMSE - min_rmse) / (max_rmse - min_rmse)
#RF
rmse_all <- (7.040815 - min_rmse) / (max_rmse - min_rmse)

xgb_temporal<-ggplot(resultados, aes(x = anio)) +
  geom_line(aes(y = R2, color = "R2"), size = 1.2) +
  geom_point(aes(y = R2, color = "R2"), size = 1.8) +
  geom_line(aes(y = rmse_escalado, color = "RMSE"), size = 1.2)+#, linetype = "dashed") +
  geom_point(aes(y = rmse_escalado, color = "RMSE"), size = 1.8)+#, shape = 1) +
  scale_y_continuous(
    name = "R2",
    sec.axis = sec_axis(~ . * (max_rmse - min_rmse) + min_rmse, name = "RMSE")
  ) +
  #XGB
  
  scale_x_continuous(breaks = 2015:2023) + 
  scale_color_manual(values = c("R²" = "#2c7fb8",  "RMSE" = "#cb181d")) +
  labs(#title = "Evaluación del modelo",
    x = "XGB-Model", color = "") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.y.right = element_text(size = 16),  # Para RMSE
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "none"  
  )



#######################################################################
## OBJETIVO: Modelos Predictivos de PM2.5 simples
##
#######################################################################
# Funcion para evaluar el desempeño de los modelos
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

##############################################################################
##############################################################################
##############################################################################
### ----- Modelo predictivo Regresion Lineal Simple   -----
# Cargar los datos
estacion <- "CH"
modelo <- "1"

dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Ajustar el modelo de RLS
modelo_lm <- lm(PM25 ~ AOD_055, data = train_data)

# Evaluar el desempeño
resultados_lm <- evaluar_modelo(modelo = modelo_lm,
                                datos_test = test_data,
                                variable_real = "PM25",
                                tipoModelo = "LM", 
                                y_test = NULL)

print(resultados_lm)

# Predicciones sobre los datos de entrenamiento
test_data$pred <- predict(modelo_lm, newdata = test_data)
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion
plot_RLS<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +   
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") + 
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 

  theme_classic()
plot_RLS

#####
# Mismo plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion. Pero con la densidad de puntos (colores)
library(ggpointdensity)  

plot_RLS <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  #
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  theme_classic()+ 
  theme(
    #legend.position = "none",
    axis.text = element_text(size = 14),     
    axis.title = element_text(size = 11)     
  )+  labs(
    x = " ",   
    y = " "    
  ) 

plot_RLS

##############################################################################
##############################################################################
##############################################################################

### ----- Modelo predictivo Regresion Lineal Simple   -----
## --- Pero se corrige AOD por la PBL (ERA5)
# Cargar los datos
estacion <- "CH"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)
#Datos
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))
#Correccion del AOD por blh segun Xu and Zhang (2020)
train_data$AOD_055_correct <- train_data$AOD_055 /train_data$blh_mean
test_data$AOD_055_correct <- test_data$AOD_055 /test_data$blh_mean

# Ajustar el modelo de RLS
modelo_lm <- lm(PM25 ~ AOD_055_correct, data = train_data)

# Evaluar el desempeño
resultados_lm <- evaluar_modelo(modelo = modelo_lm,
                                datos_test = test_data,
                                variable_real = "PM25",
                                tipoModelo = "LM", 
                                y_test = NULL)

print(resultados_lm)

# Predicciones sobre los datos de entrenamiento
test_data$pred <- predict(modelo_lm, newdata = test_data)
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion
plot_RLS_aod<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     
  geom_abline(slope = 1, intercept = 0, color = "black", ) + 
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  
  theme_classic()+  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),     
    axis.title = element_text(size = 11)     
  )+  labs(
    x = " ",   
    y = " "    
  ) 
plot_RLS_aod


#####
# Mismo plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion. Pero con la densidad de puntos (colores)
library(ggpointdensity)  

plot_RLS <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 
  theme_classic()+ theme(legend.position="none")+  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),  
    axis.title = element_text(size = 11)   
  )+  labs(
    x = " ",   # 
    y = " "     
  ) 

plot_RLS

#############################################################################
#############################################################################
### ----- Modelo predictivo Regresion Lineal Simple   -----
## --- Corregir AOD por la humedad
estacion <- "CH"
modelo <- "RH"
#Data
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Funcion de la humedad
train_data$funcRH <- 1/(1-(train_data$RH/100))
test_data$funcRH <- 1/(1-(test_data$RH/100))
#Correccion del AOD por rh segun Xu and Zhang (2020)
train_data$AOD_055_correct <- train_data$AOD_055 /train_data$funcRH
test_data$AOD_055_correct <- test_data$AOD_055 /test_data$funcRH

# Ajustar el modelo de RLS
modelo_lm <- lm(PM25 ~ AOD_055_correct, data = train_data)

# Evaluar el desempeño 
resultados_lm <- evaluar_modelo(modelo = modelo_lm,
                                datos_test = test_data,
                                variable_real = "PM25",
                                tipoModelo = "LM", 
                                y_test = NULL)

print(resultados_lm)

# Predicciones sobre los datos de entrenamiento
test_data$pred <- predict(modelo_lm, newdata = test_data)
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesio
plot_RLS_aod<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") + 
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  
  theme_classic()
plot_RLS_aod

#####
# Mismo plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion. Pero con la densidad de puntos (colores)
library(ggpointdensity) 

plot_RLS <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  theme_classic()+ theme(legend.position="none")+ theme(
    legend.position = "none",
    axis.text = element_text(size = 14),    
    axis.title = element_text(size = 11)     
  )+  labs(
    x = " ",  
    y = " "    
  ) 

plot_RLS

##############################################################################
##############################################################################
### ----- Modelo predictivo Regresion Lineal Simple   -----
## --- Corregir AOD por la PBL + humedad
estacion <- "CH"
modelo <- "RH"
# Datos
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

#Correccion AOD por BLH+RH segun Xu and Zhang (2020)
train_data$AOD_corrected <- (train_data$AOD_055 * (1 - train_data$RH / 100)) / train_data$blh_mean
test_data$AOD_corrected <- (test_data$AOD_055 * (1 - test_data$RH / 100)) / test_data$blh_mean

# Ahora entrenamos el modelo con el AOD corregido
modelo_lm <- lm(PM25 ~ AOD_corrected, data = train_data)

# Evaluar el desempeño
resultados_lm <- evaluar_modelo(modelo = modelo_lm,
                                datos_test = test_data,
                                variable_real = "PM25",
                                tipoModelo = "LM",  
                                y_test = NULL)

print(resultados_lm)
# Predicciones sobre los datos de entrenamiento
test_data$pred <- predict(modelo_lm, newdata = test_data)
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion
plot_RLS_aod<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  theme_classic()
plot_RLS_aod

#####
# Mismo plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion. Pero con la densidad de puntos (colores)

library(ggpointdensity)  

plot_RLS <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  theme_classic()+ theme(legend.position="none")+ theme(
    legend.position = "none",
    axis.text = element_text(size = 14),    
    axis.title = element_text(size = 11)     
  )+  labs(
    x = " ",   
    y = " "   
  ) 

plot_RLS

##############################################################################
##############################################################################
## ### ----- Modelo predictivo Regresion Lineal Multiple   -----
#Con todas las variables seleccionadas para cada centro urbano
# Cargar los datos
estacion <- "CH"
modelo <- "1"
#Datos
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Ajustar el modelo de RLM
modelo_lm_multiple <- lm(PM25 ~  AOD_055+ndvi + BCSMASS_dia + DUSMASS_dia + #OCSMASS
                           SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + #sp_mean+
                           t2m_mean + 
                           DEM+  d2m_mean   +v10_mean + u10_mean + tp_mean+ dayWeek,
                         data = train_data)
# Ajustar el modelo de RLM CON AOD Corregido
# modelo_lm_multiple <- lm(PM25 ~ AOD_055_correct + ndvi + BCSMASS_dia + DUSMASS_dia +
#                            SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
#                            d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
#                          data = train_data)
# Evaluar el modelo usando tu funcion
resultados_lm_multiple <- evaluar_modelo(modelo = modelo_lm_multiple,
                                         datos_test = test_data,
                                         variable_real = "PM25",
                                         tipoModelo = "LM", 
                                         y_test = NULL)

# Mostrar los resultados
print(resultados_lm_multiple)
# Revisar si se obtuvieron predicciones negativas, y el rango
test_data <- data.frame(pred=predict(modelo_lm_multiple, newdata = test_data) , PM25=test_data$PM25)
test_data <- test_data[test_data$pred>0,]
# Predicciones sobre los datos de entrenamiento
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion
plot_RLM<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 
  
  theme_classic()
plot_RLM


#####
# Mismo plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion. Pero con la densidad de puntos (colores)
library(ggpointdensity)  

plot_RLM <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  theme_classic()+ theme(legend.position="none")+ theme(
    legend.position = "none",
    axis.text = element_text(size = 14),   
    axis.title = element_text(size = 11)     
  )+  labs(
    x = " ",  
    y = " "   
  ) 

plot_RLM


##############################################################################
##############################################################################
### ----- Modelo predictivo Regresion Lineal Multiple   ----- 
#-- Se agrgan las variables progresivamente de a una
# Cargar datos
library(reshape2)
estacion <- "CH"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)
#Datos
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Orden de variables a agregar, ver para cada centro urbano en especiañl
variables <- c("blh_mean", "d2m_mean", "t2m_mean","v10_mean", "u10_mean", "tp_mean", # 
               "ndvi", "BCSMASS_dia", "DUSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia",
               "SSSMASS_dia", "DEM","dayWeek")#"sp_mean",

# Lista para guardar resultados
resultados_modelos <- list()

# Incluir primer modelo solo con AOD_055
vars_actuales <- "AOD_055"#"AOD_055_correct" #
formula_lm <- as.formula(paste("PM25 ~", vars_actuales))
modelo_lm <- lm(formula_lm, data = train_data)
resultados <- evaluar_modelo(modelo = modelo_lm,
                             datos_test = test_data,
                             variable_real = "PM25",
                             tipoModelo = "LM",
                             y_test = NULL)
resultados$Modelo <- "Modelo_1"
resultados$Variables <- vars_actuales
resultados_modelos[[1]] <- resultados

# Iterar para agregar progresivamente las variables
for (i in seq_along(variables)) {
  vars_actuales <-  c("AOD_055", variables[1:i])#c("AOD_055_correct", variables[1:i])
  formula_lm <- as.formula(paste("PM25 ~", paste(vars_actuales, collapse = " + ")))
  modelo_lm <- lm(formula_lm, data = train_data)
  
  resultados <- evaluar_modelo(modelo = modelo_lm,
                               datos_test = test_data,
                               variable_real = "PM25",
                               tipoModelo = "LM",
                               y_test = NULL)
  
  resultados$Modelo <- paste0("Modelo_", i + 1)  # +1 porque ya hicimos el primero RLS-AOD
  resultados$Variables <- paste(vars_actuales, collapse = ", ")
  
  resultados_modelos[[i + 1]] <- resultados
}

# Combinar resultados
tabla_resultados <- do.call(rbind, resultados_modelos)
tabla_resultados$Num_Variables <- sapply(strsplit(tabla_resultados$Variables, ", "), length)

# Convertir a formato largo para ggplot
data_melt <- melt(tabla_resultados[, c("Num_Variables", "R2", "RMSE")], id.vars = "Num_Variables")

# Establecer limites por variable para el plot
limites <- data.frame(
  variable = c("R2", "RMSE"),
  ymin = c(0, 6),
  ymax = c(1, 12)
)
#Union de info con ambs dataset
data_melt_limited <- left_join(data_melt, limites, by = "variable")

# Renombrar variable con notacion matematica (?
data_melt_limited$variable <- recode(data_melt_limited$variable,
                                     "R2" = "R^2",
                                     "RMSE" = "RMSE")

# Plot con facet parseados
ggplot(data_melt_limited, aes(x = Num_Variables, y = value)) +
  geom_blank(aes(y = ymin)) +
  geom_blank(aes(y = ymax)) +
  geom_line(aes(color = variable), size = 1.2) +
  geom_point(aes(color = variable), size = 2) +
  facet_wrap(~variable, scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(breaks = 1:16) +
  labs(title = " ",
       x = "",
       y = "",
       color = "Metrica") +
  theme_classic()+
  theme(
    # axis.title.x = element_text(size = 16),
    # axis.title.y = element_text(size = 16),
    # axis.title.y.right = element_text(size = 16),  # Para RMSE
    # axis.text.x = element_text(size = 14),
    # axis.text.y = element_text(size = 14),
    # axis.text.y.right = element_text(size = 14),  # Ticks del eje derecho
    # legend.text = element_text(size = 14),
    legend.position = "none"  
  )
#Guardar resultados de cada uno de los modelos obenidos
write.csv(tabla_resultados,"tabla_resultados_MX.csv")

#############################################################################
#############################################################################
### ----- Modelo predictivo LASSO   -----
# Distintas pruebas
# Kunjir 2025: alpha=0.1; max_iter = 1500, ensuring convergence.
# Bagheri 2022: regularization parameter: 0.1 
# alpha = 1 puro Lasso (penalizacion L1).
# alpha = 0 puro Ridge (penalizacion L2).

library(glmnet)

# Funcion para evaluar modelos glmnet (Lasso o Ridge)
evaluar_glmnet <- function(modelo, x_test, y_test, lambda_usar) {
  predicciones <- predict(modelo, newx = x_test, s = lambda_usar)
  df <- data.frame(predicciones=as.numeric(predicciones),y_test=y_test)
  
  df <- df[df$predicciones>0,]
  r2 <- cor(df$predicciones, df$y_test)^2
  pearson <- cor(df$y_test, df$predicciones, method = "pearson")
  rmse <- sqrt(mean((df$predicciones - df$y_test)^2))
  bias <- mean(df$predicciones - df$y_test)
  
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

# Datos
estacion <- "CH"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Seleccionar variables predictoras segun sitio

variables <- c("AOD_055", "ndvi", "BCSMASS_dia", "DUSMASS_dia", 
               "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", #"sp_mean",
               "blh_mean",  "d2m_mean","v10_mean", "t2m_mean" ,
               "u10_mean", "tp_mean","DEM", "dayWeek")

# Estandarizar predictores (media 0, sd 1) usando los parametros de entrenamiento

train_scaled <- scale(train_data[, variables])
test_scaled <- scale(test_data[, variables], center = attr(train_scaled, "scaled:center"), 
                     scale = attr(train_scaled, "scaled:scale"))

# Preparar matrices modelo
x_train <- as.matrix(train_scaled)
y_train <- train_data$PM25

x_test <- as.matrix(test_scaled)
y_test <- test_data$PM25

# Ajustar Lasso con sin cv y validacion cruzada. Varias pruebas

set.seed(123) 
# lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = 0.1)
# lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = 0.5)
# lasso_model <- glmnet(x_train, y_train, alpha = 0.9, lambda = 0.5)
#cv_lasso <- cv.glmnet(x_train, y_train, alpha = 0.1)
#cv_lasso <- cv.glmnet(x_train, y_train, alpha = 0.1, maxit = 1500)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
# cv_lasso <- cv.glmnet(x_train, y_train, alpha = 0.5, maxit = 500)

# Mejor lambda
best_lambda <- lasso_model$lambda.min
best_lambda <- cv_lasso$lambda.min
cat("Mejor lambda:", best_lambda, "/n")

# Evaluar modelo
resultados_lasso <- evaluar_glmnet(cv_lasso, x_test, y_test, lambda_usar = best_lambda)
print(resultados_lasso)

resultados_lasso <- evaluar_glmnet(lasso_model, x_test, y_test, lambda_usar = best_lambda)
print(resultados_lasso)

# Ver coeficientes del modelo Lasso
coef_lasso <- coef(lasso_model, s = best_lambda)
coef_lasso <- coef(cv_lasso, s = best_lambda)
print(coef_lasso)
# Obtener coeficientes del modelo
lasso_model
coeficientes <- coef(lasso_model, s = best_lambda)
coeficientes <- coef(cv_lasso, s = best_lambda)

# Convertir a data frame
coef_df <- as.data.frame(as.matrix(coeficientes))
coef_df$variable <- rownames(coef_df)
coef_df$variable2 <- c("intercept","AOD", 
                       "NDVI", "BCSMASS", "DUSMASS", 
                       "SO2SMASS", "SO4SMASS", "SSSMASS", 
                       #"sp", 
                       "blh",  "d2m","v10", 
                       "t2m" ,
                       "u10", "tp","DEM", "dayWeek")#
colnames(coef_df)[1] <- "coeficiente"

# Filtrar variables con coeficiente distinto de cero (descarta intercepto)
coef_filtrado <- coef_df[coef_df$coeficiente != 0 & coef_df$variable != "(Intercept)", ]

# Ordenar por valor absoluto del coeficiente
coef_filtrado <- coef_filtrado[order(abs(coef_filtrado$coeficiente), decreasing = TRUE), ]

# Graficar importancia variables
ggplot(coef_filtrado, aes(x = reorder(variable2, abs(coeficiente)), y = coeficiente)) +
  geom_col(fill = "#fd8d3c") +
  coord_flip() +
  labs(
       x = "Variables",
       y = "Coeficiente") +
  theme_classic()+theme(
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12)   
  )

# Predicciones sobre los datos de entrenamiento
pred <- predict(lasso_model, newx = x_test, s=best_lambda)
pred <- predict(cv_lasso, newx = x_test, s=best_lambda)
df <- data.frame(pred=pred,y_test=y_test)
names (df) <- c("pred","PM25")
#Revisar si hubo datos negativos en la prediccion
test_data <- df[df$pred>0,]
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion
plot_RLM_Lasso<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "#99000d", alpha = 0.6) +    
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") + 
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  
  theme_classic()
plot_RLM_Lasso


#####
# Mismo plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion. Pero con la densidad de puntos (colores)
library(ggpointdensity) 

plot_RLM_Lasso <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 
  theme_classic()+ theme(legend.position="none")+ theme(
    legend.position = "none",
    axis.text = element_text(size = 14),    
    axis.title = element_text(size = 11)    
  )+  labs(
    x = " ",   
    y = " "     
  ) 

plot_RLM_Lasso


##############################################################################
##############################################################################
### ----- Modelo predictivo RIDGE   -----
# Libreria

library(glmnet)

evaluar_modelo_ridge <- function(modelo, x_test, y_test) {
  predicciones <- predict(modelo, newx = x_test)
  predicciones <- as.numeric(predicciones)
  df <- data.frame(predicciones=as.numeric(predicciones),y_test=y_test)
  
  df <- df[df$predicciones>0,]
  # Metrica
  r2 <- cor(df$predicciones, df$y_test)^2
  pearson <- cor(df$y_test, df$predicciones, method = "pearson")
  rmse <- sqrt(mean((df$predicciones - df$y_test)^2))
  bias <- mean(df$predicciones - df$y_test)
  
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
########
# datos
estacion <- "CH"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")

# Cargar datos
train_data <- read.csv(paste0(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv"))
test_data  <- read.csv(paste0(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv"))

# Preparar matrices para glmnet (quitar intercepto con [,-1])
x_train <- model.matrix(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                          DEM + 
                          t2m_mean +
                          SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + #sp_mean +
                          d2m_mean  + v10_mean + u10_mean + tp_mean + dayWeek, data = train_data)[,-1]
y_train <- train_data$PM25

x_test <- model.matrix(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                         DEM + 
                         t2m_mean +
                         SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + #sp_mean +
                         d2m_mean  + v10_mean + u10_mean + tp_mean + dayWeek, data = test_data)[,-1]
y_test <- test_data$PM25
# Ajustar modelo Ridge sin validación cruzada
#Probar distintos lambdas manualmente: 

#Lambda es muy chico: la penalización casi desaparece, similar a RL
#Lambda mas grande: los coeficientes se reducen mucho (se acercan a 0) 
# el modelo es mas simple y más estable, pero puede perder capacidad predictiva si se pasa.

#lambdas <- c(0.0001, 0.001, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50)

ridge_model <- glmnet(x_train, y_train, 
                      alpha = 0,           
                      lambda = 0.1,         
                      standardize = TRUE)   # estandariza las variables predictoras

# Ridge con cv
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, standardize = TRUE)
best_lambda <- cv_ridge$lambda.min
cat("Mejor lambda Ridge:", best_lambda, "/n")
#Si hago pruebas manuales,obtendre resultados similres?
#lambdas <- c(0.0001, 0.001, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50)
best_lambda <- 0.1


# Ajustar modelo final Ridge con mejor lambda
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda, standardize = TRUE)

# Predicciones
predicciones <- predict(ridge_model, s = best_lambda, newx = x_test)
predicciones_num <- as.numeric(predicciones)

# Data frame para graficar
df <- data.frame(
  valores_reales = y_test,
  predicciones = predicciones_num
)

# Limites iguales para x e y
min_val <- min(c(df$valores_reales, df$predicciones), na.rm = TRUE)
max_val <- max(c(df$valores_reales, df$predicciones), na.rm = TRUE)

## Predicciones sobre los datos de entrenamiento
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion.
ggplot(df, aes(x = valores_reales, y = predicciones)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  coord_fixed(ratio = 1) +
  xlim(min_val, max_val) +
  ylim(min_val, max_val) +
  labs(
    title = "Predicciones Ridge vs Valores Reales de PM2.5",
    x = "Valores Reales de PM2.5",
    y = "Predicciones del Modelo Ridge"
  ) +
  theme_minimal()

# Extraer coeficientes del modelo Ridge
coef_ridge <- coef(ridge_model)
coef_df <- data.frame(
  variable = rownames(coef_ridge),
  coeficiente = as.numeric(coef_ridge)
)

# Quitar el intercepto
coef_df <- coef_df[coef_df$variable != "(Intercept)", ]

# Plot de importancia de variables (coeficientes)
ggplot(coef_df, aes(x = reorder(variable, abs(coeficiente)), y = coeficiente)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = paste("Importancia de variables - Modelo Ridge (Î» =", round(best_lambda, 4), ")"),
    x = "Variables",
    y = "Coeficientes"
  ) +
  theme_minimal()
evaluar_modelo_ridge(ridge_model, x_test, y_test)
evaluar_modelo_ridge(cv_ridge, x_test, y_test)

# Predicciones sobre los datos de entrenamiento
pred <- predict(ridge_model, newx = x_test, s=best_lambda)
df <- data.frame(pred=pred,PM25=y_test)
names(df) <- c("pred","PM25")
# Ver si se obtuvieron valores negativos, revisar porque!
test_data <- df[df$pred>0,]


# Predicciones sobre los datos de entrenamiento
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion.
plot_RLM_ridge<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     
  geom_abline(slope = 1, intercept = 0, color = "black", ) + 
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  n
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  
  theme_classic()
plot_RLM_ridge

#####
# Mismo plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion. Pero con la densidad de puntos (colores)
library(ggpointdensity)  

plot_RLM_ridge<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  theme_classic()+ theme(legend.position="none")+ theme(
    legend.position = "none",
    axis.text = element_text(size = 14),     
    axis.title = element_text(size = 11)    
  )+  labs(
    x = " ",  
    y = " "    
  ) 

plot_RLM_ridge
##############################################################################
##############################################################################
##############################################################################
## ### ----- Modelo predictivo RIDGE  -----
# Otra prueba
# Cargar librerÃ­as necesarias
library(glmnet)

# Crear una secuencia de lambda en escala logaritmica
lambda_grid <- 10^seq(4, -4, length = 100)

# Validacion cruzada (k=10) para modelo Ridge (alpha = 0)
cv_ridge <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 0,                  # alpha = 0 para Ridge
  lambda = lambda_grid,       # secuencia de lambda
  nfolds = 10,                # 10-fold cross-validation
  standardize = TRUE          # estandariza variables 
)

# Mostrar el mejor lambda
best_lambda <- cv_ridge$lambda.min
cat("Mejor lambda:", best_lambda, "/n")

# Graficar el error medio de cv segun lambda
plot(cv_ridge)
abline(v = log(best_lambda), col = "red", lty = 2)

# Ajustar el modelo final usando el mejor lambda
ridge_model_final <- glmnet(
  x = x_train,
  y = y_train,
  alpha = 0,
  lambda = best_lambda,
  standardize = TRUE
)

# Coeficientes del modelo
coef(ridge_model_final)
evaluar_modelo_ridge(ridge_model_final, x_test, y_test)


##############################################################################
##############################################################################
## ### ----- Modelo predictivo Modelo Aditivo Generalizado (GAM)  ----- 
#Libreria
library(mgcv)

# Datos
estacion <- "CH"
modelo <- "1"
# Cargar datasets
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# s() define un término suave para una variable. controla cuánto "se curva" la relación entre X variable y PM25
# k: Controla la complejidad del suavizado (cuántas "flexiones" puede tener la curva).
#Valores más grandes: curva más flexible, capaz de capturar relaciones no lineales complejas.
#Valores más pequeños:curva más suave, menos riesgo de sobreajuste.
# k = 5-10: para relaciones suaves y simples.
# k = 15-25: si esperás curvas más complejas o muchas observaciones.

#####----- Distintas pruebas del GAM ----- #####



model.gam1 <-gam(PM25~s(AOD_055,        #GAM formula (Y~x1+x2...) with a smooth function on AOD_550 
                        k=15),          #the dimension of the basis used to represent the smooth term
                 data=train_data,              #A data frame or list containing the model response variable and covariates required by the formula
                 method="ML")             #Maximum Likelihood smoothin parameter estimation method
model.gam1 <-gam(PM25~s(AOD_055, k = 15, bs = "cr"))

# Ajustar el modelo GAM (con suavizadores para variables continuas)  
modelo_gam <- gam(PM25 ~ s(AOD_055) + s(ndvi) + s(BCSMASS_dia) + s(DUSMASS_dia) +
                    s(SO2SMASS_dia) + s(SO4SMASS_dia) + s(SSSMASS_dia) +#s(sp_mean)+
                    s(blh_mean) + s(d2m_mean) + s(DEM, k=5) + s(t2m_mean)+ 
                    s(v10_mean) + s(u10_mean) + s(tp_mean)+ dayWeek,
                  data = train_data,method="ML")
modelo_gam <- gam(PM25 ~ s(AOD_055, k=10) + s(ndvi, k=20) + s(BCSMASS_dia, k=10) + 
                                        s(DUSMASS_dia, k=10) + s(SO2SMASS_dia, k=10) + s(SO4SMASS_dia, k=10) +
                                        s(SSSMASS_dia, k=12) + s(blh_mean, k=10) + s(sp_mean, k=10) +
                                        s(d2m_mean, k=10) + s(v10_mean, k=10) + s(u10_mean, k=10) +
                                        s(tp_mean, k=12) + dayWeek, method = "REML", select = TRUE)
# GAM más flexible
modelo_gam <- gam(PM25 ~ s(AOD_055, k=20, bs="tp") + s(ndvi, k=25, bs="cr") + s(blh_mean, k=15) + 
                  s(sp_mean, k=15) + s(d2m_mean, k=15) + dayWeek,
                data = train_data, method="REML", select=TRUE, gamma=1.2)
modelo_gam <- gam(PM25 ~ s(AOD_055, k=8) + s(ndvi, k=10) + s(blh_mean, k=8) + 
                   s(sp_mean, k=8) + s(d2m_mean, k=8) + dayWeek,
                 data = train_data, method="REML", select=TRUE, gamma=1.4)

# modelo_gam <- gam(PM25 ~ s(AOD_055, k=10) + s(ndvi, k=20) + s(BCSMASS_dia, k=10) + 
#                     s(DUSMASS_dia, k=10) + s(SO2SMASS_dia, k=10) + s(SO4SMASS_dia, k=10) + 
#                     s(SSSMASS_dia, k=12) + s(blh_mean, k=10) + s(sp_mean, k=10) +
#                     s(d2m_mean, k=10) + s(v10_mean, k=10) + s(u10_mean, k=10) + 
#                     s(tp_mean, k=12) + dayWeek,
#                   data = train_data, select = TRUE)
##$ segun MA, 2022
#modelo_gam <- gam (PM25 ~ s(AOD_055) + s   (v10_mean,u10_mean), family, data =    modeling_dataset)


# Ver si el k es suficiente: k-index es bajo (<1) o p-value < 0.05, significa que k es demasiado chico y hay que aumentarlo.
gam.check(modelo_gam)
# Evaluar el modelo usando tu funciÃ³n
resultados_gam <- evaluar_modelo(modelo = modelo_gam,#model.gam1,#
                                 datos_test = test_data,
                                 variable_real = "PM25",
                                 tipoModelo = "GAM",  # puede ser cualquier string excepto "XGB"
                                 y_test = NULL)

plot(modelo_gam)
# Mostrar resultados
print(resultados_gam)

#### Plot de cada una de las variables
# No gusta!!
windows(width = 14, height = 10)
# Graficar todos los terminos suaves del modelo en subplots
plot(modelo_gam, 
     pages = 1,       # todos los graficos en una sola pa¡gina
     se = TRUE,       # muestra bandas de confianza
     rug = TRUE,      # muestra ticks en la base indicando la densidad de datos
     shade = TRUE,    # sombrea los intervalos de confianza
     scale = 0)       # permite comparar la escala entre grÃ¡ficos (usa la misma)

#########################################
##########################################
#Otra prueba
#Lista de variables en orden acumulativo
# Variables numericas (para aplicar s())
vars_numericas <- c("AOD_055","blh_mean", "d2m_mean", "t2m_mean","sp_mean", "v10_mean", "u10_mean", "tp_mean",
                    "ndvi", "BCSMASS_dia", "DUSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia",
                    "SSSMASS_dia","DEM")

# Variables categoricas (no usar s())
vars_categoricas <- c("dayWeek")

# Lista completa en orden
var_orden <- c(vars_numericas, vars_categoricas)

resultados <- data.frame()
vars_utilizadas <- c()
# Recorremos las vriables
for (i in 1:length(var_orden)) {
  print(i)
  vars_utilizadas <- c(vars_utilizadas, var_orden[i])
  # Aplicamos la formula de auna
  partes_formula <- sapply(vars_utilizadas, function(v) {
    if (v %in% vars_numericas) {
      paste0("s(", v, ")")
    } else {
      paste0("factor(", v, ")")
    }
  })
  # Formula basica
  formula_text <- paste("PM25 ~", paste(partes_formula, collapse = " + "))
  formula_gam <- as.formula(formula_text)
  
  modelo <- gam(formula_gam, data = train_data)
  # Evaluar el desempeño
  metrica <- evaluar_modelo(modelo, datos_test = test_data, tipoModelo = "GAM")
  #Agregamos el nombrde de la variable
  metrica$Variables_incluidas <- paste(vars_utilizadas, collapse = ", ")
  metrica$Paso <- i
  # agrupamos esultados
  resultados <- bind_rows(resultados, metrica)
}

# Mostrar resultados
View(resultados)
#Guardamos
getwd()
write.csv(resultados,"GAM_resultados.csv")


######PLOT
# Predicciones sobre los datos de entrenamiento
# test_data$pred <- predict(modelo_gam, newdata = test_data)
pred <- predict(modelo_gam, newdata = test_data)
pred <- predict(model.gam1 , newdata = test_data)
# Hacemos dataframe con valores predichos y medidos
df <- data.frame(pred=pred, PM25=test_data$PM25)
#Vemos si hay datos negativos
test_data <- df[df$pred>0,]
###
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion.
plot_GAM<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +    
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 
  theme_classic()
plot_GAM


#####
# Mismo plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion. Pero con la densidad de puntos (colores)
library(ggpointdensity)  

plot_GAM<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) + 
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  theme_classic()+ theme(legend.position="none")+ theme(
    legend.position = "none",
    axis.text = element_text(size = 14),     
    axis.title = element_text(size = 11)    
  )+  labs(
    x = " ",   
    y = " "   
  )

plot_GAM


##############################################################################
##############################################################################
## ### ----- Modelo predictivo Modelo Lineal Mixto (LME)    -----

library(lme4)
library(caret)  # Para crear folds

# Cargar datos
estacion <- "CH"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)
#Datos
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))
# Seteamos fecha y las estaciones (factor) para agruparlos y hacer distintas pruebas
train_data$fecha <- as.factor(train_data$date)
test_data$fecha <- as.factor(test_data$date)
train_data$estacion <- as.factor(train_data$estacion)
test_data$estacion <- as.factor(test_data$estacion)

# Crear folds para CV (con stratification si fuera necesario, simple)
set.seed(123)
folds <- createFolds(train_data$PM25, k = 10, list = TRUE, returnTrain = FALSE)

# Funcion para entrenar y evaluar un fold
evaluar_fold <- function(indices_test) {

  train_fold <- train_data[-indices_test, ]
  val_fold <- train_data[indices_test, ]
  
  # Entrenar modelo LME en train_fold
  #modelo_fold <- lmer(PM25 ~ 1+ (1|AOD_055) , data = train_fold)
  modelo_fold <- lmer(PM25 ~ AOD_055 + (1  | fecha), data = train_fold)
  
  # Predecir en val_fold
  pred <- predict(modelo_fold, newdata = val_fold, allow.new.levels = TRUE)
  
  # Calcular metricas manualmente
  r2 <- cor(pred, val_fold$PM25)^2
  rmse <- sqrt(mean((pred - val_fold$PM25)^2))
  bias <- mean(pred - val_fold$PM25)
  min <- min(pred )
  max <- max(pred )
  return(data.frame(R2 = r2, RMSE = rmse, Bias = bias,min=min,max=max))
}

# Evaluar todos los folds y sacar promedio
resultados_cv <- lapply(folds, evaluar_fold)
resultados_cv <- do.call(rbind, resultados_cv)

# Promedio y sdde metricas CV
resumen_cv <- data.frame(
  R2_mean = mean(resultados_cv$R2),
  R2_sd = sd(resultados_cv$R2),
  RMSE_mean = mean(resultados_cv$RMSE),
  RMSE_sd = sd(resultados_cv$RMSE),
  Bias_mean = mean(resultados_cv$Bias),
  Bias_sd = sd(resultados_cv$Bias),
  min = mean(resultados_cv$min),
  max = mean(resultados_cv$max)
)

print(resumen_cv)

### Otra prueba
# --- Entrenar modelo final con todo el training set ---
modelo_final <- lmer(PM25 ~ AOD_055 + (1 + AOD_055|dayWeek ), data = train_data)
modelo_final <- lmer(PM25 ~ 1 + ( + 1 | AOD_055), data = train_data)
modelo_final <- lmer(PM25 ~ AOD_055 + (1 + AOD_055 | dayWeek) + (1 | estacion), data = train_data)
modelo_final <- lmer(PM25 ~ AOD_055, data = train_data) 
modelo_final <- lmer(PM25 ~ 1 + (1|AOD_055), data = train_data)
modelo_final <- lmer(PM25 ~ AOD_055+(1+ AOD_055|dayWeek), data = train_data)
modelo_final <- lmer(PM25 ~ AOD_055 + (1 + AOD_055 | estacion), data = train_data, REML = TRUE)

modelo_final <- lmer(PM25 ~ AOD_055 + (1 | estacion/fecha), data = train_data)

summary(modelo_final)


### Pruebas contodas las variables
# Variables a escalar (excluyendo la variable dependiente PM25 y la variable de agrupaciÃ³n fecha)
vars_a_escalar <- c("AOD_055", "t2m_mean", "d2m_mean", "v10_mean", "u10_mean",
                    "blh_mean", "sp_mean", "tp_mean", "ndvi")

# Crear copia del dataset y escalar las variables
train_data_scaled <- train_data
train_data_scaled[vars_a_escalar] <- scale(train_data[vars_a_escalar])

# Ajustar modelo con variables escaladas
modelo_final <- lmer(PM25 ~ AOD_055 + t2m_mean + d2m_mean + v10_mean + u10_mean +
                      blh_mean + sp_mean + tp_mean + ndvi +
                      (1 + AOD_055 | fecha), data = train_data_scaled)

modelo_final <- lmer(PM25 ~ AOD_055 + t2m_mean + d2m_mean + v10_mean + u10_mean +
                   blh_mean + sp_mean + tp_mean + ndvi +
                   (1 + AOD_055 | estacion),
                 data = train_data_scaled, REML = TRUE)
modelo_final <- lmer(PM25 ~ AOD_055 + t2m_mean + d2m_mean + v10_mean + u10_mean +
                   blh_mean + sp_mean + tp_mean + ndvi +
                   (1 + AOD_055 | estacion) + (1 | fecha),
                 data = train_data_scaled, REML = TRUE)

summary(modelo_final)


# Predecir en test_data
predicciones_test <- predict(modelo_final, newdata = test_data, allow.new.levels = TRUE)

# Evaluar modelo
resultados_test <- evaluar_modelo(
  modelo = modelo_final,
  datos_test = test_data,
  variable_real = "PM25",
  tipoModelo = "LME"
)

print(resultados_test)


# Entrenar modelo final con todos los datos de entrenamiento

# Hacer predicciones sobre test_data
test_data$pred <- predict(modelo_lme, newdata = test_data, allow.new.levels = TRUE)
test_data$pred <- predict(modelo_final, newdata = test_data, allow.new.levels = TRUE)
predicciones_test

#Predicciones sobre los datos de entrenamiento
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion.
plot_LME <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +    
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  theme_classic() + theme(legend.position = "none")


#####
# Mismo plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion. Pero con la densidad de puntos (colores)
plot_LME <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  theme_classic() + theme(legend.position = "none")


plot_LME




################################################################################
################################################################################
################################################################################
## ### ----- Modelo predictivo Modelo Lineal Generalizado (GLM)   ----- 
# Cargar datos
estacion <- "CH"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))
#Revisamos si hay datos negativos
train_data <- train_data[train_data$PM25>0,]
test_data <- test_data[test_data$PM25>0,]
# Probar ditintas fuciones
familia <- "gaussian"
familia <- binomial #no!
familia <-"poisson"#no!
familia <-"Gamma"
familia <- "inverse.gaussian" 
familia <- "quasi"
familia <-"quasibinomial" #no!
familia <- "quasipoisson" #no!
#Otras pruebas
familia <- Gamma(link="inverse")
familia <- Gamma(link="identity")
familia <- gaussian(link="log")


glm_model <- glm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + DEM+t2m_mean+
                   SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + #sp_mean +
                   d2m_mean    +v10_mean + u10_mean + tp_mean   +dayWeek,#family = inverse.gaussian(),
                 data = train_data)

glm_model <- glm(PM25 ~ AOD_055,
                 data = train_data)

ctrl <- glm.control(maxit = 100, epsilon = 1e-8, trace = TRUE)

glm_model <-glm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + DEM+t2m_mean+
                  SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + #sp_mean +
                  d2m_mean    +v10_mean + u10_mean + tp_mean, 
    family = Gamma(link="log"),
    data = train_data,
    control = ctrl)


# Predecir en test_data
predicciones_test <- predict(glm_model, newdata = test_data, allow.new.levels = TRUE)

# Evaluar modelo
resultados_test <- evaluar_modelo(
  modelo = glm_model,
  datos_test = test_data,
  variable_real = "PM25",
  tipoModelo = "GLM"
)
resultados_test

# Hacer predicciones sobre test_data
test_data$pred <- predict(glm_model, newdata = test_data, allow.new.levels = TRUE)
# Revisar si me dieron valores negativos
test_data<- test_data[test_data$pred>0,]
####
# Plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion.
plot_glm <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     
  geom_abline(slope = 1, intercept = 0, color = "black", ) + 
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  theme_classic() + theme(legend.position = "none")+ theme(
    legend.position = "none",
    axis.text = element_text(size = 14),     
    axis.title = element_text(size = 11)     
  )+  labs(
    x = " ",  
    y = " "     
  )

plot_glm
#####
# Mismo plot de dispersion con los datos reales medidos vs los predichos
# con lineas de regesion. Pero con la densidad de puntos (colores)

plot_glm <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  theme_classic() + theme(legend.position = "none")+ theme(
    legend.position = "none",
    axis.text = element_text(size = 14),     
    axis.title = element_text(size = 11)    
  )+  labs(
    x = " ",   
    y = " "     
  )

plot_glm


################################################################################
################################################################################
################################################################################
#Plots resumen de todos los modelos
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)

# Datos
desempeño_data <- data.frame(
  Centro = c("SP", "ST", "BA", "MD", "MX"),
  r2_simple = c(2.00E-05, 0.01223, 0.00012, 0.12, 0.16922),
  r2_multiples = c(0.3611, 0.56837, 0.45914, 0.52, 0.48979),
  rmse_simple = c(10.82, 16.368, 10.636, 8.20, 9.068),
  rmse_multiples = c(8.121, 10.823, 7.847, 6.06, 7.104)
)

# Definir el orden deseado de los centros
desempeño_data$Centro <- factor(desempeño_data$Centro, levels = c("SP", "ST", "BA", "MD", "MX"))

# Reorganizar datos en formato largo
# Usar funciones explicitas para evitar conflictos
r2_data <- desempeño_data%>%
  dplyr::select(Centro, r2_simple, r2_multiples) %>%
  #dplyr::select(Centro, rmse_simple, rmse_multiples) %>%
  tidyr::pivot_longer(
    cols = c(r2_simple, r2_multiples),
    #cols = c(rmse_simple, rmse_multiples),
    names_to = "Tipo",
    # values_to = "RMSE"
    values_to = "r2"
  )


# Etiquetas mas legibles
r2_data$Tipo <- factor(r2_data$Tipo, 
                       levels = c("rmse_simple", "rmse_multiples"),
                       labels = c("RMSE simple", "RMSE Multiple"))
r2_data$Tipo <- factor(r2_data$Tipo, 
                       levels = c("r2_simple", "r2_multiples"),
                       labels = c("R² simple", "R² Multiple"))

# Plots
ggplot(r2_data, aes(x = Centro, y = r2, fill = Tipo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(#title = "Comparacion de R2 por ciudad",
      
     x = "Sitio", y = "R²",
       
       fill = "Tipo de modelo") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none") +
  theme_classic()+scale_fill_manual(values = c( "R² simple"= "#225ea8",
                                               "R² mÃºltiple" = "#41b6c4"))
  # theme_classic()+scale_fill_manual(values = c( "RMSE Simple"= "#cb181d",
  #                                               "RMSE Multiple" = "#fb6a4a"))

################################################################################
################################################################################
#Plot con todas las metricas de RLM de todas las ciudades al mismo tiemp
df<- read.csv("D:/Josefina/Proyectos/ProyectoChile/Tot/dataset/tabla_resultados_RLM.csv")

# Filtrar SP y ST
df_sp <- df[df$sitio == "SP", ]
df_ST <- df[df$sitio == "ST", ]

# Combinar los dos sitios
df_comb <- df
df_comb$sitio <- factor(df_comb$sitio , levels = c("SP", "ST", "BA", "MD", "MX"))

# Convertir a formato largo
data_melt <- df_comb %>%
  pivot_longer(cols = c(R2, RMSE), names_to = "Metrica", values_to = "Valor")

# Establecer limites por metrica
limites <- data.frame(
  Metrica = c("R2", "RMSE"),
  ymin = c(0, 1),
  ymax = c(1, 16)
)

# Unir limites
data_plot <- left_join(data_melt, limites, by = "Metrica")

# Renombrar mÃ©tricas para notacion matematica
data_plot$Metrica <- recode(data_plot$Metrica,
                            "R2" = "R^2",
                            "RMSE" = "RMSE")

# Graficar
ggplot(data_plot, aes(x = Num_Variables, y = Valor, color = sitio)) +
  geom_blank(aes(y = ymin)) +
  geom_blank(aes(y = ymax)) +
  geom_point(size = 2) +
  geom_line(size = 1.2) +theme(legend.position = "none") +
  
  facet_wrap(~Metrica, scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(breaks = sort(unique(data_plot$Num_Variables))) +
  scale_color_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256")) +
  
  labs(
       x = "Numero de Variables",
       #y = "Valor de la Metrica",
       color = "Sitio") +
  # theme(
  #        axis.text.x = element_text(size = 1)  
  #      )
theme(legend.position = "none") +

  theme_classic()



################################################################################
################################################################################
################################################################################
#PLOTS
# RLS, Correccion RH, Correcciuon BLH, Ambas


# Datos
desempeño_data <- data.frame(
  Centro = c("SP", "ST", "BA", "MD", "MX"),
  r2_simple = c(2.00E-05,0.01,0.0001,0.12,0.14),
  rmse_simple = c(10.82,16.37,10.64,8.20,9.07),
  
  r2_blh = c(0.01062,0.33424,0.03947,0.03289,0.106),
  rmse_blh = c(10.76,13.44,10.417,8.57,9.412),
  r2_rh = c(0.10,0.05,0.002,0.15,0.27),
  rmse_rh = c(10.288,15.643,10.15,8.18,8.816),
  
  r2_ambas = c(0.15691,0.31548,0.065,0.11149,0.31876),
  rmse_ambas= c(9.94,13.239,9.796,8.377,8.329)
)

# Definir el orden deseado de los centros
desempeñ_data$Centro <- factor(desempeño_data$Centro, levels = c("SP", "ST", "BA", "MD", "MX"))

# Reorganizar datos en formato largo

r2_data <- desempeño_data %>%
  #dplyr::select(Centro, r2_simple, r2_blh,r2_rh,r2_ambas) %>%
  dplyr::select(Centro,rmse_simple, rmse_blh, rmse_rh, rmse_ambas) %>%
  tidyr::pivot_longer(
    #cols = c( r2_simple, r2_blh,r2_rh ,r2_ambas),
    cols = c(rmse_simple,rmse_blh, rmse_rh, rmse_ambas),
    names_to = "Tipo",
    values_to = "RMSE"
    #values_to = "r2"
  )


# Etiquetas mas legibles
r2_data$Tipo <- factor(r2_data$Tipo, 
                       levels = c("r2_simple","r2_blh", "r2_rh", "r2_ambas"),
                       labels = c("R² RLS", "R² BLH","R² RH",  "R² RH+BLH"))
r2_data$Tipo <- factor(r2_data$Tipo, 
                       levels = c("rmse_simple","rmse_blh", "rmse_rh", "rmse_ambas"),
                       labels = c("RMSE RLS","RMSE BLH","RMSE RH",  "RMSE RH+BLH"))

####
# Plots
ggplot(r2_data, aes(x = Centro, y = RMSE, fill = Tipo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    x = "Sitio", y = "RMSE",
    fill = "Tipo de modelo") +
  #scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none") +
  # theme_classic()+scale_fill_manual(values = c( "RÂ² RLS"= "grey",
  #                                               "RÂ² BLH"= "#feb24c",
  #                                               "RÂ² RH" ="#fb6a4a", 
  #                                               "RÂ² RH+BLH" = "#cb181d"))


 theme_classic()+scale_fill_manual(values = c( "RMSE RLS"= "grey",
          "RMSE BLH"= "#feb24c",
                                               "RMSE RH" ="#fb6a4a",
                                               "RMSE RH+BLH" = "#cb181d"))





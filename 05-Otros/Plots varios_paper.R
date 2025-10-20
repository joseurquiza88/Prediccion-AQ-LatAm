

###
#Carpeta
estacion <- "CH"
modelo <- "1"

dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

dirModelo <- (paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))

## Busco archivos
list.files(path = dirModelo, pattern = "XGB", full.names = TRUE)

#model_ML <- "D:/Josefina/Proyectos/Tesis/CH/modelos/01-SVR-M1-170625-CH.RData"
#model_ML <- "D:/Josefina/Proyectos/Tesis/CH/modelos/01-ET-M1-170625-CH.RData"
#model_ML <-"D:/Josefina/Proyectos/Tesis/CH/modelos/01-RF-M1-170625CH.RData"
model_ML <- "D:/Josefina/Proyectos/Tesis/CH/modelos/01-XGB-M1-170625-CH.RData"
load(model_ML)


# Predicciones sobre los datos de entrenamiento
test_data$pred <- predict(modelo_ranger, newdata = test_data)
# Para XGB

#XGB
X_test <- test_data[ ,c( "AOD_055",
                         "ndvi", "BCSMASS_dia","DUSMASS_dia",#"sp_mean",
                         "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                         "d2m_mean", "t2m_mean", "v10_mean",
                         "u10_mean", "tp_mean","DEM",
                         "dayWeek")]#
y_test<- test_data$PM25

dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
# resultados_XGB <- evaluar_modelo(modelo=xgb_model, datos_test=dtest, variable_real = "PM25",tipoModelo="XGB",y_test=y_test)
resultados_XGB

predictions <- predict(xgb_model, newdata = dtest)
test_data$pred <- predictions
plot<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()
plot


library(ggplot2)
library(ggpointdensity)  # si no lo tenés: install.packages("ggpointdensity")

plot <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),     # 🔹 Aumenta tamaño de los valores de ambos ejes
    axis.title = element_text(size = 11)     # 🔹 (opcional) aumenta tamaño de los títulos de ejes
  )+  labs(
    x = " ",   # 🔹 Nombre del eje X
    y = " "     # 🔹 Nombre del eje Y
  ) 

plot

plot_RLS

#####

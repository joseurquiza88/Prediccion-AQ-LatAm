
estacion <- "SP"
data_SP <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-XGB-CV-M1-200525-",estacion,".csv",sep=""))
data_SP$date <- as.Date(as.POSIXct(data_SP$date, format = "%Y-%m-%d"))#


data_SP_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-XGB-CV-1-210525-sAOD-",estacion,".csv",sep=""))
data_SP_sAOD$date <- as.Date(as.POSIXct(data_SP_sAOD$date, format = "%Y-%m-%d"))#


data_SP <- data_SP[year(data_SP$date)==2024,]
data_SP_sAOD <- data_SP_sAOD[year(data_SP_sAOD$date)==2024,]
unique(year(data_SP$date))
unique(year(data_SP_sAOD$date))

data_SP$date <- as.Date(as.POSIXct(data_SP$date, format = "%Y-%m-%d"))#
data_SP_sAOD$date <- as.Date(as.POSIXct(data_SP_sAOD$date, format = "%Y-%m-%d"))#

##### Promedios diarios

promedio_diario_SP <- data_SP %>%
  group_by(date) %>%
  summarise(pm25_predicho_AOD = mean(valor_raster, na.rm = TRUE),
            pm25_observado_AOD = mean(mean, na.rm = TRUE))
nrow(promedio_diario_SP)
promedio_diario_SP_sAOD <- data_SP_sAOD %>%
  group_by(date) %>%
  summarise(pm25_predicho_sAOD = mean(valor_raster, na.rm = TRUE),
            pm25_observado_sAOD = mean(mean, na.rm = TRUE))

promedio_diario_SP <- data_SP
promedio_diario_SP_sAOD <- data_SP_sAOD
nrow(promedio_diario_SP)
nrow(promedio_diario_SP_sAOD)
# data_merged <- left_join(promedio_diario_SP, promedio_diario_SP_sAOD, by = "date")
data_merged <- left_join(promedio_diario_SP, promedio_diario_SP_sAOD, by = c("date","estacion.x"))
unique(year(data_merged$date))

data_merged$pm25_predicho_AOD <- data_merged$valor_raster.x
data_merged$pm25_observado_AOD <- data_merged$mean.x
data_merged$pm25_predicho_sAOD <-data_merged$valor_raster.y
data_merged$pm25_observado_sAOD <- data_merged$mean.y
names (data_merged)
nrow(data_merged)

## Metricas
r2_sAOD <- cor(data_merged$pm25_predicho_sAOD, data_merged$pm25_observado_sAOD)^2
pearson_sAOD <- cor(data_merged$pm25_observado_sAOD, data_merged$pm25_predicho_sAOD, method = "pearson")
rmse_sAOD <- sqrt(mean((data_merged$pm25_predicho_sAOD - data_merged$pm25_observado_sAOD)^2))
bias_sAOD <- mean(data_merged$pm25_predicho_sAOD - data_merged$pm25_observado_sAOD)


r2_AOD <- cor(data_merged$pm25_predicho_AOD, data_merged$pm25_observado_AOD)^2
pearson_AOD <- cor(data_merged$pm25_observado_AOD, data_merged$pm25_predicho_AOD, method = "pearson")
rmse_AOD <- sqrt(mean((data_merged$pm25_predicho_AOD - data_merged$pm25_observado_AOD)^2))
bias_AOD <- mean(data_merged$pm25_predicho_AOD - data_merged$pm25_observado_AOD)

n <- nrow(data_merged)
n
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba"
plot_RLS<- ggplot(data_merged) +
  geom_point(aes(x = pm25_observado_sAOD, y = pm25_predicho_sAOD),color = "#00441b", alpha=0.9,size = 1.5,shape=20) +     # puntos reales vs predicción
  #geom_smooth(aes(x = mean_pm25_sAOD, y = mean_valor_raster_sAOD),method = "lm", se = FALSE, color = "#00441b",linetype = "dashed") +  # ajuste de regresión
  
  geom_point(aes(x = pm25_observado_AOD, y = pm25_predicho_AOD),color = "#238b45", alpha = 0.6,size = 1.5, shape=8) +     # puntos reales vs predicción
  #geom_smooth(aes(x = mean_pm25_AOD, y = mean_valor_raster_AOD),method = "lm", se = FALSE, color = "#238b45",linetype = "dashed") +  # ajuste de regresión
  geom_smooth(aes(x = pm25_observado_sAOD, y = pm25_predicho_sAOD),method = "lm", color = "#00441b", se = FALSE,size = 1, linetype = "dashed") +  # Línea de regresión
  geom_smooth(aes(x = pm25_observado_AOD, y = pm25_predicho_AOD),method = "lm", color = "#238b45", se = FALSE,size = 1.2, linetype = "solid") +  # Línea de regresión
  
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  
  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()+labs(
    x = "Observado",
    y = "Prediccion"
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )+
  #ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 70,label = paste("sAOD"), size = 3, color = "black")+

  ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(r2_sAOD, 2)), size = 3, color = "black")+

  ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(rmse_sAOD, 2)), size = 3, color = "black")+

  ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(bias_sAOD, 2)), size = 3, color = "black")+

  #ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n, 2)), size = 3, color = "black")+


  ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+

  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(r2_AOD, 2)), size = 3, color = "black")+

  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(rmse_AOD, 2)), size = 3, color = "black")+

  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(bias_AOD, 2)), size = 3, color = "black")+

  #ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n, 2)), size = 3, color = "black")+


  theme_classic() #+
plot_RLS



################################################################
################################################################

estacion <- "CH"
data_ST <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-XGB-CV-M1-190625-",estacion,".csv",sep=""))
data_ST$date <- as.Date(as.POSIXct(data_ST$date, format = "%Y-%m-%d"))#


data_ST_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-XGB-CV-M1-230625-sAOD-",estacion,".csv",sep=""))
data_ST_sAOD$date <- as.Date(as.POSIXct(data_ST_sAOD$date, format = "%Y-%m-%d"))#


data_ST <- data_ST[year(data_ST$date)==2024,]
data_ST_sAOD <- data_ST_sAOD[year(data_ST_sAOD$date)==2024,]
unique(year(data_ST$date))
unique(year(data_ST_sAOD$date))

data_ST$date <- as.Date(as.POSIXct(data_ST$date, format = "%Y-%m-%d"))#
data_ST_sAOD$date <- as.Date(as.POSIXct(data_ST_sAOD$date, format = "%Y-%m-%d"))#

##### Promedios diarios
data_ST$mean <- data_ST$Registros.completos
data_ST_sAOD$mean <- data_ST_sAOD$Registros.completos
##### Promedios diarios

promedio_diario_ST <- data_ST %>%
  group_by(date) %>%
  summarise(pm25_predicho_AOD = mean(valor_raster, na.rm = TRUE),
            pm25_observado_AOD = mean(mean, na.rm = TRUE))
nrow(promedio_diario_ST) 
# promedio_diario_ST_sAOD <- data_ST_sAOD %>%
#   group_by(date) %>%
#   summarise(pm25_predicho_sAOD = mean(valor_raster, na.rm = TRUE),
#             pm25_observado_sAOD = mean(mean, na.rm = TRUE))

promedio_diario_ST <- data_ST
promedio_diario_ST_sAOD <- data_ST_sAOD
nrow(promedio_diario_ST)
nrow(promedio_diario_ST_sAOD)
# data_merged <- left_join(promedio_diario_ST, promedio_diario_ST_sAOD, by = "date")
data_merged <- left_join(promedio_diario_ST, promedio_diario_ST_sAOD, by = c("date","estacion.x"))
unique(year(data_merged$date))

data_merged$pm25_predicho_AOD <- data_merged$valor_raster.x
data_merged$pm25_observado_AOD <- data_merged$mean.x
data_merged$pm25_predicho_sAOD <-data_merged$valor_raster.y
data_merged$pm25_observado_sAOD <- data_merged$mean.y
names (data_merged)

summary(data_merged)

data_merged<-data_merged[data_merged$pm25_predicho_sAOD>0,]
data_merged<-data_merged[data_merged$pm25_predicho_AOD>0,]

## Metricas
r2_sAOD <- cor(data_merged$pm25_predicho_sAOD, data_merged$pm25_observado_sAOD)^2
pearson_sAOD <- cor(data_merged$pm25_observado_sAOD, data_merged$pm25_predicho_sAOD, method = "pearson")
rmse_sAOD <- sqrt(mean((data_merged$pm25_predicho_sAOD - data_merged$pm25_observado_sAOD)^2))
bias_sAOD <- mean(data_merged$pm25_predicho_sAOD - data_merged$pm25_observado_sAOD)


r2_AOD <- cor(data_merged$pm25_predicho_AOD, data_merged$pm25_observado_AOD)^2
pearson_AOD <- cor(data_merged$pm25_observado_AOD, data_merged$pm25_predicho_AOD, method = "pearson")
rmse_AOD <- sqrt(mean((data_merged$pm25_predicho_AOD - data_merged$pm25_observado_AOD)^2))
bias_AOD <- mean(data_merged$pm25_predicho_AOD - data_merged$pm25_observado_AOD)

n <- nrow(data_merged)
n
# ST "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba"
plot_RLS<- ggplot(data_merged) +
  geom_point(aes(x = pm25_observado_sAOD, y = pm25_predicho_sAOD),color = "#fc4e2a", alpha=0.9,size = 1.5,shape=20) +     # puntos reales vs predicción
  #geom_smooth(aes(x = mean_pm25_sAOD, y = mean_valor_raster_sAOD),method = "lm", se = FALSE, color = "#00441b",linetype = "dashed") +  # ajuste de regresión
  
  geom_point(aes(x = pm25_observado_AOD, y = pm25_predicho_AOD),color = "#feb24c", alpha = 0.6,size = 1.5, shape=8) +     # puntos reales vs predicción
  #geom_smooth(aes(x = mean_pm25_AOD, y = mean_valor_raster_AOD),method = "lm", se = FALSE, color = "#238b45",linetype = "dashed") +  # ajuste de regresión
  geom_smooth(aes(x = pm25_observado_sAOD, y = pm25_predicho_sAOD),method = "lm", color = "#fc4e2a", se = FALSE,size = 1, linetype = "dashed") +  # Línea de regresión
  geom_smooth(aes(x = pm25_observado_AOD, y = pm25_predicho_AOD),method = "lm", color = "#feb24c", se = FALSE,size = 1.2, linetype = "solid") +  # Línea de regresión
  
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  
  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()+labs(
    x = "Observado",
    y = "Prediccion"
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )+
  #ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 70,label = paste("sAOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(r2_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(rmse_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(bias_sAOD, 2)), size = 3, color = "black")+
  
  #ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n, 2)), size = 3, color = "black")+
  
  
  ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(r2_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(rmse_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(bias_AOD, 2)), size = 3, color = "black")+
  
  #ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n, 2)), size = 3, color = "black")+
  
  
  theme_classic() #+
plot_RLS





################################################################
################################################################

estacion <- "BA"
data_BA <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-170625-",estacion,".csv",sep=""))
data_BA$date <- as.Date(as.POSIXct(data_BA$date, format = "%Y-%m-%d"))#


data_BA_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-ET-CV-M1-230625-sAOD-",estacion,".csv",sep=""))
data_BA_sAOD$date <- as.Date(as.POSIXct(data_BA_sAOD$date, format = "%Y-%m-%d"))#


data_BA <- data_BA[year(data_BA$date)==2024,]
data_BA_sAOD <- data_BA_sAOD[year(data_BA_sAOD$date)==2024,]
unique(year(data_BA$date))
unique(year(data_BA_sAOD$date))

data_BA$date <- as.Date(as.POSIXct(data_BA$date, format = "%Y-%m-%d"))#
data_BA_sAOD$date <- as.Date(as.POSIXct(data_BA_sAOD$date, format = "%Y-%m-%d"))#

##### Promedios diarios

promedio_diario_BA <- data_BA %>%
  group_by(date) %>%
  summarise(pm25_predicho_AOD = mean(valor_raster, na.rm = TRUE),
            pm25_observado_AOD = mean(mean, na.rm = TRUE))
nrow(promedio_diario_BA)
promedio_diario_BA_sAOD <- data_BA_sAOD %>%
  group_by(date) %>%
  summarise(pm25_predicho_sAOD = mean(valor_raBAer, na.rm = TRUE),
            pm25_observado_sAOD = mean(mean, na.rm = TRUE))

promedio_diario_BA <- data_BA
promedio_diario_BA_sAOD <- data_BA_sAOD
nrow(promedio_diario_BA)
nrow(promedio_diario_BA_sAOD)
# data_merged <- left_join(promedio_diario_BA, promedio_diario_BA_sAOD, by = "date")
data_merged <- left_join(promedio_diario_BA, promedio_diario_BA_sAOD, by = c("date","estacion.x"))
unique(year(data_merged$date))

data_merged$pm25_predicho_AOD <- data_merged$valor_raster.x
data_merged$pm25_observado_AOD <- data_merged$mean.x
data_merged$pm25_predicho_sAOD <-data_merged$valor_raster.y
data_merged$pm25_observado_sAOD <- data_merged$mean.y
names (data_merged)

summary(data_merged)

data_merged<-data_merged[data_merged$pm25_predicho_sAOD>0,]
data_merged<-data_merged[data_merged$pm25_predicho_AOD>0,]



## Metricas
r2_sAOD <- cor(data_merged$pm25_predicho_sAOD, data_merged$pm25_observado_sAOD)^2
pearson_sAOD <- cor(data_merged$pm25_observado_sAOD, data_merged$pm25_predicho_sAOD, method = "pearson")
rmse_sAOD <- sqrt(mean((data_merged$pm25_predicho_sAOD - data_merged$pm25_observado_sAOD)^2))
bias_sAOD <- mean(data_merged$pm25_predicho_sAOD - data_merged$pm25_observado_sAOD)


r2_AOD <- cor(data_merged$pm25_predicho_AOD, data_merged$pm25_observado_AOD)^2
pearson_AOD <- cor(data_merged$pm25_observado_AOD, data_merged$pm25_predicho_AOD, method = "pearson")
rmse_AOD <- sqrt(mean((data_merged$pm25_predicho_AOD - data_merged$pm25_observado_AOD)^2))
bias_AOD <- mean(data_merged$pm25_predicho_AOD - data_merged$pm25_observado_AOD)

n <- nrow(data_merged)
n
# BA "#00441b","#238b45"
#BA "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba"
plot_RLS<- ggplot(data_merged) +
  geom_point(aes(x = pm25_observado_sAOD, y = pm25_predicho_sAOD),color = "#99000d", alpha=0.9,size = 1.5,shape=20) +     # puntos reales vs predicción
  #geom_smooth(aes(x = mean_pm25_sAOD, y = mean_valor_raster_sAOD),method = "lm", se = FALSE, color = "#00441b",linetype = "dashed") +  # ajuBAe de regresión
  
  geom_point(aes(x = pm25_observado_AOD, y = pm25_predicho_AOD),color = "#fb6a4a", alpha = 0.6,size = 1.5, shape=8) +     # puntos reales vs predicción
  #geom_smooth(aes(x = mean_pm25_AOD, y = mean_valor_raster_AOD),method = "lm", se = FALSE, color = "#238b45",linetype = "dashed") +  # ajuBAe de regresión
  geom_smooth(aes(x = pm25_observado_sAOD, y = pm25_predicho_sAOD),method = "lm", color = "#99000d", se = FALSE,size = 1, linetype = "dashed") +  # Línea de regresión
  geom_smooth(aes(x = pm25_observado_AOD, y = pm25_predicho_AOD),method = "lm", color = "#fb6a4a", se = FALSE,size = 1.2, linetype = "solid") +  # Línea de regresión
  
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  
  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()+labs(
    x = "Observado",
    y = "Prediccion"
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )+
  #ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 70,label = paste("sAOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(r2_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(rmse_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(bias_sAOD, 2)), size = 3, color = "black")+
  
  #ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n, 2)), size = 3, color = "black")+
  
  
  ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(r2_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(rmse_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(bias_AOD, 2)), size = 3, color = "black")+
  
  #ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n, 2)), size = 3, color = "black")+
  
  
  theme_classic() #+
plot_RLS




################################################################
################################################################

estacion <- "MD"
data_MD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-260525-",estacion,".csv",sep=""))
data_MD$date <- as.Date(as.POSIXct(data_MD$date, format = "%Y-%m-%d"))#


data_MD_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-270525-sAOD-",estacion,".csv",sep=""))
data_MD_sAOD$date <- as.Date(as.POSIXct(data_MD_sAOD$date, format = "%Y-%m-%d"))#


data_MD <- data_MD[year(data_MD$date)==2024,]
data_MD_sAOD <- data_MD_sAOD[year(data_MD_sAOD$date)==2024,]
unique(year(data_MD$date))
unique(year(data_MD_sAOD$date))

data_MD$date <- as.Date(as.POSIXct(data_MD$date, format = "%Y-%m-%d"))#
data_MD_sAOD$date <- as.Date(as.POSIXct(data_MD_sAOD$date, format = "%Y-%m-%d"))#

##### Promedios diarios

promedio_diario_MD <- data_MD %>%
  group_by(date) %>%
  summarise(pm25_predicho_AOD = mean(valor_raster, na.rm = TRUE),
            pm25_observado_AOD = mean(mean, na.rm = TRUE))
nrow(promedio_diario_MD)
# promedio_diario_ST_sAOD <- data_ST_sAOD %>%
#   group_by(date) %>%
#   summarise(pm25_predicho_sAOD = mean(valor_raster, na.rm = TRUE),
#             pm25_observado_sAOD = mean(mean, na.rm = TRUE))

promedio_diario_MD <- data_MD
promedio_diario_MD_sAOD <- data_MD_sAOD
nrow(promedio_diario_MD)
nrow(promedio_diario_MD_sAOD)
# data_merged <- left_join(promedio_diario_MD, promedio_diario_MD_sAOD, by = "date")
data_merged <- left_join(promedio_diario_MD, promedio_diario_MD_sAOD, by = c("date","estacion.x"))
unique(year(data_merged$date))

data_merged$pm25_predicho_AOD <- data_merged$valor_raster.x
data_merged$pm25_observado_AOD <- data_merged$mean.x
data_merged$pm25_predicho_sAOD <-data_merged$valor_raster.y
data_merged$pm25_observado_sAOD <- data_merged$mean.y
names (data_merged)

summary(data_merged)

data_merged<-data_merged[data_merged$pm25_predicho_sAOD>0,]
data_merged<-data_merged[data_merged$pm25_predicho_AOD>0,]



## Metricas
r2_sAOD <- cor(data_merged$pm25_predicho_sAOD, data_merged$pm25_observado_sAOD)^2
pearson_sAOD <- cor(data_merged$pm25_observado_sAOD, data_merged$pm25_predicho_sAOD, method = "pearson")
rmse_sAOD <- sqrt(mean((data_merged$pm25_predicho_sAOD - data_merged$pm25_observado_sAOD)^2))
bias_sAOD <- mean(data_merged$pm25_predicho_sAOD - data_merged$pm25_observado_sAOD)


r2_AOD <- cor(data_merged$pm25_predicho_AOD, data_merged$pm25_observado_AOD)^2
pearson_AOD <- cor(data_merged$pm25_observado_AOD, data_merged$pm25_predicho_AOD, method = "pearson")
rmse_AOD <- sqrt(mean((data_merged$pm25_predicho_AOD - data_merged$pm25_observado_AOD)^2))
bias_AOD <- mean(data_merged$pm25_predicho_AOD - data_merged$pm25_observado_AOD)

n <- nrow(data_merged)
n
# MD "#00441b","#238b45"
#MD "#fc4e2a",  "#feb24c",
# MD  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dMD"
plot_RLS<- ggplot(data_merged) +
  geom_point(aes(x = pm25_observado_sAOD, y = pm25_predicho_sAOD),color = "#023858", alpha=0.9,size = 1.5,shape=20) +     # puntos reales vs predicción
  #geom_smooth(aes(x = mean_pm25_sAOD, y = mean_valor_raster_sAOD),method = "lm", se = FALSE, color = "#00441b",linetype = "dashed") +  # ajuMDe de regresión
  
  geom_point(aes(x = pm25_observado_AOD, y = pm25_predicho_AOD),color = "#4292c6", alpha = 0.6,size = 1.5, shape=8) +     # puntos reales vs predicción
  #geom_smooth(aes(x = mean_pm25_AOD, y = mean_valor_raster_AOD),method = "lm", se = FALSE, color = "#238b45",linetype = "dashed") +  # ajuMDe de regresión
  geom_smooth(aes(x = pm25_observado_sAOD, y = pm25_predicho_sAOD),method = "lm", color = "#023858", se = FALSE,size = 1, linetype = "dashed") +  # Línea de regresión
  geom_smooth(aes(x = pm25_observado_AOD, y = pm25_predicho_AOD),method = "lm", color = "#4292c6", se = FALSE,size = 1.2, linetype = "solid") +  # Línea de regresión
  
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  
  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()+labs(
    x = "Observado",
    y = "Prediccion"
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )+
  #ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 70,label = paste("sAOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(r2_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(rmse_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(bias_sAOD, 2)), size = 3, color = "black")+
  
  #ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n, 2)), size = 3, color = "black")+
  
  
  ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(r2_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(rmse_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(bias_AOD, 2)), size = 3, color = "black")+
  
  #ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n, 2)), size = 3, color = "black")+
  
  
  theme_classic() #+
plot_RLS



################################################################
################################################################

estacion <- "MX"
data_MX <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-XGB-CV-M1-290525-",estacion,".csv",sep=""))
data_MX$date <- as.Date(as.POSIXct(data_MX$date, format = "%Y-%m-%d"))#


data_MX_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-XGB-CV-M1-230625-sAOD-",estacion,".csv",sep=""))
data_MX_sAOD$date <- as.Date(as.POSIXct(data_MX_sAOD$date, format = "%Y-%m-%d"))#


data_MX <- data_MX[year(data_MX$date)==2024,]
data_MX_sAOD <- data_MX_sAOD[year(data_MX_sAOD$date)==2024,]
unique(year(data_MX$date))
unique(year(data_MX_sAOD$date))

data_MX$date <- as.Date(as.POSIXct(data_MX$date, format = "%Y-%m-%d"))#
data_MX_sAOD$date <- as.Date(as.POSIXct(data_MX_sAOD$date, format = "%Y-%m-%d"))#

##### Promedios diarios

promedio_diario_MX <- data_MX %>%
  group_by(date) %>%
  summarise(pm25_predicho_AOD = mean(valor_raster, na.rm = TRUE),
            pm25_observado_AOD = mean(mean, na.rm = TRUE))
nrow(promedio_diario_MX)

promedio_diario_MX_sAOD <- data_MX_sAOD %>%
  group_by(date) %>%
  summarise(pm25_predicho_sAOD = mean(valor_raster, na.rm = TRUE),
            pm25_observado_sAOD = mean(mean, na.rm = TRUE))

promedio_diario_MX <- data_MX
promedio_diario_MX_sAOD <- data_MX_sAOD
nrow(promedio_diario_MX)
nrow(promedio_diario_MX_sAOD)
# data_merged <- left_join(promedio_diario_MX, promedio_diario_MX_sAOD, by = "date")
data_merged <- left_join(promedio_diario_MX, promedio_diario_MX_sAOD, by = c("date","estacion.x"))
unique(year(data_merged$date))

data_merged$pm25_predicho_AOD <- data_merged$valor_raster.x
data_merged$pm25_observado_AOD <- data_merged$mean.x
data_merged$pm25_predicho_sAOD <-data_merged$valor_raster.y
data_merged$pm25_observado_sAOD <- data_merged$mean.y
names (data_merged)

summary(data_merged)

data_merged<-data_merged[data_merged$pm25_predicho_sAOD>0,]
data_merged<-data_merged[data_merged$pm25_predicho_AOD>0,]



## Metricas
r2_sAOD <- cor(data_merged$pm25_predicho_sAOD, data_merged$pm25_observado_sAOD)^2
pearson_sAOD <- cor(data_merged$pm25_observado_sAOD, data_merged$pm25_predicho_sAOD, method = "pearson")
rmse_sAOD <- sqrt(mean((data_merged$pm25_predicho_sAOD - data_merged$pm25_observado_sAOD)^2))
bias_sAOD <- mean(data_merged$pm25_predicho_sAOD - data_merged$pm25_observado_sAOD)


r2_AOD <- cor(data_merged$pm25_predicho_AOD, data_merged$pm25_observado_AOD)^2
pearson_AOD <- cor(data_merged$pm25_observado_AOD, data_merged$pm25_predicho_AOD, method = "pearson")
rmse_AOD <- sqrt(mean((data_merged$pm25_predicho_AOD - data_merged$pm25_observado_AOD)^2))
bias_AOD <- mean(data_merged$pm25_predicho_AOD - data_merged$pm25_observado_AOD)

n <- nrow(data_merged)
n
# MX "#00441b","#238b45"
#MX "#fc4e2a",  "#feb24c",
# MX  "#99000d"  "#fb6a4a",
#MX "#023858", "#4292c6"
# MX "#3f007d", "#807dba"
plot_RLS<- ggplot(data_merged) +
  geom_point(aes(x = pm25_observado_sAOD, y = pm25_predicho_sAOD),color = "#3f007d", alpha=0.9,size = 1.5,shape=20) +     # puntos reales vs predicción
  #geom_smooth(aes(x = mean_pm25_sAOD, y = mean_valor_raster_sAOD),method = "lm", se = FALSE, color = "#00441b",linetype = "dashed") +  # ajuMXe de regresión
  
  geom_point(aes(x = pm25_observado_AOD, y = pm25_predicho_AOD),color = "#807dba", alpha = 0.6,size = 1.5, shape=8) +     # puntos reales vs predicción
  #geom_smooth(aes(x = mean_pm25_AOD, y = mean_valor_raster_AOD),method = "lm", se = FALSE, color = "#238b45",linetype = "dashed") +  # ajuMXe de regresión
  geom_smooth(aes(x = pm25_observado_sAOD, y = pm25_predicho_sAOD),method = "lm", color = "#3f007d", se = FALSE,size = 1, linetype = "dashed") +  # Línea de regresión
  geom_smooth(aes(x = pm25_observado_AOD, y = pm25_predicho_AOD),method = "lm", color = "#807dba", se = FALSE,size = 1.2, linetype = "solid") +  # Línea de regresión
  
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  
  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()+labs(
    x = "Observado",
    y = "Prediccion"
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )+
  #ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 70,label = paste("sAOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(r2_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(rmse_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(bias_sAOD, 2)), size = 3, color = "black")+
  
  #ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n, 2)), size = 3, color = "black")+
  
  
  ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(r2_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(rmse_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(bias_AOD, 2)), size = 3, color = "black")+
  
  #ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n, 2)), size = 3, color = "black")+
  
  
  theme_classic() #+
plot_RLS



##########################################
# faltante de datos

# Cargar paquete
library(ggplot2)

# Crear el dataframe
datos <- data.frame(
  Sitio = c("SP", "ST", "BA", "MD", "MX"),
  Porcentaje = c(63, 72, 74, 40, 73)
)

# Crear el gráfico con colores personalizados
ggplot(datos, aes(x = reorder(Sitio, -Porcentaje), y = Porcentaje, fill = Sitio)) +
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE, alpha=0.4) +
  geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5, size = 5, color = "black") +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c(
    "SP" = "#00441b",
    "ST" = "#fc4e2a",
    "BA" = "#99000d",
    "MD" = "#023858",
    "MX" = "#3f007d"
  )) +
  labs(
    #title = "Disponibilidad de datos PM₂.₅ en 2024",
    #subtitle = "Porcentaje de días con datos reales por sitio",
    x = " ",
    y = "Disponibilidad (%)"#"Disponibilidad de datos en 2024"
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )


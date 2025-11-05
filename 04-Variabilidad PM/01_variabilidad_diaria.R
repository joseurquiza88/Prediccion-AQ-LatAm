
#######################################################################
## OBJETIVO: Analisis de las predicciones obtenidas de los mapas diarios
##
#######################################################################

## Prediccion diaria con/sin AOD
estacion <- "SP"
data_SP <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-XGB-CV-M1-200525-",estacion,".csv",sep=""))
data_SP$date <- as.Date(as.POSIXct(data_SP$date, format = "%Y-%m-%d"))#


data_SP_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-XGB-CV-1-210525-sAOD-",estacion,".csv",sep=""))
data_SP_sAOD$date <- as.Date(as.POSIXct(data_SP_sAOD$date, format = "%Y-%m-%d"))#

# Solo analizamos el perido no visto del 2024
data_SP <- data_SP[year(data_SP$date)==2024,]
data_SP_sAOD <- data_SP_sAOD[year(data_SP_sAOD$date)==2024,]
# Corroboramos aÒo 2024
unique(year(data_SP$date))
unique(year(data_SP_sAOD$date))

#######################
# Promedios diarios con AOD
data_AOD_diario_SP <- data_SP %>%
  group_by(date) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )
# Promedios diarios sin AOD
data_sAOD_diario_SP <- data_SP_sAOD %>%
 group_by(date) %>%
  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

# Seteamos la fecha
data_AOD_diario_SP$date <- as.Date(as.POSIXct(data_AOD_diario_SP$date, format = "%Y-%m-%d"))#
data_sAOD_diario_SP$date <- as.Date(as.POSIXct(data_sAOD_diario_SP$date, format = "%Y-%m-%d"))#
# corroboramos por las dudas el aÒo
data_AOD_diario_SP <- data_AOD_diario_SP[year(data_AOD_diario_SP$date)==2024,]
data_sAOD_diario_SP <- data_sAOD_diario_SP[year(data_sAOD_diario_SP$date)==2024,]

# Hacemos modelos con/sin AOD segun la fecha
data_merged <- left_join(data_sAOD_diario_SP, data_AOD_diario_SP, by = "date")
unique(year(data_merged$date))


# Transformamos el dataset
data_long_SP <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_sAOD),#mean_valor_raster_AOD,
    names_to = "variable",
    values_to = "valor"
  )

### Plot, serie temporal con ambos datos
# No se ve mucho no queda lindo

#Oscuro Prediccion - Claro con mediccion
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",
library(scales)  # por si necesit√°s formatos personalizados
#Oscuro SAOD - Claro con AOD
 ggplot(data_long_SP, aes(x = date, y = valor, color = variable)) +

  geom_line(size = 0.5) +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "#66c2a4",    
      mean_valor_raster_sAOD = "#00441b"
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_sAOD = "Predicci√≥n"
    )
  ) +
  # scale_x_date(
  #   date_breaks = "2 month",           # un tick por mes
  #   date_labels = "%B",                # formato: "ene", "feb", etc.
  #   limits = as.Date(c("2024-01-01", "2024-12-31"))  # llimites deleje
  # ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)  
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    #legend.position = "none",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
 
################
## metricas etadisticas de los datos medidos y las prediccion
 #Con AOD
mean(data_AOD_diario_SP$mean_pm25_AOD)
sd(data_AOD_diario_SP$mean_pm25_AOD)
mean(data_AOD_diario_SP$mean_valor_raster_AOD)
sd(data_AOD_diario_SP$mean_valor_raster_AOD)
#####
#Sin AOD
mean(data_sAOD_diario_SP$mean_pm25_sAOD)
sd(data_sAOD_diario_SP$mean_pm25_sAOD)
mean(data_sAOD_diario_SP$mean_valor_raster_sAOD)
sd(data_sAOD_diario_SP$mean_valor_raster_sAOD)

# Resumen de Metrica general
summary(data_AOD_diario_SP$mean_pm25_AOD)
summary(data_AOD_diario_SP$mean_valor_raster_AOD)

# Corroboramos que los datos completos y bno haya nans
data_merged2 <-data_merged[complete.cases(data_merged),]
# Volvemos a hacer las metricas para corroborar
# Medicion
mean(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
sd(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
#con AOD
mean(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
#Saod
mean(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)



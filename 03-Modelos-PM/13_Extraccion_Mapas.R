#######################################################################
## OBJETIVO: A partir de los mapas (diarios, mensuales, anuales),
# se extraen los datos en los sitios(estciones de monitoreo) de interes
# segun el centro urbano
##
#######################################################################
estacion <- "BA"
modelo <- "01-ET-CV-M1-170625-BA"

#Directorio donde se encuentran todas las imagenes
#dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasDiarias/",modelo,"/",year,"/",sep="")
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasDiarias/",modelo,"/",sep="")
#dir <- "D:/Josefina/Proyectos/ProyectoChile/CH/modelos/Salidas/SalidasMensuales/01-XGB-CV-M1-190625-CH"
setwd(dir)
id <- list.files(path = dir,
                 pattern = "*.tif",
                 full.names = FALSE)
# archivo csv generado manualmente donde se encuentra las coordenadas
# de cada estacion de monitoreo
puntos <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))
puntos <- puntos[puntos$Considerado=="SI",]
#puntos <- puntos[puntos$tipo=="referencia",]
# Corroramos el numero de estaciones
nrow(puntos)

crs_project <- "+proj=longlat +datum=WGS84"
df_rbind <- data.frame()

i<-1
# Recorremos directorio donde se encuentrn las imagenes
for (i in 1:length(id)){
  print(i)
  pred_raster <- raster(id[i]) # abrimos con formato raster
  
  #plot(pred_raster)
  # Extraer los valores del raster en las coordenadas especificadas
  valores_raster <- extract(pred_raster, puntos[, c("long", "lat")])
  
  # Unir los valores del raster al dataframe original
  puntos_con_valores <- puntos %>%
    mutate(valor_raster = valores_raster)
  #Dia
  fechaInteres <- as.Date(substr(id[i],4,13), format = "%Y-%m-%d")# Mostrar el dataframe resultante
 #mes
  #fechaInteres <- substr(id[i],9,16)
  puntos_con_valores$date <- fechaInteres
  
  df_rbind <- rbind(df_rbind,puntos_con_valores)
}
 
write.csv(df_rbind, paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/Comparativas_resultados/PM_modelado/data_PM-Modelado-TOT_",estacion,".csv",sep=""))
###########################################################
# Al data set anterior lo quiero unir con las mediciones reales
# para ver que tan bien se hicieron las predicciones

# Mediciones reales en las estaciones de monitoreo de PM2.5 
#Ponemos en formato
data_sensores <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/06_estaciones/",estacion,"_estaciones.csv",sep=""))
data_sensores <- data_sensores[complete.cases(data_sensores$date),]
data_sensores$date <- as.Date(as.POSIXct(data_sensores$date, format = "%d/%m/%Y"))#"%Y-%m-%d"))#

# #Ponemos en formato el dataset de las predicciones
df_rbind$date <- as.Date(as.POSIXct(df_rbind$date, format = "%Y-%m-%d"))#
# vemos las variables
names(data_sensores)
names(df_rbind)
unique(df_rbind$ID)
unique(data_sensores$ID)

# Hacemos un merge entre las mediciones reales y las predicciones
# Unimos segun dia y estacion
merged_df <- merge(df_rbind,data_sensores, by = c("ID", "date"), all.x = TRUE)
# Descartamos datos que no coinciden
# Cuantos son los datos descartados?
merged_df_subt <- merged_df[complete.cases(merged_df$mean),]
merged_df_subt <- merged_df[complete.cases(merged_df$Registros.completos),]
merged_df_subt <- merged_df_subt[complete.cases(merged_df_subt$valor_raster),]

# Solo nos quedamos con los datos de l 2024 para hacer una validacion independiente
# Ya que el modelos se entreno/testeo sin estos datos
merged_df_subt2 <- merged_df_subt[year(merged_df_subt$date) ==2024,]
merged_df_subt2 <- merged_df_subt[year(merged_df_subt$date) !=2024,]
merged_df_subt2 <- merged_df_subt2[year(merged_df_subt2$date) !=2023,]
merged_df_subt2$mean <-merged_df_subt2$Registros.completos #solo ST
nrow(merged_df_subt2)





#Evaluamos el desempeño todos los valores
model <- lm(mean~valor_raster , data = merged_df_subt2)

# Calculo de metricas de desempeño
R2 <- summary(model)$r.squared
RMSE <- sqrt(mean(residuals(model)^2))
Bias <- mean(merged_df_subt2$mean - merged_df_subt2$valor_raster)
n <- nrow(merged_df_subt2)
df_metrica <- data.frame(R2,RMSE,Bias,n)
df_metrica
#write.csv(merged_df_subt,paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-260525-MD.csv",sep=""))
#######
# Otra evaluacion del desempeño pero condatos diarios globales
# Hacemos media diaria considerando todas las estaciones juntas
merged_df_subt2$date <- as.Date(merged_df_subt2$date)
#agrupamos por dia
df_diario <- merged_df_subt2 %>%
  group_by(date) %>%
  summarise(
    mean_prediccion = mean(valor_raster, na.rm = TRUE),
    mean_medicion= mean(mean, na.rm = TRUE)
  )

# Modelos 2 con datos diarios
model_v2 <- lm( mean_medicion~mean_prediccion , data = df_diario)
# Calculo de metricas de desempeño
R2_v2 <- summary(model_v2)$r.squared
RMSE_v2 <- sqrt(mean(residuals(model_v2)^2))
Bias_v2 <- mean(df_diario$mean_medicion - df_diario$mean_prediccion)
n_v2 <- nrow(df_diario)
df_metrica_v2 <- data.frame(R2_v2,RMSE_v2,Bias_v2,n_v2)
df_metrica_v2

###########3
####Plot comparando ambos datos
ggplot(df_diario, aes(x = date)) +

  geom_line(aes(y = mean_medicion, color = "Monitoreo"), size = 0.8,na.rm = TRUE) +
  
  geom_line(aes(y = mean_prediccion, color = "Modelo"), size = 0.8, na.rm = TRUE)+#, linetype = "dashed") +
 # regesion por estacion de monitoreo
  # Separar en subplots por estacion
  #facet_wrap(~ ID , scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 120),breaks = seq(0, 120, by = 40)) +  # Ticks cada 10 en el eje Y
  
  labs(title = modelo,
       x = "",
       y = "PM2.5",
       color = "Variables") +
  scale_color_manual(values = c("Monitoreo" = "#2ca25f", "Modelo" = "#feb24c"),#,"Monitoreo"="blue"),
                     labels = c("Monitoreo" = "Monitoreo", "Modelo" = "Modelo"))+#, "mean"="Monitoreo")) +

  theme_classic() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),  
    axis.title.x = element_text(size = 8),             
    axis.title.y = element_text(size = 8),              
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 6),               
    strip.text = element_text(size = 5),                
    legend.title = element_text(size = 8),              
    legend.text = element_text(size = 5)               
  )


# Crear columna mes-aÃ±o en formato YYYY-MM
data_sensores_mes <- data_sensores %>%
  mutate(
    mes_anio = format(date, "%m-%Y"),
    date = format(date, "%m-%Y")
  )

#####################################################################
#####################################################################
### Otras estadisticas
# --- Promedio mensual por estacion ---
promedio_mensual <- data_sensores_mes %>%
  group_by(estacion, mes_anio,date) %>%
  summarise(
    media_registros = mean(Registros.completos, na.rm = TRUE),
    .groups = "drop"
  )
#########
# --- Promedio mensual general (todas las estaciones) ---
promedio_mensual_general <- data_sensores %>%
  group_by(mes_anio) %>%
  summarise(
    media_registros = mean(Registros.completos, na.rm = TRUE),
    .groups = "drop"
  )

#
df_merge <- df_rbind %>%
  left_join(promedio_mensual, by = c("estacion", "date"))

# Guardar para revisar manualmente
write.csv(df_merge, paste(dir,"/1-XGB-CV-M1-190625-CH_merge.csv",sep=""))


##########################################################
##########################################################
##########################################################
##########################################################
# Codigo relacionado con 03_Comparativa_ModelosGlobales 
#relacionado a la comparativa 
# con el modelo WUSTL

# Se hace un merge entre las mediciones reales, 
# las obtenidas con mi modelo y las del modelo global para comparalas

#
df <- read.csv("D:/Josefina/Proyectos/ProyectoChile/CH/Comparativas_resultados/PM_wustl/data_PM-WUSTL-TOT_comparativa XGB_CV_1-190625.csv")

df_complete <- df[complete.cases(df$media_registros),]

## Mediciones reales vs mi modelo
model_v2 <- lm( media_registros~valor_raster_.1.XGB.CV.M1.190625.CH_merge , data = df_complete)
R2_v2 <- summary(model_v2)$r.squared
RMSE_v2 <- sqrt(mean(residuals(model_v2)^2))
Bias_v2 <- mean(df_complete$media_registros - df_complete$valor_raster_.1.XGB.CV.M1.190625.CH_merge)
n_v2 <- nrow(df_complete)
df_metrica_v2 <- data.frame(R2_v2,RMSE_v2,Bias_v2,n_v2)
df_metrica_v2

## Mediciones reales vs modelo WUSTL
model_v3 <- lm( media_registros~ extracted_values_WUSTL_TOT, data = df_complete)
# Calculo de mÃ©tricas de desempeÃ±o
R2_v3 <- summary(model_v3)$r.squared
RMSE_v3 <- sqrt(mean(residuals(model_v3)^2))
Bias_v3 <- mean(df_complete$media_registros - df_complete$extracted_values_WUSTL_TOT)
n_v3<- nrow(df_complete)
df_metrica_v3 <- data.frame(R2_v3,RMSE_v3,Bias_v3,n_v3)
df_metrica_v3

######
## Mi modelo vs WUSTL
model_v4 <- lm( valor_raster_.1.XGB.CV.M1.190625.CH_merge~ extracted_values_WUSTL_TOT, data = df_complete)
R2_v4 <- summary(model_v4)$r.squared
RMSE_v4 <- sqrt(mean(residuals(model_v4)^2))
Bias_v4 <- mean(df_complete$valor_raster_.1.XGB.CV.M1.190625.CH_merge - df_complete$extracted_values_WUSTL_TOT)
n_v4<- nrow(df_complete)
df_metrica_v4 <- data.frame(R2_v4,RMSE_v4,Bias_v4,n_v4)
df_metrica_v4

##########################################
## Plots de dispersion Mi modelo vs WUSTL

library(ggplot2)
library(ggpointdensity)  

plot_RLS <- ggplot(df_complete, aes(x = extracted_values_WUSTL_TOT, y = valor_raster_.1.XGB.CV.M1.190625.CH_merge)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
  theme_classic()+ 
 
  theme(
    #legend.position = "none",
    axis.text = element_text(size = 14),     
    axis.title = element_text(size = 11)     #
  )+  labs(
    x = " ",   
    y = " "     
  ) 

plot_RLS


##########################################
## Plots de dispersion Mediciones vs WUSTL
plot_RLS <- ggplot(df_complete, aes(y = extracted_values_WUSTL_TOT, x = media_registros)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  
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


##########################################
## Plots de dispersion  comparativa entre modelos

# Reestructuramos el dataframe a formato largo
df_long <- df_complete %>%
  pivot_longer(
    cols = c(extracted_values_WUSTL_TOT, valor_raster_.1.XGB.CV.M1.190625.CH_merge),
    names_to = "variable",
    values_to = "valor_y"
  )

# Graficamos ambas relaciones
plot_RLS <- ggplot(df_long, aes(x = media_registros, y = valor_y, color = variable)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  
  geom_abline(slope = 1, intercept = 0, color = "black") +       
  scale_color_manual(values = c("blue", "red")) +                
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 11)
  ) +
  labs(
    x = "media_registros",
    y = "Valores",
    color = "Variable"
  )  +theme(
    legend.position = "none"#,   
    #legend.title = element_blank(), 
    # para eliminar completamente la leyenda, usar: legend.position = "none"
  )

plot_RLS

#
###############################################
###############################################
# otro plot

# tructuramos el dataframe a formato largo
df_long <- df_complete %>%
  pivot_longer(
    cols = c(extracted_values_WUSTL_TOT, valor_raster_.1.XGB.CV.M1.190625.CH_merge),
    names_to = "variable",
    values_to = "valor_y"
  )

# Graficamos
plot_RLS <- ggplot(df_long, aes(x = media_registros, y = valor_y)) +
  geom_point(aes(color = variable), alpha = 0.8, size = 1) +
  geom_smooth(
    data = subset(df_long, variable == "extracted_values_WUSTL_TOT"),
    method = "lm", se = FALSE, color = "#045a8d", linetype = "solid", size = 1.2
  ) +
  geom_smooth(
    data = subset(df_long, variable == "valor_raster_.1.XGB.CV.M1.190625.CH_merge"),
    method = "lm", se = FALSE, color = "#006d2c", linetype = "solid", size = 1.2
  ) +
  #Linea 1:
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8) +
  # Colores y etiquetas
  scale_color_manual(
    values = c("#2b8cbe", "#2ca25f"),
    labels = c("V5GL04", "This model"),
    name = NULL   
  ) +
 
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 40)) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 40)) +
  
  theme_classic() +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 9),        
    legend.position = c(0.15, 0.8),                
    #legend.background = element_rect(fill = "white", color = "gray80")
  ) +
  labs(
    x = " ",
    y = " "
  )

plot_RLS


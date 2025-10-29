##############################################################################
##otra forma de extraer los datos
estacion <- "CH"
#modelo <- "01-XGB-CV-M1-200525-SP"
# modelo <- "02-XGB-CV-1-210525-sAOD-SP" 
#modelo <- "01-XGB-CV-M1-200525-MERRA-Combinado-SP"

#modelo <- "01-XGB-CV-M1-190625-CH"
#modelo <- "02-XGB-CV-M1-230625-sAOD-CH"
#modelo <- "01-XGB-CV-M1-190625-MERRA-Combinado-CH"

#modelo <- "01-ET-CV-M1-170625-BA"
#modelo <- "02-ET-CV-M1-230625-sAOD-BA"
#modelo <- "01-ET-CV-M1-170625-Combinado-BA"

#modelo <- "01-ET-CV-M1-260525-MD"
#modelo <- "01-ET-CV-M1-270525-sAOD-MD"
# modelo <- "01-ET-CV-M1-260525-Combinado-MD"
# 
#modelo <- "01-XGB-CV-M1-290525-MX"
#modelo <- "02-XGB-CV-M1-230625-sAOD-MX"
modelo <- "01-RF-CV-M1-170625-CH_combinado"

#dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasDiarias/",modelo,"/",year,"/",sep="")
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasDiarias/",modelo,"/",sep="")
dir <- "D:/Josefina/Proyectos/ProyectoChile/CH/modelos/Salidas/SalidasMensuales/01-XGB-CV-M1-190625-CH"
setwd(dir)
id <- list.files(path = dir,
                 pattern = "*.tif",
                 full.names = FALSE)

data_estacciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))
data_estacciones <- data_estacciones[data_estacciones$Considerado=="SI",]
data_estacciones <- data_estacciones[data_estacciones$tipo=="referencia",]
nrow(data_estacciones)
puntos <- data_estacciones


crs_project <- "+proj=longlat +datum=WGS84"
df_rbind <- data.frame()

i<-1
for (i in 1:length(id)){
  print(i)
  pred_raster <- raster(id[i])
  
  #plot(pred_raster)
  # Extraer los valores del raster en las coordenadas especificadas
  valores_raster <- extract(pred_raster, puntos[, c("long", "lat")])
  
  # Unir los valores del raster al dataframe original
  puntos_con_valores <- puntos %>%
    mutate(valor_raster = valores_raster)
  #Dia
  # fechaInteres <- as.Date(substr(id[i],4,13), format = "%Y-%m-%d")# Mostrar el dataframe resultante
 #mes
  fechaInteres <- substr(id[i],9,15)
  puntos_con_valores$date <- fechaInteres
  
  df_rbind <- rbind(df_rbind,puntos_con_valores)
}

data_sensores <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/06_estaciones/",estacion,"_estaciones.csv",sep=""))
data_sensores <- data_sensores[complete.cases(data_sensores$date),]
data_sensores$date <- as.Date(as.POSIXct(data_sensores$date, format = "%d/%m/%Y"))#"%Y-%m-%d"))#
df_rbind$date <- as.Date(as.POSIXct(df_rbind$date, format = "%Y-%m-%d"))#
# vemos las variabñes
names(data_sensores)
names(df_rbind)
unique(df_rbind$ID)
unique(data_sensores$ID)
# merge
merged_df <- merge(df_rbind,data_sensores, by = c("ID", "date"), all.x = TRUE)
merged_df_subt <- merged_df[complete.cases(merged_df$mean),]
merged_df_subt <- merged_df[complete.cases(merged_df$Registros.completos),]

merged_df_subt <- merged_df_subt[complete.cases(merged_df_subt$valor_raster),]

# merged_df_subt2 <- merged_df_subt[year(merged_df_subt$date) !=2024,]
merged_df_subt2 <- merged_df_subt[year(merged_df_subt$date) ==2024,]
merged_df_subt2$mean <-merged_df_subt2$Registros.completos
nrow(merged_df_subt2)
model <- lm(mean~valor_raster , data = merged_df_subt2)

# Calculo de métricas de desempeño
R2 <- summary(model)$r.squared
RMSE <- sqrt(mean(residuals(model)^2))
Bias <- mean(merged_df_subt2$mean - merged_df_subt2$valor_raster)
n <- nrow(merged_df_subt2)
df_metrica <- data.frame(R2,RMSE,Bias,n)
df_metrica
#write.csv(merged_df_subt,paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-260525-MD.csv",sep=""))
#write.csv(merged_df_subt,paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_",modelo,".csv",sep=""))

merged_df_subt2$date <- as.Date(merged_df_subt2$date)
df_diario <- merged_df_subt2 %>%
  group_by(date) %>%
  summarise(
    mean_prediccion = mean(valor_raster, na.rm = TRUE),
    mean_medicion= mean(mean, na.rm = TRUE)
  )


model_v2 <- lm( mean_medicion~mean_prediccion , data = df_diario)
# Calculo de métricas de desempeño
R2_v2 <- summary(model_v2)$r.squared
RMSE_v2 <- sqrt(mean(residuals(model_v2)^2))
Bias_v2 <- mean(df_diario$mean_medicion - df_diario$mean_prediccion)
n_v2 <- nrow(df_diario)
df_metrica_v2 <- data.frame(R2_v2,RMSE_v2,Bias_v2,n_v2)
df_metrica_v2
####PLOT
ggplot(df_diario, aes(x = date)) +
  #ggplot(df_date_rbind, aes(x = date)) +
  # Línea para Registros.validados
  # Línea para valor_Raster
  geom_line(aes(y = mean_medicion, color = "Monitoreo"), size = 0.8,na.rm = TRUE) +
  
  geom_line(aes(y = mean_prediccion, color = "Modelo"), size = 0.8, na.rm = TRUE)+#, linetype = "dashed") +
  #geom_line(aes(y = valor_raster.y, color = "MERRA-2"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  #facet_wrap(~ ID , scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 120),breaks = seq(0, 120, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = modelo,
       x = "",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("Monitoreo" = "#2ca25f", "Modelo" = "#feb24c"),#,"Monitoreo"="blue"),
                     labels = c("Monitoreo" = "Monitoreo", "Modelo" = "Modelo"))+#, "mean"="Monitoreo")) +
  
  # Personalización del tema
  
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),  # Tamaño y alineación del título
    axis.title.x = element_text(size = 8),              # Tamaño del título del eje X
    axis.title.y = element_text(size = 8),              # Tamaño del título del eje Y
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1), # Tamaño y rotación de los ticks del eje X
    axis.text.y = element_text(size = 6),               # Tamaño de los ticks del eje Y
    strip.text = element_text(size = 5),                # Tamaño del texto de los subplots
    legend.title = element_text(size = 8),              # Tamaño del título de la leyenda
    legend.text = element_text(size = 5)                # Tamaño del texto de la leyenda
  )


# Crear columna mes-año en formato YYYY-MM
data_sensores_mes <- data_sensores %>%
  mutate(
    mes_anio = format(date, "%m-%Y"),
    date = format(date, "%m-%Y")
  )

# --- Promedio mensual por estación ---
promedio_mensual <- data_sensores_mes %>%
  group_by(estacion, mes_anio,date) %>%
  summarise(
    media_registros = mean(Registros.completos, na.rm = TRUE),
    .groups = "drop"
  )

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


write.csv(df_merge, paste(dir,"/1-XGB-CV-M1-190625-CH_merge.csv",sep=""))
#############################
df <- read.csv("D:/Josefina/Proyectos/ProyectoChile/CH/Comparativas_resultados/PM_wustl/data_PM-WUSTL-TOT_comparativa XGB_CV_1-190625.csv")

df_complete <- df[complete.cases(df$media_registros),]

model_v2 <- lm( media_registros~valor_raster_.1.XGB.CV.M1.190625.CH_merge , data = df_complete)
# Calculo de métricas de desempeño
R2_v2 <- summary(model_v2)$r.squared
RMSE_v2 <- sqrt(mean(residuals(model_v2)^2))
Bias_v2 <- mean(df_complete$media_registros - df_complete$valor_raster_.1.XGB.CV.M1.190625.CH_merge)
n_v2 <- nrow(df_complete)
df_metrica_v2 <- data.frame(R2_v2,RMSE_v2,Bias_v2,n_v2)
df_metrica_v2


model_v3 <- lm( media_registros~ extracted_values_WUSTL_TOT, data = df_complete)
# Calculo de métricas de desempeño
R2_v3 <- summary(model_v3)$r.squared
RMSE_v3 <- sqrt(mean(residuals(model_v3)^2))
Bias_v3 <- mean(df_complete$media_registros - df_complete$extracted_values_WUSTL_TOT)
n_v3<- nrow(df_complete)
df_metrica_v3 <- data.frame(R2_v3,RMSE_v3,Bias_v3,n_v3)
df_metrica_v3



model_v4 <- lm( valor_raster_.1.XGB.CV.M1.190625.CH_merge~ extracted_values_WUSTL_TOT, data = df_complete)
# Calculo de métricas de desempeño
R2_v4 <- summary(model_v4)$r.squared
RMSE_v4 <- sqrt(mean(residuals(model_v4)^2))
Bias_v4 <- mean(df_complete$valor_raster_.1.XGB.CV.M1.190625.CH_merge - df_complete$extracted_values_WUSTL_TOT)
n_v4<- nrow(df_complete)
df_metrica_v4 <- data.frame(R2_v4,RMSE_v4,Bias_v4,n_v4)
df_metrica_v4



library(ggplot2)
library(ggpointdensity)  # si no lo tenés: install.packages("ggpointdensity")

plot_RLS <- ggplot(df_complete, aes(x = extracted_values_WUSTL_TOT, y = valor_raster_.1.XGB.CV.M1.190625.CH_merge)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+ 
  #theme(legend.position="none")+  theme_classic() +
  theme(
    #legend.position = "none",
    axis.text = element_text(size = 14),     # 🔹 Aumenta tamaño de los valores de ambos ejes
    axis.title = element_text(size = 11)     # 🔹 (opcional) aumenta tamaño de los títulos de ejes
  )+  labs(
    x = " ",   # 🔹 Nombre del eje X
    y = " "     # 🔹 Nombre del eje Y
  ) 

plot_RLS



plot_RLS <- ggplot(df_complete, aes(y = extracted_values_WUSTL_TOT, x = media_registros)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+ 
  #theme(legend.position="none")+  theme_classic() +
  theme(
    #legend.position = "none",
    axis.text = element_text(size = 14),     # 🔹 Aumenta tamaño de los valores de ambos ejes
    axis.title = element_text(size = 11)     # 🔹 (opcional) aumenta tamaño de los títulos de ejes
  )+  labs(
    x = " ",   # 🔹 Nombre del eje X
    y = " "     # 🔹 Nombre del eje Y
  ) 

plot_RLS



library(ggplot2)
library(tidyr)
library(dplyr)

# Supongamos que df_complete tiene estas columnas:
# media_registros, extracted_values_WUSTL_TOT, valor_raster_.1.XGB.CV.M1.190625.CH_merge

# 1️⃣ Reestructuramos el dataframe a formato largo
df_long <- df_complete %>%
  pivot_longer(
    cols = c(extracted_values_WUSTL_TOT, valor_raster_.1.XGB.CV.M1.190625.CH_merge),
    names_to = "variable",
    values_to = "valor_y"
  )

# 2️⃣ Graficamos ambas relaciones
plot_RLS <- ggplot(df_long, aes(x = media_registros, y = valor_y, color = variable)) +
  geom_point(alpha = 0.6) +  # puntos semi transparentes
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  # una recta por grupo
  geom_abline(slope = 1, intercept = 0, color = "black") +       # línea de referencia 1:1
  scale_color_manual(values = c("blue", "red")) +                # colores personalizados
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
    legend.position = "none"#,    # si querés, podés cambiarlo
    #legend.title = element_blank(), # para que no aparezca el título de la leyenda
    # para eliminar completamente la leyenda, usar: legend.position = "none"
  )

plot_RLS

#
###############################################
###############################################
library(ggplot2)
library(tidyr)
library(dplyr)

# tructuramos el dataframe a formato largo
df_long <- df_complete %>%
  pivot_longer(
    cols = c(extracted_values_WUSTL_TOT, valor_raster_.1.XGB.CV.M1.190625.CH_merge),
    names_to = "variable",
    values_to = "valor_y"
  )

# Graficamos
plot_RLS <- ggplot(df_long, aes(x = media_registros, y = valor_y)) +
  # 🔹 Puntos
  geom_point(aes(color = variable), alpha = 0.8, size = 1) +
  # 🔹 Línea 1 (azul oscuro)
  geom_smooth(
    data = subset(df_long, variable == "extracted_values_WUSTL_TOT"),
    method = "lm", se = FALSE, color = "#045a8d", linetype = "solid", size = 1.2
  ) +
  # 🔹 Línea 2 (verde oscuro)
  geom_smooth(
    data = subset(df_long, variable == "valor_raster_.1.XGB.CV.M1.190625.CH_merge"),
    method = "lm", se = FALSE, color = "#006d2c", linetype = "solid", size = 1.2
  ) +
  # 🔹 Línea 1:1
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8) +
  # 🔹 Colores y etiquetas personalizadas
  scale_color_manual(
    values = c("#2b8cbe", "#2ca25f"),
    labels = c("V5GL04", "This model"),
    name = NULL   # quita el título "Variable"
  ) +
  # 🔹 Ejes
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 40)) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 40)) +
  # 🔹 Tema
  theme_classic() +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 9),        # 🔸 tamaño texto leyenda
    legend.position = c(0.15, 0.8),                # 🔸 posición dentro del gráfico
    #legend.background = element_rect(fill = "white", color = "gray80")
  ) +
  labs(
    x = " ",
    y = " "
  )

plot_RLS


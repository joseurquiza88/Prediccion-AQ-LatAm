
# Estadisticas delos mapas
########################################################################
########################################################################
########################################################################
## SP

SP_AOD <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/modelos/salidas/SalidasAnuales/01-XGB-CV-M1-200525-SP/Promedio_anual_2024-01-XGB-CV-M1-200525-SP_Recorte.tif")
SP_sAOD <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/modelos/salidas/SalidasAnuales/02-XGB-CV-1-210525-sAOD-SP/Promedio_anual_2024-02-XGB-CV-1-210525-sAOD-SP_Recortado.tif")


# Extraer los valores del raster eliminando los NA
valores_AOD_SP <- values(SP_AOD$Promedio_anual_2024.01.XGB.CV.M1.200525.SP_Recorte)[!is.na(values(SP_AOD$Promedio_anual_2024.01.XGB.CV.M1.200525.SP_Recorte))]

valores_SAOD_SP <- values(SP_sAOD$Promedio_anual_2024.02.XGB.CV.1.210525.sAOD.SP_Recortado)[!is.na(values(SP_sAOD$Promedio_anual_2024.02.XGB.CV.1.210525.sAOD.SP_Recortado))]

mean(valores_AOD_SP)
sd(valores_AOD_SP)
min(valores_AOD_SP)
max(valores_AOD_SP)
mean(valores_SAOD_SP)
sd(valores_SAOD_SP)
min(valores_SAOD_SP)
max(valores_SAOD_SP)
mean_valor_raster_AOD = "#a1d99b" 
mean_valor_raster_sAOD = "#41ab5d"



# Suponiendo que valores_AOD_SP es un vector numérico sin NA
df <- data.frame(AOD = valores_AOD_SP,
                 sAOD = valores_SAOD_SP)

ggplot(df, aes(x = AOD)) +
  geom_histogram(binwidth = 2, fill = "#a1d99b", color = "black", boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
  scale_y_continuous(limits = c(0, 300))+
  labs(#title = "Histograma AOD promedio 2024",
       x = " ", 
       y = " ") +
  theme_classic(base_size = 15)

ggplot(df, aes(x = sAOD)) +
  geom_histogram(binwidth = 2, fill = "#41ab5d", color = "black", boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
  scale_y_continuous(limits = c(0, 300))+
  labs(#title = "Histograma AOD promedio 2024",
    x = " ", 
    y = " ") +
  theme_classic(base_size = 15)




############
library(raster)
library(RColorBrewer)

# Cargar raster (ya los tienes)
SP_AOD <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/modelos/salidas/SalidasAnuales/01-XGB-CV-M1-200525-SP/Promedio_anual_2024-01-XGB-CV-M1-200525-SP_Recorte.tif")
SP_sAOD <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/modelos/salidas/SalidasAnuales/02-XGB-CV-1-210525-sAOD-SP/Promedio_anual_2024-02-XGB-CV-1-210525-sAOD-SP_Recortado.tif")

# Asegurarse que tienen la misma resolución y extensión
SP_sAOD <- resample(SP_sAOD, SP_AOD, method = "bilinear")

# Calcular mapa de diferencia (con AOD - sin AOD)
diff_map <- SP_AOD - SP_sAOD

# Visualizar el mapa de diferencias
# Paleta de colores: azul para diferencias negativas, rojo para positivas
pal <- colorRampPalette(c("blue", "white", "red"))(100)

plot(diff_map, col = pal, 
     #main = "Mapa de diferencia PM2.5 (Con AOD - Sin AOD)",
     xlab = " ", ylab = " ")
legend("bottomright", legend = c("Mayor diferencia", "Menor diferencia"),
       fill = c("blue", "red"), bty = "n")

# Opcional: añadir una leyenda con puntos críticos de diferencia
legend("topright", legend = c("Mejora con AOD", "Peor con AOD"),
       fill = c("blue", "red"), bty = "n")


legend("topright",
       legend = c("Diferencia positiva (modelo con AOD > sin AOD)", "Diferencia negativa (modelo con AOD < sin AOD)"),
       fill = c("blue", "red"), bty = "n")
### ### ### ### ### ### ### ### ### ### 
### DIFERENCIA RELATIVA
# Calcular mapa de diferencia (con AOD - sin AOD)
diff_map_relativa <- ((SP_AOD - SP_sAOD)/SP_sAOD)*100

# Visualizar el mapa de diferencias
# Paleta de colores: azul para diferencias negativas, rojo para positivas
pal <- colorRampPalette(c("blue", "white", "red"))(100)

plot(diff_map_relativa, col = pal, 
     #main = "Mapa de diferencia PM2.5 (Con AOD - Sin AOD)",
     xlab = " ", ylab = " ")
legend("bottomright", legend = c("Mayor diferencia", "Menor diferencia"),
       fill = c("blue", "red"), bty = "n")

# Opcional: añadir una leyenda con puntos críticos de diferencia
legend("topright", legend = c("Mejora con AOD", "Peor con AOD"),
       fill = c("blue", "red"), bty = "n")


legend("topright",
       legend = c("Diferencia positiva (modelo con AOD > sin AOD)", "Diferencia negativa (modelo con AOD < sin AOD)"),
       fill = c("blue", "red"), bty = "n")


########################################################################
########################################################################
########################################################################
## CH

CH_AOD <- raster("D:/Josefina/Proyectos/ProyectoChile/CH/modelos/salidas/SalidasAnuales/01-XGB-CV-M1-190625-CH/Promedio_anual_2024-01-XGB-CV-M1-190625-CH.tif")
CH_sAOD <- raster("D:/Josefina/Proyectos/ProyectoChile/CH/modelos/salidas/SalidasAnuales/02-XGB-CV-M1-230625-sAOD-CH/Promedio_anual_2024-02-XGB-CV-M1-230625-sAOD-CH.tif")


# Extraer los valores del raster eliminando los NA
valores_AOD_CH <- values(CH_AOD$Promedio_anual_2024.01.XGB.CV.M1.190625.CH)[!is.na(values(CH_AOD$Promedio_anual_2024.01.XGB.CV.M1.190625.CH))]

valores_SAOD_CH <- values(CH_sAOD$Promedio_anual_2024.02.XGB.CV.M1.230625.sAOD.CH)[!is.na(values(CH_sAOD$Promedio_anual_2024.02.XGB.CV.M1.230625.sAOD.CH))]

round(mean(valores_AOD_CH),2)
round(sd(valores_AOD_CH),2)
round(min(valores_AOD_CH),2)
round(max(valores_AOD_CH),2)

round(mean(valores_SAOD_CH),2)
round(sd(valores_SAOD_CH),2)
round(min(valores_SAOD_CH),2)
round(max(valores_SAOD_CH),2)

mean_valor_raster_AOD = "#feb24c", 
mean_valor_raster_sAOD = "#fc4e2a" 



# Suponiendo que valores_AOD_CH es un vector numérico sin NA
df <- data.frame(AOD = valores_AOD_CH,
                 sAOD = valores_SAOD_CH)

ggplot(df, aes(x = AOD)) +
  geom_histogram(binwidth = 2, fill = "#feb24c",  color = "black", boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10)) +
  scale_y_continuous(limits = c(0, 300))+
  labs(#title = "Histograma AOD promedio 2024",
    x = " ", 
    y = " ") +
  theme_classic(base_size = 15)

ggplot(df, aes(x = sAOD)) +
  geom_histogram(binwidth = 2, fill = "#fc4e2a" , color = "black", boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10)) +
  scale_y_continuous(limits = c(0, 300))+
  labs(#title = "Histograma AOD promedio 2024",
    x = " ", 
    y = " ") +
  theme_classic(base_size = 15)

############
library(raster)
library(RColorBrewer)


# Asegurarse que tienen la misma resolución y extensión
CH_sAOD <- resample(CH_sAOD, CH_AOD, method = "bilinear")

# Calcular mapa de diferencia (con AOD - sin AOD)
diff_map <- CH_AOD - CH_sAOD

# Visualizar el mapa de diferencias
# Paleta de colores: azul para diferencias negativas, rojo para positivas
pal <- colorRampPalette(c("blue", "white", "red"))(100)

plot(diff_map, col = pal)#, 
     #main = "Mapa de diferencia PM2.5 (Con AOD - Sin AOD)",
     xlab = "Longitud", ylab = "Latitud")

# Opcional: añadir una leyenda con puntos críticos de diferencia
legend("topright", legend = c("Mejora con AOD", "Peor con AOD"),
       fill = c("blue", "red"), bty = "n")

legend("bottomright", legend = c("Mayor diferencia", "Menor diferencia"),
       fill = c("blue", "red"), bty = "n")

legend("topright",
       legend = c("Diferencia positiva (modelo con AOD > sin AOD)", "Diferencia negativa (modelo con AOD < sin AOD)"),
       fill = c("blue", "red"), bty = "n")

### ### ### ### ### ### ### ### ### ### 
### DIFERENCIA RELATIVA
# Calcular mapa de diferencia (con AOD - sin AOD)
diff_map_relativa <- ((CH_AOD - CH_sAOD)/CH_sAOD)*100
plot(diff_map_relativa, col = pal)#, 
#main = "Mapa de diferencia PM2.5 (Con AOD - Sin AOD)",
xlab = "Longitud", ylab = "Latitud")
########################################################################
########################################################################
########################################################################
## BA

BA_AOD <- raster("D:/Josefina/Proyectos/ProyectoChile/BA/modelos/salidas/SalidasAnuales/01-ET-CV-M1-170625-BA/Promedio_anual_2024-01-ET-CV-M1-170625-BA_Recorte.tif")
BA_sAOD <- raster("D:/Josefina/Proyectos/ProyectoChile/BA/modelos/salidas/SalidasAnuales/02-ET-CV-M1-230625-sAOD-BA/Promedio_anual_2024-02-ET-CV-M1-230625-sAOD-BA_recorte.tif")


# Extraer los valores del raster eliminando los NA
valores_AOD_BA <- values(BA_AOD$Promedio_anual_2024.01.ET.CV.M1.170625.BA_Recorte)[!is.na(values(BA_AOD$Promedio_anual_2024.01.ET.CV.M1.170625.BA_Recorte))]

valores_SAOD_BA <- values(BA_sAOD$Promedio_anual_2024.02.ET.CV.M1.230625.sAOD.BA_recorte)[!is.na(values(BA_sAOD$Promedio_anual_2024.02.ET.CV.M1.230625.sAOD.BA_recorte))]

round(mean(valores_AOD_BA),2)
round(sd(valores_AOD_BA),2)
round(min(valores_AOD_BA),2)
round(max(valores_AOD_BA),2)
round(mean(valores_SAOD_BA),2)
round(sd(valores_SAOD_BA),2)
round(min(valores_SAOD_BA),2)
round(max(valores_SAOD_BA),2)

mean_valor_raster_AOD = "#fb6a4a",
mean_valor_raster_sAOD = "#99000d" 



# Suponiendo que valores_AOD_BA es un vector numérico sin NA
df <- data.frame(AOD = valores_AOD_BA,
                 sAOD = valores_SAOD_BA)

ggplot(df, aes(x = AOD)) +
  geom_histogram(binwidth = 2, fill = "#fb6a4a",  color = "black", boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
  scale_y_continuous(limits = c(0, 3000))+
  labs(#title = "Histograma AOD promedio 2024",
    x = " ", 
    y = " ") +
  theme_classic(base_size = 15)

ggplot(df, aes(x = sAOD)) +
  geom_histogram(binwidth = 2, fill = "#99000d"  , color = "black", boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
  scale_y_continuous(limits = c(0, 3000))+
  labs(#title = "Histograma AOD promedio 2024",
    x = " ", 
    y = " ") +
  theme_classic(base_size = 15)


############
library(raster)
library(RColorBrewer)


# Asegurarse que tienen la misma resolución y extensión
BA_sAOD <- resample(BA_sAOD, BA_AOD, method = "bilinear")

# Calcular mapa de diferencia (con AOD - sin AOD)
diff_map <- BA_AOD - BA_sAOD

# Visualizar el mapa de diferencias
# Paleta de colores: azul para diferencias negativas, rojo para positivas
pal <- colorRampPalette(c("blue", "white", "red"))(100)

plot(diff_map, col = pal)#, 
     #main = "Mapa de diferencia PM2.5 (Con AOD - Sin AOD)",
     xlab = "Longitud", ylab = "Latitud")

# Opcional: añadir una leyenda con puntos críticos de diferencia
legend("topright", legend = c("Mejora con AOD", "Peor con AOD"),
       fill = c("blue", "red"), bty = "n")

legend("topright", legend = c("Mayor diferencia", "Menor diferencia"),
       fill = c("blue", "red"), bty = "n")

legend("topright",
       legend = c("Diferencia positiva (modelo con AOD > sin AOD)", "Diferencia negativa (modelo con AOD < sin AOD)"),
       fill = c("blue", "red"), bty = "n")


### ### ### ### ### ### ### ### ### ### 
### DIFERENCIA RELATIVA
# Calcular mapa de diferencia (con AOD - sin AOD)
diff_map_relativa <- ((CH_AOD - CH_sAOD)/CH_sAOD)*100
plot(diff_map_relativa, col = pal)#, 
########################################################################
########################################################################
########################################################################
## MD

MD_AOD <- raster("D:/Josefina/Proyectos/ProyectoChile/MD/modelos/salidas/SalidasAnuales/01-ET-CV-M1-260525-MD/Promedio_anual_2024-01-ET-CV-M1-260525-MD_recortado.tif")
MD_sAOD <- raster("D:/Josefina/Proyectos/ProyectoChile/MD/modelos/salidas/SalidasAnuales/01-ET-CV-M1-270525-sAOD-MD/Promedio_anual_2024-01-ET-CV-M1-270525-sAOD-MD_recortado.tif")



# Extraer los valores del raster eliminando los NA
valores_AOD_MD <- values(MD_AOD$Promedio_anual_2024.01.ET.CV.M1.260525.MD_recortado)[!is.na(values(MD_AOD$Promedio_anual_2024.01.ET.CV.M1.260525.MD_recortado))]

valores_SAOD_MD <- values(MD_sAOD$Promedio_anual_2024.01.ET.CV.M1.270525.sAOD.MD_recortado)[!is.na(values(MD_sAOD$Promedio_anual_2024.01.ET.CV.M1.270525.sAOD.MD_recortado))]

round(mean(valores_AOD_MD),2)
round(sd(valores_AOD_MD),2)
round(min(valores_AOD_MD),2)
round(max(valores_AOD_MD),2)
round(mean(valores_SAOD_MD),2)
round(sd(valores_SAOD_MD),2)
round(min(valores_SAOD_MD),2)
round(max(valores_SAOD_MD),2)

mean_valor_raster_AOD = "#9ecae1",#"#4292c6",
mean_valor_raster_sAOD = "#2171b5"#"#023858"



# Suponiendo que valores_AOD_MD es un vector numérico sin NA
df <- data.frame(AOD = valores_AOD_MD,
                 sAOD = valores_SAOD_MD)

ggplot(df, aes(x = AOD)) +
  geom_histogram(binwidth = 2, fill = "#9ecae1",  color = "black", boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
  scale_y_continuous(limits = c(0, 200))+
  labs(#title = "Histograma AOD promedio 2024",
    x = " ", 
    y = " ") +
  theme_classic(base_size = 15)

ggplot(df, aes(x = sAOD)) +
  geom_histogram(binwidth = 2, fill = "#2171b5" , color = "black", boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
  scale_y_continuous(limits = c(0, 200))+
  labs(#title = "Histograma AOD promedio 2024",
    x = " ", 
    y = " ") +
  theme_classic(base_size = 15)


############
library(raster)
library(RColorBrewer)


# Asegurarse que tienen la misma resolución y extensión
MD_sAOD <- resample(MD_sAOD, MD_AOD, method = "bilinear")

# Calcular mapa de diferencia (con AOD - sin AOD)
diff_map <- MD_AOD - MD_sAOD

# Visualizar el mapa de diferencias
# Paleta de colores: azul para diferencias negativas, rojo para positivas
pal <- colorRampPalette(c("blue", "white", "red"))(100)

plot(diff_map, col = pal)#, 
     #main = "Mapa de diferencia PM2.5 (Con AOD - Sin AOD)",
     xlab = "Longitud", ylab = "Latitud")

# Opcional: añadir una leyenda con puntos críticos de diferencia
legend("topright", legend = c("Mejora con AOD", "Peor con AOD"),
       fill = c("blue", "red"), bty = "n")

legend("topright", legend = c("Mayor diferencia", "Menor diferencia"),
       fill = c("blue", "red"), bty = "n")

legend("topright",
       legend = c("Diferencia positiva (modelo con AOD > sin AOD)", "Diferencia negativa (modelo con AOD < sin AOD)"),
       fill = c("blue", "red"), bty = "n")


### ### ### ### ### ### ### ### ### ### 
### DIFERENCIA RELATIVA
# Calcular mapa de diferencia (con AOD - sin AOD)
diff_map_relativa <- ((MD_AOD - MD_sAOD)/MD_sAOD)*100
plot(diff_map_relativa, col = pal)#, 
########################################################################
########################################################################
########################################################################
## MX

MX_AOD <- raster("D:/Josefina/Proyectos/ProyectoChile/MX/modelos/salidas/SalidasAnuales/01-XGB-CV-M1-290525-MX/Promedio_anual_2024-01-XGB-CV-M1-290525-MX_Recortado.tif")
MX_sAOD <- raster("D:/Josefina/Proyectos/ProyectoChile/MX/modelos/salidas/SalidasAnuales/02-XGB-CV-M1-230625-sAOD-MX/Promedio_anual_2024-02-XGB-CV-M1-230625-sAOD-MX_Recortado.tif")


# Extraer los valores del raster eliminando los NA
valores_AOD_MX <- values(MX_AOD$Promedio_anual_2024.01.XGB.CV.M1.290525.MX_Recortado)[!is.na(values(MX_AOD$Promedio_anual_2024.01.XGB.CV.M1.290525.MX_Recortado))]

valores_SAOD_MX <- values(MX_sAOD$Promedio_anual_2024.02.XGB.CV.M1.230625.sAOD.MX_Recortado)[!is.na(values(MX_sAOD$Promedio_anual_2024.02.XGB.CV.M1.230625.sAOD.MX_Recortado))]

round(mean(valores_AOD_MX),2)
round(sd(valores_AOD_MX),2)
round(min(valores_AOD_MX),2)
round(max(valores_AOD_MX),2)
round(mean(valores_SAOD_MX),2)
round(sd(valores_SAOD_MX),2)
round(min(valores_SAOD_MX),2)
round(max(valores_SAOD_MX),2)

mean_valor_raster_AOD = "#807dba",
mean_valor_raster_sAOD = "#810f7c"#"#3f007d"



# Suponiendo que valores_AOD_MX es un vector numérico sin NA
df <- data.frame(AOD = valores_AOD_MX,
                 sAOD = valores_SAOD_MX)

ggplot(df, aes(x = AOD)) +
  geom_histogram(binwidth = 2, fill = "#807dba",  color = "black", boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10)) +
  scale_y_continuous(limits = c(0, 1500))+
  labs(#title = "Histograma AOD promedio 2024",
    x = " ", 
    y = " ") +
  theme_classic(base_size = 15)

ggplot(df, aes(x = sAOD)) +
  geom_histogram(binwidth = 2, fill = "#810f7c" , color = "black", boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10)) +
  scale_y_continuous(limits = c(0, 1500))+
  labs(#title = "Histograma AOD promedio 2024",
    x = " ", 
    y = " ") +
  theme_classic(base_size = 15)



############
library(raster)
library(RColorBrewer)


# Asegurarse que tienen la misma resolución y extensión
MX_sAOD <- resample(MX_sAOD, MX_AOD, method = "bilinear")

# Calcular mapa de diferencia (con AOD - sin AOD)
diff_map <- MX_AOD - MX_sAOD

# Visualizar el mapa de diferencias
# Paleta de colores: azul para diferencias negativas, rojo para positivas
pal <- colorRampPalette(c("blue", "white", "red"))(100)

plot(diff_map, col = pal)#, 
     #main = "Mapa de diferencia PM2.5 (Con AOD - Sin AOD)",
     xlab = "Longitud", ylab = "Latitud")

# Opcional: añadir una leyenda con puntos críticos de diferencia
legend("topright", legend = c("Mejora con AOD", "Peor con AOD"),
       fill = c("blue", "red"), bty = "n")

legend("topright", legend = c("Mayor diferencia", "Menor diferencia"),
       fill = c("blue", "red"), bty = "n")

legend("topright",
       legend = c("Diferencia positiva (modelo con AOD > sin AOD)", "Diferencia negativa (modelo con AOD < sin AOD)"),
       fill = c("blue", "red"), bty = "n")


### ### ### ### ### ### ### ### ### ### 
### DIFERENCIA RELATIVA
# Calcular mapa de diferencia (con AOD - sin AOD)
pal <- colorRampPalette(c("blue", "white", "red"))(100)

diff_map_relativa <- ((MX_AOD - MX_sAOD)/MX_sAOD)*100
plot(diff_map_relativa, col = pal)#, 
diff_map_relativa
#########################################################
############################################################
library(ggplot2)
library(dplyr)

# Supongamos que estos son tus vectores de datos
# valores_AOD_SP <- c(...)  
# valores_AOD_ST <- c(...)  
# valores_AOD_BA <- c(...)  
# valores_AOD_MD <- c(...)  
# valores_AOD_MX <- c(...)  

library(ggplot2)
library(dplyr)

# Combinar en un solo data frame con el orden deseado
df <- data.frame(
  valor = c(valores_AOD_SP, valores_AOD_CH, valores_AOD_BA, valores_AOD_MD, valores_AOD_MX),
  sitio = factor(rep(c("SP", "ST", "BA", "MD", "MX"),
                     times = c(length(valores_AOD_SP), length(valores_AOD_CH), 
                               length(valores_AOD_BA), length(valores_AOD_MD), 
                               length(valores_AOD_MX))),
                 levels = c("SP", "ST", "BA", "MD", "MX"))
)

# Definir paleta de colores (cambié CH por ST)
colores <- c(
  "SP" = "#00441b",
  "ST" = "#fc4e2a",
  "BA" = "#99000d",
  "MD" = "#023858",
  "MX" = "#3f007d"
)

# Graficar con texto sobre la línea horizontal usando geom_text
a<-ggplot(df, aes(x = sitio, y = valor, fill = sitio)) +
  geom_boxplot(width = 0.4, alpha = 0.7, outlier.size = 1) +
  scale_fill_manual(values = colores) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "black", size = 0.7) +
  labs(#title = "Distribución de concentraciones AOD por sitio",
     x = " ",
     y = " ")+#expression(paste("Concentración de AOD (", mu, "g/m"^3, ")"))) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 40)) +
  theme(legend.position = "none",
        text = element_text(size = 11))
a
a+ggplot2::annotate("text", x = 1.5, y = 6, label = "Limite anual OMS ", hjust = 1, size = 4)
 a 
 
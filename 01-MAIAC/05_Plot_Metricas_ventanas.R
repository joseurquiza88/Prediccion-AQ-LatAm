


#######################################################################
## OBJETIVO: Plots estadisticas MAIAC-AERONET 
## segun ventanas espacio-temporales
#######################################################################

#Abrir csv que guardamos con todas las metricas
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V03/metricas_V03.csv")
# Filtros para hacer los plots
metricas <- metricas[metricas$collection == "C61",]
metricas <- metricas[metricas$region == "latam",]


############################################################
##                     R2
############################################################
# Hacer el factor nos permite ordenar despues la info
metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
# Vemos los nombres de las metricas para no equivocarnos
unique(metricas$metrica)
# Seleccion de metrica
metrica_interes <- "r2"   
# Filtramos y nos quedamos con un df solo con la metrica
metricas_subset <- metricas[metricas$metrica == metrica_interes,]
# Seteamos para plotear las ventanas espacio-temporales
metricas_subset$buffer <- factor(metricas_subset$espacial, levels = c(1, 3, 5, 15, 25))
metricas_subset$buffer <- factor(metricas_subset$temporal, levels = c(30, 60, 90, 120))
# Seteamos nombres de los sitios/estacion para poder ordenarlos 
#como queremos
metricas_subset$ciudad <- factor(metricas_subset$estacion)
metricas_subset$ciudad <- factor(metricas_subset$ciudad, 
                                 levels = c("SP", "ST", "BA", "MD", "LP", "MX"))
#################################################################
#--- Plots varios seleccionar el mejor
# Grafico de barras agrupadas

r2_agrupadas<-ggplot(metricas_subset, aes(x = ciudad, y = valor, fill = buffer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    x = "Estacion",
    y = expression(R^2),
    fill = "Ventana espacial (km)"
  ) +
  scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256")) +
  
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),  # Agrandar los numeros de los ticks
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "none"  # Elimina la leyenda
  ) 

r2_agrupadas


# Guardar plot
ggsave("D:/Josefina/Proyectos/Tesis/plot/01-MAIAC_Performance/MAIAC-C61-AER-Latam-R2.png",r2_agrupadas,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)
############################################################
##                     RMSE
############################################################
# Seleccion de metrica
metrica_interes <-"rmse" 
# Filtramoss metrica de interes
metricas_subset <- metricas[metricas$metrica == metrica_interes,]
# Hacer el factor nos permite ordenar despues la info
metricas_subset$buffer <- factor(metricas_subset$espacial, levels = c(1, 3, 5, 15, 25))
metricas_subset$buffer <- factor(metricas_subset$temporal, levels = c(30,60,90,120))
metricas_subset$ciudad <- factor(metricas_subset$estacion)
metricas_subset$ciudad <- factor(metricas_subset$ciudad, 
                                 levels = c("SP", "ST", "BA", "MD", "LP", "MX"))

# Grafico de barras agrupadas
rmse_agrupadas<- ggplot(metricas_subset, aes(x = ciudad, y = valor, fill = buffer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    x = "Estacion",
    y = "RMSE",
    fill = "Ventana espacial (km)"
  ) +
  scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256")) +
  
  scale_y_continuous(limits=c(0, 0.12),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+#,0.16,0.18,0.2,
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),  # Agrandar los numeros de los ticks
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "none"  # Elimina la leyenda
  ) 

rmse_agrupadas
# Guardar plot
ggsave("D:/Josefina/Proyectos/Tesis/plot/01-MAIAC_Performance/MAIAC-C61-AER-Latam-RMSE.png",rmse_agrupadas,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)




############################################################
##                     BIAS
############################################################
# Seleccion de metrica
metrica_interes <-"bias" 
# Filtramoss metrica de interes
metricas_subset <- metricas[metricas$metrica == metrica_interes,]
# Hacer el factor nos permite ordenar despues la info
metricas_subset$buffer <- factor(metricas_subset$espacial, levels = c(1, 3, 5, 15, 25))
metricas_subset$buffer <- factor(metricas_subset$temporal, levels = c(30,60,90,120))
metricas_subset$ciudad <- factor(metricas_subset$estacion)
metricas_subset$ciudad <- factor(metricas_subset$ciudad, 
                                 levels = c("SP", "ST", "BA", "MD", "LP", "MX"))
# Grafico de barras agrupadas
bias_agrupado<- ggplot(metricas_subset, aes(x = ciudad, y = valor, fill = buffer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    x = "Estacion",
    y = "Bias",
    fill = "Ventana espacial (km)"
  ) +
  scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256")) +
  
  # Personalizar ejes
  scale_y_continuous(limits=c(-0.06, 0.06))+

  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),  # Agrandar los numeros de los ticks
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "none"  # Elimina la leyenda
  ) 

bias_agrupado
#Guardar plot
ggsave("D:/Josefina/Proyectos/Tesis/plot/01-MAIAC_Performance/MAIAC-C61-AER-Latam-Bias.png",bias_agrupado,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)



############################################################
##                     REU
############################################################
# Seleccion de metrica
metrica_interes <-"reuMeanAOD" 
# Filtramoss metrica de interes
metricas_subset <- metricas[metricas$metrica == metrica_interes,]
# Hacer el factor nos permite ordenar despues la info
metricas_subset$buffer <- factor(metricas_subset$espacial, levels = c(1, 3, 5, 15, 25))
metricas_subset$buffer <- factor(metricas_subset$temporal, levels = c(30,60,90,120))
metricas_subset$ciudad <- factor(metricas_subset$estacion)
metricas_subset$ciudad <- factor(metricas_subset$ciudad, 
                                 levels = c("SP", "ST", "BA", "MD", "LP", "MX"))



# Grafico de barras agrupadas
reu_agrupado<- ggplot(metricas_subset, aes(x = ciudad, y = valor, fill = buffer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    x = "Estacion",
    y = "REU",
    fill = "Ventana espacial (km)"
  ) +
  scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256")) +
  
  # Personalización de los ejes
  scale_y_continuous(limits=c(0, 300),breaks = c(0,40,80,120,160,200,240,280))+
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),  # Agrandar los numeros de los ticks
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "top"  # Elimina la leyenda
  ) 

reu_agrupado
ggsave("D:/Josefina/Proyectos/Tesis/plot/01-MAIAC_Performance/MAIAC-C61-AER-Latam-REU2.png",reu_agrupado,
       width = 14,
       height = 8,
       units = "cm",
       dpi = 500)


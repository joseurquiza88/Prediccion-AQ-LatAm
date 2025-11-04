
#######################################################################
## OBJETIVO: Analisis de las estaciones de monitoreo de PM2.5 
# en los centros urbanos seleccionados
## se hacen distintos plots
#######################################################################


### Numero de datos entrnamiento-testeo

datos <- data.frame(
  Sitio = rep(c("SP", "ST", "BA", "MD", "MX"), each = 2),
  Tipo = rep(c("Entrenamiento", "Testeo"), times = 5),
  Observaciones = c(8867, 3799, 15800, 6768, 2421, 1035, 4695, 2009, 16077, 6887)
)

# Ordenar sitios
orden_sitios <- datos %>%
  group_by(Sitio) %>%
  summarise(total = sum(Observaciones)) %>%
  arrange(total) %>%
  pull(Sitio)
# Poner factor para ordenar los sitios
datos$Sitio <- factor(datos$Sitio, levels = orden_sitios)

# Calcular proporciones
datos_prop <- datos %>%
  group_by(Sitio) %>%
  mutate(prop = Observaciones / sum(Observaciones))
# Plot de las proporciones teste/entrenamiento
ggplot(datos_prop, aes(x = Sitio, y = prop, fill = Tipo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Observaciones),
            position = position_stack(vjust = 0.5), 
            size = 3.5, color = "white") +  # texto blanco para mÃ¡s contraste
  labs(y = "ProporciÃ³n", x = "", fill = "Tipo de dato") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Entrenamiento" = "#4292c6", "Testeo" = "#08519c")) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    legend.position = "right"
  )

######################################################
## Analisis por centro urbano

estacion <- "MD"
data<- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/06_estaciones/",estacion,"_estaciones.csv",sep=""))
data$date <- as.POSIXct(as.character(data$date), format = "%d/%m/%Y")#"%Y-%m-%d")#
#data$mean<-data$Registros.completos # Para ST
# Corroboramos si hay datos faltanes
data <- data[complete.cases(data$mean),]
# Corroboramos si hay datos igual/distinto/mayor/menor  a 0
data <- data[data$mean !=0,]
data <- data[data$mean >0,]
# Nos quedamos con la info entre 2015-2024
data <- data[(year(data$date)) >= 2015,]
data <- data[(year(data$date)) < 2025,]

names(data)

######################################################
#             Estadisticas basicas generales
######################################################
summary(data$mean)
sd(data$mean)
######################################################
#             Estadisticas basicas por estacion
######################################################
resumen_por_estacion <- data %>%
  group_by(estacion) %>%
  summarise(
    cantidad = n(),
    promedio = mean(mean, na.rm = TRUE),
    minimo = min(mean, na.rm = TRUE),
    maximo = max(mean, na.rm = TRUE),
    sd = sd(mean, na.rm = TRUE),
    .groups = "drop"
  )
View(resumen_por_estacion)
# Estacion con los valores promedios mas bajos
estacion_min <- resumen_por_estacion[resumen_por_estacion$promedio == min(resumen_por_estacion$promedio),]
estacion_min
# Estacion con los valores promedios mas altos
estacion_max <- resumen_por_estacion[resumen_por_estacion$promedio == max(resumen_por_estacion$promedio),]
estacion_max

# Estacion con los valores  mas altos
estacion_picos_max <- resumen_por_estacion[resumen_por_estacion$maximo == max(resumen_por_estacion$maximo),]
estacion_picos_max

######################################################
#     Estadisticas basicas por estacion por mes
######################################################
data_2024 <- data[year(data$date) == 2024,]
unique(year(data_2024$date))
resumen_por_mes <- data %>%
  #resumen_por_mes <- data_2024 %>%
  mutate(mes = month(date, label = TRUE, abbr = FALSE, locale = "es_ES")) %>%
  group_by(mes) %>%
  summarise(
    minimo = round(min(mean, na.rm = TRUE),2),
    maximo = round(max(mean, na.rm = TRUE),2),
    promedio = round(mean(mean, na.rm = TRUE),2),
    sd = round(sd(mean, na.rm = TRUE),2),
    .groups = "drop"
  ) %>%
  arrange(match(mes, month.name))  # ordena los meses correctamente

View(resumen_por_mes)

# Estacion con los valores PICOS mas altos
# Obtener los 3 valores m?ximos ?nicos (orden descendente)
top3_vals_max <- sort(unique(resumen_por_mes$maximo), decreasing = TRUE)[1:3]
# Filtrar filas que tienen esos valores
estacion_picos_max <- resumen_por_mes[resumen_por_mes$maximo %in% top3_vals_max, ]


# Estacion con los valores  promedios mas bajos
# Obtener los 3 valores m?ximos ?nicos (orden descendente)
top3_vals_promedioMin <- sort(unique(resumen_por_mes$promedio), decreasing = FALSE)[1:3]
# Filtrar filas que tienen esos valores
estacion_picos_min <- resumen_por_mes[resumen_por_mes$promedio %in% top3_vals_promedioMin, ]
estacion_picos_min

top3_vals_promedioMax <- sort(unique(resumen_por_mes$promedio), decreasing = TRUE)[1:3]
# Filtrar filas que tienen esos valores
estacion_picos_maxProm <- resumen_por_mes[resumen_por_mes$promedio %in% top3_vals_promedioMax, ]
estacion_picos_maxProm

######################################################
#       Serie temporal diaria por estacion para todo 
######################################################
unique(data_plot$estacion)
media_por_estacion <- data %>%
  group_by(estacion) %>%
  summarise(media_estacion = mean(mean, na.rm = TRUE))
View(media_por_estacion)
data$date <- as.Date(data$date)
# Unir la media por estaci?n al dataframe original
data_plot <- left_join(data, media_por_estacion, by = "estacion")
nombre_estaciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))
nombre_estaciones
data_plot
data_combinada <- data_plot %>%
  left_join(nombre_estaciones, by = "estacion")
# Graficar
data_plot<-data_combinada
data_plot$estacion <- data_plot$estacion2 
stats_por_estacion <- data_plot %>%
  group_by(estacion) %>%
  summarise(
    media = round(mean(mean, na.rm = TRUE), 2),
    max = round(max(mean, na.rm = TRUE), 2),
    sd = round(sd(mean, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("Media: ", media, "\nSD: ", sd, "\nMax: ", max),
    x = as.Date("2015-01-01"),  # izquierda del gr?fico
    # y = 350  #CH                  # altura deseada del texto
    y = 120,
  )

# Crear un data frame para la l?nea horizontal (AMS mean)
lineas_extra <- data.frame(
  estacion = unique(data_plot$estacion),
  total_mean = 18.56
)
# Medias por estacion, corroborar!!
# SP 16.43, 
# ST 25.96, 
# BA 14.43 
# MD 18.56 
# MX 21.09

## Plot
serie_temporal <- ggplot() +
  geom_line(data = data_plot, aes(x = date, y = mean, color = "Media estacion")) +
  # Linea horizontal de AMS mean en cada faceta
  geom_hline(data = lineas_extra, aes(yintercept = total_mean, color = "Media SP"), size = 0.9) +
  geom_label(data = stats_por_estacion,
             aes(x = x, y = y, label = label),
             hjust = 0, vjust = 0,
             fill = "white", alpha = 1, size = 1.9) +
  # Facetas por estaci?n
  facet_wrap(~ estacion, scales = "fixed") +
  # Ejes
  scale_x_date(limits = as.Date(c("2015-01-01", "2024-12-31"))) +
  # scale_y_continuous(limits = c(0, 350)) +
  scale_y_continuous(limits = c(0, 200)) +
  # Definir colores y etiquetas de leyenda
  scale_color_manual(
    name = NULL,
    values = c("Media estacion" = "#2ca25f", "Media SP" = "red")
  ) +
  #labs(y = expression(PM[2.5]), x= "Date") +
  labs(x = NULL, y = NULL)+
  theme_classic() +
  theme(
    legend.position = "none",  # Eliminar la leyenda
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Rotar las etiquetas del eje x a 45 grados y tama?o de texto m?s peque?o
    axis.text.y = element_text(size =8),  # Reducir el tama?o de las etiquetas del eje y
    strip.text = element_text(size = 8)  # Cambiar el tama?o de los t?tulos de las facetas (subplots)
  )

# Vemos el plot y lo guardamos
serie_temporal
dir <- paste("D:/Josefina/Proyectos/Tesis/",estacion,"/plots/",sep="")
getwd()
setwd(dir)
ggsave(
  filename = paste(dir,"03_Serie-Temporal.png",sep=""),
  plot = serie_temporal,       
  width = 10,               # Ancho en pulgadas
  height = 6,               # Alto en pulgadas
  dpi = 500                 # Resolucion en puntos por pulgada (alta calidad)
)



######################################################
#             boxplot por estacion
######################################################
data$label <- "PM2.5"
## Colores por sitio
# c("SP" = "#005a32", 
#   "ST" = "#fd8d3c", 
#   "BA" = "#99000d", 
#   "MD" = "#023858", 
#   "MX" = "#ce1256")) 

### Plot
ggplot(data, aes(x = estacion, y = mean)) +
  geom_boxplot(fill ="#ce1256", outlier.shape = NA,width = 0.3) +
  #facet_wrap(~ ID, scales = "free_y") +
  scale_y_continuous(limits = c(0, 80)) +
  theme_classic() +
  labs(x = " ", y = " ") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)  
  )




######################################################
#             Promedios por año
######################################################
data$year <-  year(data$date)
promedio_anuales <- data %>%
  group_by(year) %>%
  summarise(avg_pm25 = mean(mean, na.rm = TRUE),
            min = min(mean, na.rm = TRUE),
            max = mean(mean, na.rm = TRUE),)
View(promedio_anuales)
######################################################
#            % de cambio 2015-2024 total
######################################################
### Las concentraciones diminuyeron/aumentaron entre 2015-2024?
prom_2015 <- promedio_anuales[promedio_anuales$year==2015,]
prom_2024 <-promedio_anuales[promedio_anuales$year==2024,]
porcentajeCambio <- round(((prom_2015$avg_pm25 - prom_2024$avg_pm25)/prom_2015$avg_pm25 )*100,2)
#Si es negativo significa que aumentaron, si es positivo disminuyeron
porcentajeCambio

######################################################
#            % de cambio 2015-2024 por estaciones
######################################################
# Calcular promedio anual por estacion
promedio_anuales <- data %>%
  group_by(estacion, year) %>% #Se agrupa por estacion y año
  summarise(avg_pm25 = mean(mean, na.rm = TRUE)) %>%
  ungroup()

# Filtrar para años 2015 y 2024 para buscar la diferencia
datos_2015 <- promedio_anuales %>% filter(year == 2015)
datos_2024 <- promedio_anuales %>% filter(year == 2024)

# Unir por estacion y calcular porcentaje de cambio
cambios_por_estacion <- datos_2015 %>%
  dplyr::select(estacion, avg_pm25_2015 = avg_pm25) %>%
  dplyr::left_join(
    datos_2024 %>% dplyr::select(estacion, avg_pm25_2024 = avg_pm25),
    by = "estacion"
  ) %>%
  dplyr::mutate(
    porcentaje_cambio = round(((avg_pm25_2024 - avg_pm25_2015) / avg_pm25_2015) * 100, 2)
  )

# Si es positivo significa que aumento, si el valor es negativo es porque disminuyeron los
# valores entre 2015 y 2024
View(cambios_por_estacion)




######################################################
#       Serie temporal por año por estacion
######################################################
# Asegurar que la columna fecha esta en formato Date
data$date <- as.Date(data$date)

# Crear columna de año
data$year <- year(data$date)

# Promedio anual por estacion
promedios_estacion <- data %>%
  group_by(estacion, year) %>%
  summarise(avg_pm25 = mean(mean, na.rm = TRUE), .groups = "drop")

# Promedio anual general
promedio_general <- data %>%
  group_by(year) %>%
  summarise(avg_pm25 = mean(mean, na.rm = TRUE)) %>%
  mutate(estacion = "Media BA")

# Unir ambos conjuntos
serie_completa <- bind_rows(promedios_estacion, promedio_general)

### Plot
serie_temporal_anual <- ggplot() +
  geom_line(data = serie_completa, 
            aes(x = year, y = avg_pm25, color = estacion, group = estacion),
            size = 0.7) +
  
  geom_line(data = filter(serie_completa, estacion == "Media BA"),
            aes(x = year, y = avg_pm25, group = estacion),
            color = "black", size = 1.3, linetype = "solid") +
  
  labs(
    x = NULL,
    y = NULL,
    color = "Estacion"
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 13),
    strip.text = element_text(size = 12)
  )

### Guardar plot
serie_temporal_anual
dir <- paste("D:/Josefina/Proyectos/Tesis/",estacion,"/plots/",sep="")
getwd()
setwd(dir)
ggsave(
  filename = paste(dir,"Serie_temporal_anual.png",sep=""),
  plot = serie_temporal_anual,       
  width = 8,               # Ancho en pulgadas
  height = 4,               # Alto en pulgadas
  dpi = 500                 # Resolucion en puntos por pulgada (alta calidad)
)  

####
# Crear columna con nombre del mes en ingles y completo
datos_boxplot <- data %>%
  mutate(
    mes = month(date, label = TRUE, abbr = FALSE, locale = "es_ES"),
    mes = factor(mes, levels = month.name)  # ordenar de enero a diciembre
  )

 # Boxplot con todos los valores diarios por mes
mensual_total <-ggplot(datos_boxplot, aes(x = mes, y = mean)) +
  #geom_boxplot(fill = "lightblue", color = "black") +
  geom_boxplot(
    fill = "lightblue",
    color = "black",
    outlier.shape = 21,         # circulo con borde
    outlier.size = 1.5,         # m?s peque?os
    outlier.stroke = 0.3,       # grosor del borde
    outlier.fill = NA,          # sin relleno
    outlier.colour = "black"    # color del borde
  )+
  labs(
    #title = "Distribucion diaria de PM2.5 por mes (2015-2024)",
    x = "Month",
    y = "Daily PM2.5"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



#########
# Otro plot
# Crear columna con nombre del mes en español y ordenarlos
datos_boxplot <- data %>%
  mutate(
    mes = month(date, label = TRUE, abbr = FALSE, locale = "es_ES"),
    mes = factor(mes, levels = c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                                 "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"))
  )

# Boxplot sin outliers y con meses en español
datos_boxplot <- datos_boxplot[complete.cases(datos_boxplot$mean),]
mensual_total <- ggplot(datos_boxplot, aes(x = mes, y = mean)) +
  geom_boxplot(
    fill =  "#ce1256",#"#023858",#"#99000d",#"#005a32",#"#fd8d3c",
    color = "black",
    outlier.shape = NA  # eliminar los outliers
  )  + scale_y_continuous(
    limits = c(0, 120),
    breaks = seq(0, 120, by = 40) 
  ) +
  labs(
    x = " ",
    y = "  "
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 13),
    strip.text = element_text(size = 12)
  )
#Plot
mensual_total
# Guardar plot
dir <- paste("D:/Josefina/Proyectos/Tesis/",estacion,"/plots/",sep="")
getwd()
setwd(dir)
ggsave(
  filename = paste(dir,"BoxPlot_mensual.png",sep=""),
  plot = mensual_total ,      
  width = 8,               # Ancho en pulgadas
  height = 4,               # Alto en pulgadas
  dpi = 500                 # Resolucion en puntos por pulgada (alta calidad)
)  

#################################################################################
#################################################################################
#                           Preparacion de datos 
# Seleccion de variables con el Factor VIF

# biblioteca para vif
library(car)
#  --        01. Preparacion de los datos: seleccion de variables
estacion <- "BA"
data_com <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp.csv",sep=""))
names(data_com)
#Poner en formato la fecha
data_com$date <- as.POSIXct(as.character(data_com$date), format = "%Y-%m-%d")
# Agregamos numero de dia
data_com$dayWeek <- wday(data_com$date, week_start = 1)
unique(year(data_com$date))
# lO volvemos a guardar porque no lo teniamos al weekday
write.csv(data_com,paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp.csv",sep=""))

# Nos quedamos solo con los datos 2015-2023
data_com<- data_com[year(data_com$date) != 2024,]
# Verificamos
unique(year(data_com$date))

#Generamos modelo lineal multiple con todas las variables  (17 Vars)
modelo <- lm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + OCSMASS_dia+ t2m_mean
               SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
               d2m_mean  + v10_mean + u10_mean + tp_mean + DEM + dayWeek,
             data = data_com)


vif(modelo)
# Esto te devuelve una tabla con el VIF para cada variable. 
#Como regla general:

#   VIF = 1: no hay colinealidad
# VIF entre 5 y 10: hay cierta colinealidad, ojo
# VIF > 10: colinealidad severa ??? deberia eliminar o transformar alguna variable

# Ordenamos VIF
sort(vif(modelo), decreasing = TRUE)
df <- data.frame(vif(modelo))
View(df)

car::vif(modelo)
summary(modelo)



######################################################
#########################################################
# Boxplot por ciudad
data_tot <- read.csv("D:/Josefina/Proyectos/Tesis/TOT/proceed/estaciones/TOT_estaciones.csv")
data_tot$date <- as.POSIXct(as.character(data_tot$date), format = "%d/%m/%Y")#"%Y-%m-%d")#
data_tot <- data_tot[complete.cases(data_tot$mean),]
data_tot <- data_tot[data_tot$mean !=0,]
data_tot <- data_tot[data_tot$mean >0,]
data_tot <- data_tot[(year(data_tot$date)) >= 2015,]
data_tot <- data_tot[(year(data_tot$date)) < 2025,]
names(data_tot)

data_tot$ciudad <- factor(data_tot$sitio, 
                                 levels = c("SP", "ST", "BA", "MD", "MX"))


library(ggplot2)

ggplot() +
  geom_boxplot(data = data_tot,
               mapping = aes(x = ciudad, y = mean, fill = ciudad),
               outlier.size = 0.6) +
  labs(
       x = "Sitio",
       y = expression(PM[2.5]~(mu*g/m^3)),
       fill = "Sitio") +  
  scale_fill_manual(values = c("#005a32", "#fd8d3c", "#99000d", "#023858", "#ce1256")) +
  theme_classic()


## corroboramos plot por las dudas que el factor no este bien hecho
ggplot() +
  geom_boxplot(data_tot, mapping=aes (x = sitio, y = mean,fill = "#023858")) +
  labs(title = "Distribucion de PM2.5 por sitio",
       x = "Sitio",
       y = "Concentracion de PM2.5 (µg/m³)") +
  #scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256"))+
  theme_classic()

######################################################
#########################################################
# Serie temporal anual por cada ciudad
# Agregar columna año
data_tot$year <- format(data_tot$date, "%Y")
unique(data_tot$year)
# Calcular promedio anual por ciudad
annual_means <- data_tot %>%
  group_by(ciudad, year) %>%
  summarise(mean_PM25 = mean(mean, na.rm = TRUE)) %>%
  ungroup()
# Plot de promedios anual por ciudad
ggplot(annual_means, aes(x = as.integer(year), y = mean_PM25, color = ciudad)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ ciudad, scales = "free_y") +
  scale_color_manual(values = c("SP" = "#005a32", 
                                "ST" = "#fd8d3c", 
                                "BA" = "#99000d", 
                                "MD" = "#023858", 
                                "MX" = "#ce1256")) +
  scale_y_continuous(limits = c(10, 35)) +
  scale_x_continuous(breaks = 2015:2024)+
  labs(
    x = "Año",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  

# Para ver si superan o no el umbral anual del OMS
annual_means_2024 <- annual_means[annual_means$year == 2024,]

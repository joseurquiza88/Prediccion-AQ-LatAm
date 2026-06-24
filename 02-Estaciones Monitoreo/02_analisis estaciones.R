
# Objetivo ----
# Analisis de las estaciones de monitoreo de PM2.5
# en los centros urbanos seleccionados
# se hacen distintos plots
#Numero de datos entrenamiento testeo ----
datos <- data.frame(
  sitio = rep(c("SP", "ST", "BA", "MD", "MX"), each = 2),
  tipo = rep(c("Entrenamiento", "Testeo"), times = 5),
  observaciones = c(8867, 3799, 15800, 6768, 2421,
                    1035, 4695, 2009, 16077, 6887)
)

# Ordenar sitios ----
orden_sitios <- datos %>%
  group_by(sitio) %>%
  summarise(total = sum(observaciones)) %>%
  arrange(total) %>%
  pull(sitio)
# Poner factor para ordenar los sitios
datos$sitio <- factor(datos$sitio, levels = orden_sitios)

# Calcular proporciones ----
datos_prop <- datos %>%
  group_by(sitio) %>%
  mutate(prop = observaciones / sum(observaciones))
# Plot de las proporciones testeo/entrenamiento
ggplot(datos_prop, aes(x = sitio, y = prop, fill = tipo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = observaciones),
            position = position_stack(vjust = 0.5), 
            size = 3.5, color = "white") +  # texto blanco para más contraste
  labs(y = "Proporción", x = "", fill = "tipo de dato") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Entrenamiento" = "#4292c6",
                               "Testeo" = "#08519c")) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    legend.position = "right"
  )

# Analisis por centro urbano ----

estacion <- "MD"
data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/", estacion,
                       "/proceed/06_estaciones/", estacion,
                       "_estaciones.csv", sep=""))
data$date <- as.POSIXct(as.character(data$date), format = "%d/%m/%Y")
# data$mean <- data$Registros.completos # Para ST
# Corroboramos si hay datos faltanes ----
data <- data[complete.cases(data$mean), ]
# Corroboramos si hay datos igual/distinto/mayor/menor  a 0
data <- data[data$mean != 0, ]
data <- data[data$mean > 0, ]
# Nos quedamos con la info entre 2015-2024
data <- data[(year(data$date)) >= 2015,]
data <- data[(year(data$date)) < 2025,]

names(data)

#Estadisticas basicas generales ----
summary(data$mean)
sd(data$mean)
# Estadisticas basicas por estacion ----
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
estacion_min <- resumen_por_estacion [resumen_por_estacion$promedio ==
                                        min(resumen_por_estacion$promedio),]
estacion_min
# Estacion con los valores promedios mas altos
estacion_max <- resumen_por_estacion [resumen_por_estacion$promedio ==
                                        max(resumen_por_estacion$promedio),]
estacion_max

# Estacion con los valores  mas altos
estacion_picos_max <- resumen_por_estacion[resumen_por_estacion$maximo == 
                                             max(resumen_por_estacion$maximo),]
estacion_picos_max

#Estadisticas basicas por estacion por mes ----
data_2024 <- data[year(data$date) == 2024,]
unique(year(data_2024$date))
resumen_por_mes <- data %>%
  mutate(mes = month(date, label = TRUE, abbr = FALSE, locale = "es_ES")) %>%
  group_by(mes) %>% summarise(
                              minimo = round(min(mean, na.rm = TRUE), 2),
                              maximo = round(max(mean, na.rm = TRUE), 2),
                              promedio = round(mean(mean, na.rm = TRUE), 2),
                              sd = round(sd(mean, na.rm = TRUE), 2),
                              .groups = "drop") %>%
  arrange(match(mes, month.name))
View(resumen_por_mes)

# Estacion con los valores PICOS mas altos0
top3_vals_max <- sort(unique(resumen_por_mes$maximo), decreasing = TRUE)[1:3]
# Filtrar filas que tienen esos valores
estacion_picos_max <- resumen_por_mes [resumen_por_mes$maximo %in% 
                                         top3_vals_max, ]

# Estacion con los valores  promedios mas bajos
top3_vals_promedioMin <- sort(unique(resumen_por_mes$promedio),
                              decreasing = FALSE)[1:3]
# Filtrar filas que tienen esos valores
estacion_picos_min <- resumen_por_mes[
                resumen_por_mes$promedio %in% top3_vals_promedioMin, ]
estacion_picos_min

top3_vals_promedioMax <- sort(unique(resumen_por_mes$promedio), 
decreasing = TRUE)[1:3]

# Filtrar filas que tienen esos valores
estacion_picos_maxProm <- resumen_por_mes [resumen_por_mes$promedio %in% top3_vals_promedioMax,]
estacion_picos_maxProm  

# Serie temporal diaria por estacion para todo ----
unique(data_plot$estacion)
media_por_estacion <- data %>%
  group_by(estacion) %>%
  summarise(media_estacion = mean(mean, na.rm = TRUE))
View(media_por_estacion)
data$date <- as.Date(data$date)

# Unir la media por estacion al dataframe original
data_plot <- left_join(data, media_por_estacion, by = "estacion")
nombre_estaciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",
                                    estacion,"/dataset/estaciones/sitios_",
                                    estacion, ".csv", sep = ""))

data_combinada <- data_plot %>%
  left_join(nombre_estaciones, by = "estacion")
# Graficar ----
data_plot <- data_combinada
data_plot$estacion <- data_plot$estacion2
stats_por_estacion <- data_plot %>%
  group_by(estacion) %>%
  summarise(media = round(mean(mean, na.rm = TRUE), 2),
            max = round(max(mean, na.rm = TRUE), 2),
            sd = round(sd(mean, na.rm = TRUE), 2),
            .groups = "drop") %>% 
  mutate(label = paste0("Media: ", media, "\nSD: ", sd, "\nMax: ", max),
         x = as.Date("2015-01-01"), y = 120)

# Linea horizontal
lineas_extra <- data.frame(
  estacion = unique(data_plot$estacion),
  total_mean = 18.56
)
# Medias por estacion, corroborar!!
# SP 16.43
# ST 25.96
# BA 14.43
# MD 18.56
# MX 21.09

## Plot ----
serie_temporal <- ggplot() +
  geom_line(data = data_plot, 
            aes(x = date, y = mean, color = "Media estacion")) +
  geom_hline(data = lineas_extra, 
             aes(yintercept = total_mean, color = "Media SP"),
             size = 0.9) +
  geom_label(data = stats_por_estacion, 
             aes(x = x, y = y, label = label), hjust = 0, vjust = 0,
             fill = "white", alpha = 1, size = 1.9) +
  facet_wrap(~ estacion, scales = "fixed") +
  scale_x_date(limits = as.Date(c("2015-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_color_manual(name = NULL, 
                     values = c("Media estacion" = "#2ca25f",
                                "Media SP" = "red")) +
  labs(x = NULL, y = NULL) + theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 8))

# Guadar plot ----
serie_tempora
dir <- paste("D:/Josefina/Proyectos/Tesis/", estacion,"/plots/", sep = "")
getwd()
setwd(dir)
ggsave(filename = paste(dir, "03_Serie-Temporal.png", sep = ""),
       plot = serie_temporal, width = 10, height = 6, dpi = 500)

# Boxplot por estacion ----
data$label <- "PM2.5"
## Colores por sitio
# c("SP" = "#005a32"
#   "ST" = "#fd8d3c"
#   "BA" = "#99000d"
#   "MD" = "#023858"
#   "MX" = "#ce1256"

### Plot
ggplot(data, aes(x = estacion, y = mean)) +
geom_boxplot(fill ="#ce1256", outlier.shape = NA,width = 0.3) +
scale_y_continuous(limits = c(0, 80)) + theme_classic() +
labs(x = " ", y = " ") +
theme( axis.text.x = element_blank(), axis.ticks.x = element_blank(),
axis.title.x = element_text(size = 13),
axis.text.y = element_text(size = 13))


# Promedios por año ----
data$year <-  year(data$date)
promedio_anuales <- data %>%
group_by(year) %>%
summarise(avg_pm25 = mean(mean, na.rm = TRUE),
min = min(mean, na.rm = TRUE),
max = max(mean, na.rm = TRUE),)
View(promedio_anuales)
# % de cambio 2015-2024 total ----
### Las concentraciones diminuyeron/aumentaron entre 2015-2024?
prom_2015 <- promedio_anuales[promedio_anuales$year == 2015,]
prom_2024 <-promedio_anuales[promedio_anuales$year == 2024,]
porcentajeCambio <- round(((prom_2015$avg_pm25 - prom_2024$avg_pm25)/prom_2015$avg_pm25 )*100,2)
#Si es negativo significa que aumentaron, si es positivo disminuyeron
porcentajeCambio
# % de cambio 2015-2024 por estaciones ----
# Calcular promedio anual por estacion
promedio_anuales <- data %>%
group_by(estacion, year) %>% #Se agrupa por estacion y a?o
summarise(avg_pm25 = mean(mean, na.rm = TRUE)) %>%
ungroup()

# Filtrar para a?os 2015 y 2024 para buscar la diferencia
datos_2015 <- promedio_anuales %>% filter(year == 2015)
datos_2024 <- promedio_anuales %>% filter(year == 2024)

# Unir por estacion y calcular porcentaje de cambio
cambios_por_estacion <- datos_2015 %>%
dplyr::select(estacion, avg_pm25_2015 = avg_pm25) %>%
dplyr::left_join(
datos_2024 %>% dplyr::select(estacion, avg_pm25_2024 = avg_pm25),
by = "estacion"
) %>%
dplyr::mutate(porcentaje_cambio = round(((
avg_pm25_2024 - avg_pm25_2015) / avg_pm25_2015) * 100, 2)
)

# Si es positivo significa que aumento, si el valor
es negativo es porque disminuyeron los valores entre 2015 y 2024
View(cambios_por_estacion)

# Serie temporal por año por estacion ----

# Asegurar que la columna fecha esta en formato Date
data$date <- as.Date(data$date)

# Crear columna de a?o
data$year <- year(data$date)

# Promedio anual por estacion ----
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
) + scale_x_continuous(breaks = 2015:2024) +
theme_classic() +
theme(
axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
axis.text.y = element_text(size = 13),
strip.text = element_text(size = 12)
)

# Guardar plot
serie_temporal_anual
dir <- paste("D:/Josefina/Proyectos/Tesis/",estacion,"/plots/",sep="")
getwd()
setwd(dir)
ggsave(filename = paste(dir,"Serie_temporal_anual.png",sep=""),
plot = serie_temporal_anual,
width = 8,
height = 4,
dpi = 500
)

# Crear columna con nombre del mes en ingles y completo
datos_boxplot <- data %>%
mutate(
mes = month(date, label = TRUE, abbr = FALSE, locale = "es_ES"),
mes = factor(mes, levels = month.name)
)

# Boxplot con todos los valores diarios por mes
mensual_total <-ggplot(datos_boxplot, aes(x = mes, y = mean)) +
geom_boxplot(
fill = "lightblue",
color = "black",
outlier.shape = 21,         # circulo con borde
outlier.size = 1.5,         # m?s peque?os
outlier.stroke = 0.3,       # grosor del borde
outlier.fill = NA,          # sin relleno
outlier.colour = "black"    # color del borde
)+
labs(x = "Month", y = "Daily PM2.5") +
theme_classic() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Otro plot
datos_boxplot <- data %>%
mutate(
mes = month(date, label = TRUE, abbr = FALSE, locale = "es_ES"),
mes = factor(mes, levels = c("enero", "febrero", "marzo", "abril", "mayo", "junio",
"julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")))

# Boxplot sin outliers y con meses en espa?ol
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
labs( x = " ", y = "  ") +
theme_classic() + theme(axis.text.x =
element_text(angle = 45, hjust = 1, size = 12),
axis.text.y = element_text(size = 13),
strip.text = element_text(size = 12))
#Plot
mensual_total
# Guardar plot
dir <- paste("D:/Josefina/Proyectos/Tesis/",estacion,"/plots/",sep="")
getwd()
setwd(dir)
ggsave(
filename = paste(dir,"BoxPlot_mensual.png",sep=""),
plot = mensual_total ,
width = 8,
height = 4,
dpi = 500
)

#Preparacion de datos ----
# Seleccion de variables con el Factor VIF

# biblioteca para vif
library(car)
#  --        01. Preparacion de los datos: seleccion de variables
estacion <- "BA"
data_com <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/", 
estacion, "/proceed/merge_tot/", estacion, "_merge_comp.csv", sep = ""))
names(data_com)
#Poner en formato la fecha
data_com$date <- as.POSIXct(as.character(data_com$date), format = "%Y-%m-%d")
# Agregamos numero de dia
data_com$dayWeek <- wday(data_com$date, week_start = 1)
unique(year(data_com$date))
# lO volvemos a guardar porque no lo teniamos al weekday
write.csv(data_com,paste("D:/Josefina/Proyectos/ProyectoChile/",
estacion, "/proceed/merge_tot/", estacion, "_merge_comp.csv", sep = ""))

# Nos quedamos solo con los datos 2015-2023
data_com <- data_com[year(data_com$date) != 2024,]
# Verificamos
unique(year(data_com$date))

#Generamos modelo lineal multiple con todas las variables  (17 Vars)
modelo <- lm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia +
OCSMASS_dia + t2m_mean + SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean +
sp_mean + d2m_mean  + v10_mean + u10_mean + tp_mean + DEM + dayWeek,
data = data_com)


vif(modelo)
# Esto te devuelve una tabla con el VIF para cada variable. 
#Regla general:
# VIF = 1: no hay colinealidad
# VIF entre 5 y 10: hay cierta colinealidad, ojo
# VIF > 10: colinealidad severa 

# Ordenamos VIF ----
sort(vif(modelo), decreasing = TRUE)
df <- data.frame(vif(modelo))
View(df)

car::vif(modelo)
summary(modelo)


# Plot VIF ----
# Crear dataframe con valores de VIF antes y despues
vif_data <- data.frame(
Centro = c("SP", "ST", "BA", "MD", "MX"),
VIF_Antes = c(9.78, 14.74, 11.44, 22.46, 7.83),
VIF_Despues = c(3.25, 7.34, 3.12, 9.92, 3.70)
)

# Convertir a formato largo para ggplot
vif_long <- vif_data %>%
pivot_longer(cols = c("VIF_Antes", "VIF_Despues"),
names_to = "Estado", values_to = "VIF")

# Definir el orden deseado de los centros
vif_long$Centro <- factor(vif_long$Centro,
levels = c("SP", "ST", "BA", "MD", "MX"))

# Renombrar para presentacion
vif_long$Estado <- recode(vif_long$Estado,
"VIF_Antes" = "Antes de depuración",
"VIF_Despues" = "Después de depuración")
#Plot
ggplot() +
geom_bar(data = vif_long, aes(x = Centro, y = VIF, fill = Estado), stat = "identity", position = "dodge") +
geom_hline(yintercept = 10, color = "black", linetype = "dashed") +
labs(x = "sitio",
y = "Valor maximo de VIF",
fill = "Estado") +
scale_y_continuous(limits = c(0, 25)) +
theme_classic() +
theme(legend.position = "none") +
scale_fill_manual(values = c("Antes de depuracion" = "#225ea8",
"Despues de depuracion" = "#e7298a"))

# Boxplot por ciudad ----
data_tot <- read.csv("D:/Josefina/Proyectos/Tesis/TOT/proceed/
estaciones/TOT_estaciones.csv")

data_tot$date <- as.POSIXct(as.character(data_tot$date),
format = "%d/%m/%Y")
data_tot <- data_tot[complete.cases(data_tot$mean),]
data_tot <- data_tot[data_tot$mean != 0,]
data_tot <- data_tot[data_tot$mean >0,]
data_tot <- data_tot[(year(data_tot$date)) >= 2015,]
data_tot <- data_tot[(year(data_tot$date)) < 2025,]
names(data_tot)

data_tot$ciudad <- factor(data_tot$sitio,
levels = c("SP", "ST", "BA", "MD", "MX"))

#Plot
ggplot() +
geom_boxplot(data = data_tot,
mapping = aes(x = ciudad, y = mean, fill = ciudad), outlier.size = 0.6) +
labs(x = "sitio", y = expression(PM[2.5]~(mu*g/m^3)),
fill = "sitio") + scale_fill_manual(values =
c("#005a32", "#fd8d3c", "#99000d", "#023858", "#ce1256")) +
theme_classic()

# corroboramos plot por las dudas que el factor no este bien hecho
ggplot() +
geom_boxplot(data_tot, mapping=aes (x = sitio, y = mean,fill = "#023858")) +
labs(title = "Distribucion de PM2.5 por sitio", x = "sitio",
y = "Concentracion de PM2.5 (ug/m?)") + theme_classic()

# Serie temporal anual por cada ciudad ----
# Agregar columna a?o
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
"ST" = "#fd8d3c", "BA" = "#99000d", "MD" = "#023858", "MX" = "#ce1256")) +
scale_y_continuous(limits = c(10, 35)) +
scale_x_continuous(breaks = 2015:2024)+
labs(
x = "Año", y = expression(PM[2.5]~(mu*g/m^3))) +
theme_classic() +
theme(axis.text.x = element_text(angle = 45, hjust = 1),
legend.position = "none")

# Para ver si superan o no el umbral anual del OMS
annual_means_2024 <- annual_means[annual_means$year == 2024,]

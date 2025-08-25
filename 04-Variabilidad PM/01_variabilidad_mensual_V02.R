library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)

# Parámetros y rutas
estacion <- "SP"
base_path <- "D:/Josefina/Proyectos/Tesis/"

# Carga y preparación de datos
data_SP <- read.csv(paste0(base_path, estacion, "/resultados/merge_Prediccion_Real/", estacion, "_merge_01-XGB-CV-M1-200525-", estacion, ".csv"))
data_SP$date <- as.Date(as.POSIXct(data_SP$date, format = "%Y-%m-%d"))
data_SP$month <- month(data_SP$date)
data_SP <- data_SP[year(data_SP$date) == 2024, ]

data_SP_sAOD <- read.csv(paste0(base_path, estacion, "/resultados/merge_Prediccion_Real/", estacion, "_merge_02-XGB-CV-1-210525-sAOD-", estacion, ".csv"))
data_SP_sAOD$date <- as.Date(as.POSIXct(data_SP_sAOD$date, format = "%Y-%m-%d"))
data_SP_sAOD$month <- month(data_SP_sAOD$date)
data_SP_sAOD <- data_SP_sAOD[year(data_SP_sAOD$date) == 2024, ]

data_SP_sAOD<-data_SP_sAOD[data_SP_sAOD$valor_raster>0,]
data_SP<-data_SP[data_SP$valor_raster>0,]


# Promedios mensuales
data_AOD_mensual_SP <- data_SP %>%
  group_by(month) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_sAOD_mensual_SP <- data_SP_sAOD %>%
  group_by(month) %>%
  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

# Calcular media y desviación estándar para mean_pm25_sAOD
stats_pm25_sAOD <- data_SP_sAOD %>%
  group_by(month) %>%
  summarise(
    mean_pm25 = mean(mean, na.rm = TRUE),
    sd_pm25 = sd(mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ymin = mean_pm25 - sd_pm25,
    ymax = mean_pm25 + sd_pm25,
    month = factor(month, levels = 1:12,
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
  )

# Merge de datos para graficar
data_merged <- left_join(data_sAOD_mensual_SP, data_AOD_mensual_SP, by = "month")

data_long_SP <- data_merged %>%
  pivot_longer(
    cols = c(mean_pm25_sAOD, mean_valor_raster_AOD, mean_valor_raster_sAOD),
    names_to = "variable",
    values_to = "valor"
  )

data_long_SP$month <- factor(
  data_long_SP$month,
  levels = 1:12,
  labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
)

# Preparar data para leyenda de banda y líneas de desviación estándar
ribbon_df <- stats_pm25_sAOD %>%
  mutate(variable = "±1 Desv. Estándar")

ggplot() +
  # Banda gris (sin leyenda)
  geom_ribbon(
    data = ribbon_df,
    aes(x = month, ymin = ymin, ymax = ymax),
    fill = "grey70",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  # Línea inferior punteada (sí aparece en leyenda como referencia)
  geom_line(
    data = ribbon_df,
    aes(x = month, y = ymin, group = 1),
    color = "grey40",
    size = 0.7,
    linetype = "dashed"
  ) +
  # Línea superior punteada (solo para cerrar la banda, sin leyenda)
  geom_line(
    data = ribbon_df,
    aes(x = month, y = ymax, group = 1),
    color = "grey40",
    size = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  # Líneas y puntos de las variables principales
  geom_line(
    data = data_long_SP,
    aes(x = month, y = valor, color = variable, group = variable),
    size = 0.8
  ) +
  geom_point(
    data = data_long_SP,
    aes(x = month, y = valor, color = variable, group = variable),
    size = 1.5
  ) +
  # Colores para las líneas
  scale_color_manual(
    values = c(
      "mean_pm25_sAOD" = "black",  
      "mean_pm25_sAOD" = "black",
      "mean_valor_raster_AOD" = "#a1d99b", 
      "mean_valor_raster_sAOD" = "#41ab5d",
      "sd_pm25" = "grey40"
    ),
    labels = c(
      "mean_pm25_sAOD" = "Mediciones",
      "mean_pm25_sAOD" = "Mediciones",
      "mean_valor_raster_AOD" = "AOD",
      "mean_valor_raster_sAOD" = "sAOD",
      "sd_pm25" = "sd"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, by = 20)
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.12, 0.85),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

#######################################################
#######################################################


library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)

# Parámetros y rutas
estacion <- "CH"
base_path <- "D:/Josefina/Proyectos/Tesis/"

# Carga y preparación de datos
data_CH <- read.csv(paste0(base_path, estacion, "/resultados/merge_Prediccion_Real/", estacion, "_merge_01-XGB-CV-M1-190625-", estacion, ".csv"))

data_CH$date <- as.Date(as.POSIXct(data_CH$date, format = "%Y-%m-%d"))
data_CH$month <- month(data_CH$date)
data_CH <- data_CH[year(data_CH$date) == 2024, ]

data_CH_sAOD <- read.csv(paste0(base_path, estacion, "/resultados/merge_Prediccion_Real/", estacion, "_merge_02-XGB-CV-M1-230625-sAOD-", estacion, ".csv"))
data_CH_sAOD$date <- as.Date(as.POSIXct(data_CH_sAOD$date, format = "%Y-%m-%d"))
data_CH_sAOD$month <- month(data_CH_sAOD$date)
data_CH_sAOD <- data_CH_sAOD[year(data_CH_sAOD$date) == 2024, ]
data_CH$mean <- data_CH$Registros.completos
data_CH_sAOD$mean <- data_CH_sAOD$Registros.completos
data_CH_sAOD<-data_CH_sAOD[data_CH_sAOD$valor_raster>0,]
data_CH<-data_CH[data_CH$valor_raster>0,]

# Promedios mensuales
data_AOD_mensual_CH <- data_CH %>%
  group_by(month) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_sAOD_mensual_CH <- data_CH_sAOD %>%
  group_by(month) %>%
  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

# Calcular media y desviación estándar para mean_pm25_sAOD
stats_pm25_sAOD <- data_CH_sAOD %>%
  group_by(month) %>%
  summarise(
    mean_pm25 = mean(mean, na.rm = TRUE),
    sd_pm25 = sd(mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ymin = mean_pm25 - sd_pm25,
    ymax = mean_pm25 + sd_pm25,
    month = factor(month, levels = 1:12,
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
  )

# Merge de datos para graficar
data_merged <- left_join(data_sAOD_mensual_CH, data_AOD_mensual_CH, by = "month")

data_long_CH <- data_merged %>%
  pivot_longer(
    cols = c(mean_pm25_sAOD, mean_valor_raster_AOD, mean_valor_raster_sAOD),
    names_to = "variable",
    values_to = "valor"
  )

data_long_CH$month <- factor(
  data_long_CH$month,
  levels = 1:12,
  labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
)

# Preparar data para leyenda de banda y líneas de desviación estándar
ribbon_df <- stats_pm25_sAOD %>%
  mutate(variable = "±1 Desv. Estándar")

ggplot() +
  # Banda gris (sin leyenda)
  geom_ribbon(
    data = ribbon_df,
    aes(x = month, ymin = ymin, ymax = ymax),
    fill = "grey70",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  # Línea inferior punteada (sí aparece en leyenda como referencia)
  geom_line(
    data = ribbon_df,
    aes(x = month, y = ymin, group = 1),
    color = "grey40",
    size = 0.7,
    linetype = "dashed"
  ) +
  # Línea superior punteada (solo para cerrar la banda, sin leyenda)
  geom_line(
    data = ribbon_df,
    aes(x = month, y = ymax, group = 1),
    color = "grey40",
    size = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  # Líneas y puntos de las variables principales
  geom_line(
    data = data_long_CH,
    aes(x = month, y = valor, color = variable, group = variable),
    size = 0.8
  ) +
  geom_point(
    data = data_long_CH,
    aes(x = month, y = valor, color = variable, group = variable),
    size = 1.5
  ) +
  # Colores para las líneas

  scale_color_manual(
    values = c(
      "mean_pm25_sAOD" = "black",  
      "mean_pm25_sAOD" = "black",
      "mean_valor_raster_AOD" = "#feb24c",  
      "mean_valor_raster_sAOD" = "#fc4e2a" ,
      "sd_pm25" = "grey40"
    ),
    labels = c(
      "mean_pm25_sAOD" = "Mediciones",
      "mean_pm25_sAOD" = "Mediciones",
      "mean_valor_raster_AOD" = "AOD",
      "mean_valor_raster_sAOD" = "sAOD",
      "sd_pm25" = "sd"
    )
   ) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, by = 20)
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.12, 0.85),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )




####################################################
######################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)

# Parámetros y rutas
estacion <- "BA"
base_path <- "D:/Josefina/Proyectos/Tesis/"

# Carga y preparación de datos
data_BA <- read.csv(paste0(base_path, estacion, "/resultados/merge_Prediccion_Real/", estacion, "_merge_01-ET-CV-M1-170625-", estacion, ".csv"))
data_BA$date <- as.Date(as.POSIXct(data_BA$date, format = "%Y-%m-%d"))
data_BA$month <- month(data_BA$date)
data_BA <- data_BA[year(data_BA$date) == 2024, ]

data_BA_sAOD <- read.csv(paste0(base_path, estacion, "/resultados/merge_Prediccion_Real/", estacion, "_merge_02-ET-CV-M1-230625-sAOD-", estacion, ".csv"))
data_BA_sAOD$date <- as.Date(as.POSIXct(data_BA_sAOD$date, format = "%Y-%m-%d"))
data_BA_sAOD$month <- month(data_BA_sAOD$date)
data_BA_sAOD <- data_BA_sAOD[year(data_BA_sAOD$date) == 2024, ]

data_BA_sAOD<-data_BA_sAOD[data_BA_sAOD$valor_raster>0,]
data_BA<-data_BA[data_BA$valor_raster>0,]


# Promedios mensuales
data_AOD_mensual_BA <- data_BA %>%
  group_by(month) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_sAOD_mensual_BA <- data_BA_sAOD %>%
  group_by(month) %>%
  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

# Calcular media y desviación estándar para mean_pm25_sAOD
stats_pm25_sAOD <- data_BA_sAOD %>%
  group_by(month) %>%
  summarise(
    mean_pm25 = mean(mean, na.rm = TRUE),
    sd_pm25 = sd(mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ymin = mean_pm25 - sd_pm25,
    ymax = mean_pm25 + sd_pm25,
    month = factor(month, levels = 1:12,
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
  )

# Merge de datos para graficar
data_merged <- left_join(data_sAOD_mensual_BA, data_AOD_mensual_BA, by = "month")

data_long_BA <- data_merged %>%
  pivot_longer(
    cols = c(mean_pm25_sAOD, mean_valor_raster_AOD, mean_valor_raster_sAOD),
    names_to = "variable",
    values_to = "valor"
  )

data_long_BA$month <- factor(
  data_long_BA$month,
  levels = 1:12,
  labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
)

# Preparar data para leyenda de banda y líneas de desviación estándar
ribbon_df <- stats_pm25_sAOD %>%
  mutate(variable = "±1 Desv. Estándar")

ggplot() +
  # Banda gris (sin leyenda)
  geom_ribbon(
    data = ribbon_df,
    aes(x = month, ymin = ymin, ymax = ymax),
    fill = "grey70",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  # Línea inferior punteada (sí aparece en leyenda como referencia)
  geom_line(
    data = ribbon_df,
    aes(x = month, y = ymin, group = 1),
    color = "grey40",
    size = 0.7,
    linetype = "dashed"
  ) +
  # Línea superior punteada (solo para cerrar la banda, sin leyenda)
  geom_line(
    data = ribbon_df,
    aes(x = month, y = ymax, group = 1),
    color = "grey40",
    size = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  # Líneas y puntos de las variables principales
  geom_line(
    data = data_long_BA,
    aes(x = month, y = valor, color = variable, group = variable),
    size = 0.8
  ) +
  geom_point(
    data = data_long_BA,
    aes(x = month, y = valor, color = variable, group = variable),
    size = 1.5
  ) +
  # Colores para las líneas

  scale_color_manual(
    values = c(
      "mean_pm25_sAOD" = "black",  
      "mean_pm25_sAOD" = "black",
      "mean_valor_raster_AOD" = "#fb6a4a", 
      "mean_valor_raster_sAOD" = "#99000d" ,
      "sd_pm25" = "grey40"
    ),
    labels = c(
      "mean_pm25_sAOD" = "Mediciones",
      "mean_pm25_sAOD" = "Mediciones",
      "mean_valor_raster_AOD" = "AOD",
      "mean_valor_raster_sAOD" = "sAOD",
      "sd_pm25" = "sd"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, by = 20)
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.12, 0.85),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

#######################################################
#######################################################

# Parámetros y rutas
estacion <- "MD"
base_path <- "D:/Josefina/Proyectos/Tesis/"

# Carga y preparación de datos
data_MD <- read.csv(paste0(base_path, estacion, "/resultados/merge_Prediccion_Real/", estacion, "_merge_01-ET-CV-M1-260525-", estacion, ".csv"))
data_MD$date <- as.Date(as.POSIXct(data_MD$date, format = "%Y-%m-%d"))
data_MD$month <- month(data_MD$date)
data_MD <- data_MD[year(data_MD$date) == 2024, ]


data_MD_sAOD <- read.csv(paste0(base_path, estacion, "/resultados/merge_Prediccion_Real/", estacion, "_merge_01-ET-CV-M1-270525-sAOD-", estacion, ".csv"))
data_MD_sAOD$date <- as.Date(as.POSIXct(data_MD_sAOD$date, format = "%Y-%m-%d"))
data_MD_sAOD$month <- month(data_MD_sAOD$date)
data_MD_sAOD <- data_MD_sAOD[year(data_MD_sAOD$date) == 2024, ]

data_MD_sAOD<-data_MD_sAOD[data_MD_sAOD$valor_raster>0,]
data_MD<-data_MD[data_MD$valor_raster>0,]


# Promedios mensuales
data_AOD_mensual_MD <- data_MD %>%
  group_by(month) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_sAOD_mensual_MD <- data_MD_sAOD %>%
  group_by(month) %>%
  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

# Calcular media y desviación estándar para mean_pm25_sAOD
stats_pm25_sAOD <- data_MD_sAOD %>%
  group_by(month) %>%
  summarise(
    mean_pm25 = mean(mean, na.rm = TRUE),
    sd_pm25 = sd(mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ymin = mean_pm25 - sd_pm25,
    ymax = mean_pm25 + sd_pm25,
    month = factor(month, levels = 1:12,
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
  )

# Merge de datos para graficar
data_merged <- left_join(data_sAOD_mensual_MD, data_AOD_mensual_MD, by = "month")

data_long_MD <- data_merged %>%
  pivot_longer(
    cols = c(mean_pm25_sAOD, mean_valor_raster_AOD, mean_valor_raster_sAOD),
    names_to = "variable",
    values_to = "valor"
  )

data_long_MD$month <- factor(
  data_long_MD$month,
  levels = 1:12,
  labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
)

# Preparar data para leyenda de banda y líneas de desviación estándar
ribbon_df <- stats_pm25_sAOD %>%
  mutate(variable = "±1 Desv. Estándar")

ggplot() +
  # Banda gris (sin leyenda)
  geom_ribbon(
    data = ribbon_df,
    aes(x = month, ymin = ymin, ymax = ymax),
    fill = "grey70",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  # Línea inferior punteada (sí aparece en leyenda como referencia)
  geom_line(
    data = ribbon_df,
    aes(x = month, y = ymin, group = 1),
    color = "grey40",
    size = 0.7,
    linetype = "dashed"
  ) +
  # Línea superior punteada (solo para cerrar la banda, sin leyenda)
  geom_line(
    data = ribbon_df,
    aes(x = month, y = ymax, group = 1),
    color = "grey40",
    size = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  # Líneas y puntos de las variables principales
  geom_line(
    data = data_long_MD,
    aes(x = month, y = valor, color = variable, group = variable),
    size = 0.8
  ) +
  geom_point(
    data = data_long_MD,
    aes(x = month, y = valor, color = variable, group = variable),
    size = 1.5
  ) +
  # Colores para las líneas

  scale_color_manual(
    values = c(
      "mean_pm25_sAOD" = "black",  
      "mean_pm25_sAOD" = "black",
      "mean_valor_raster_AOD" = "#9ecae1",
      "mean_valor_raster_sAOD" = "#2171b5",
      "sd_pm25" = "grey40"
    ),
    labels = c(
      "mean_pm25_sAOD" = "Mediciones",
      "mean_pm25_sAOD" = "Mediciones",
      "mean_valor_raster_AOD" = "AOD",
      "mean_valor_raster_sAOD" = "sAOD",
      "sd_pm25" = "sd"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, by = 20)
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.12, 0.85),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

#######################################################
#######################################################


# Parámetros y rutas
estacion <- "MX"
base_path <- "D:/Josefina/Proyectos/Tesis/"

# Carga y preparación de datos
data_MX <- read.csv(paste0(base_path, estacion, "/resultados/merge_Prediccion_Real/", estacion, "_merge_01-XGB-CV-M1-290525-", estacion, ".csv"))
data_MX$date <- as.Date(as.POSIXct(data_MX$date, format = "%Y-%m-%d"))
data_MX$month <- month(data_MX$date)
data_MX <- data_MX[year(data_MX$date) == 2024, ]


data_MX_sAOD <- read.csv(paste0(base_path, estacion, "/resultados/merge_Prediccion_Real/", estacion, "_merge_02-XGB-CV-M1-230625-sAOD-", estacion, ".csv"))
data_MX_sAOD$date <- as.Date(as.POSIXct(data_MX_sAOD$date, format = "%Y-%m-%d"))
data_MX_sAOD$month <- month(data_MX_sAOD$date)
data_MX_sAOD <- data_MX_sAOD[year(data_MX_sAOD$date) == 2024, ]

data_MX_sAOD<-data_MX_sAOD[data_MX_sAOD$valor_raster>0,]
data_MX<-data_MX[data_MX$valor_raster>0,]


# Promedios mensuales
data_AOD_mensual_MX <- data_MX %>%
  group_by(month) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_sAOD_mensual_MX <- data_MX_sAOD %>%
  group_by(month) %>%
  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

# Calcular media y desviación estándar para mean_pm25_sAOD
stats_pm25_sAOD <- data_MX_sAOD %>%
  group_by(month) %>%
  summarise(
    mean_pm25 = mean(mean, na.rm = TRUE),
    sd_pm25 = sd(mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ymin = mean_pm25 - sd_pm25,
    ymax = mean_pm25 + sd_pm25,
    month = factor(month, levels = 1:12,
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
  )

# Merge de datos para graficar
data_merged <- left_join(data_sAOD_mensual_MX, data_AOD_mensual_MX, by = "month")

data_long_MX <- data_merged %>%
  pivot_longer(
    cols = c(mean_pm25_sAOD, mean_valor_raster_AOD, mean_valor_raster_sAOD),
    names_to = "variable",
    values_to = "valor"
  )

data_long_MX$month <- factor(
  data_long_MX$month,
  levels = 1:12,
  labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
)

# Preparar data para leyenda de banda y líneas de desviación estándar
ribbon_df <- stats_pm25_sAOD %>%
  mutate(variable = "±1 Desv. Estándar")

ggplot() +
  # Banda gris (sin leyenda)
  geom_ribbon(
    data = ribbon_df,
    aes(x = month, ymin = ymin, ymax = ymax),
    fill = "grey70",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  # Línea inferior punteada (sí aparece en leyenda como referencia)
  geom_line(
    data = ribbon_df,
    aes(x = month, y = ymin, group = 1),
    color = "grey40",
    size = 0.7,
    linetype = "dashed"
  ) +
  # Línea superior punteada (solo para cerrar la banda, sin leyenda)
  geom_line(
    data = ribbon_df,
    aes(x = month, y = ymax, group = 1),
    color = "grey40",
    size = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  # Líneas y puntos de las variables principales
  geom_line(
    data = data_long_MX,
    aes(x = month, y = valor, color = variable, group = variable),
    size = 0.8
  ) +
  geom_point(
    data = data_long_MX,
    aes(x = month, y = valor, color = variable, group = variable),
    size = 1.5
  ) +
  # Colores para las líneas
  scale_color_manual(
    values = c(
      "mean_pm25_sAOD" = "black",  
      "mean_pm25_sAOD" = "black",
      "mean_valor_raster_AOD" = "#807dba", 
      "mean_valor_raster_sAOD" = "#810f7c",
      "sd_pm25" = "grey40"
    ),
    labels = c(
      "mean_pm25_sAOD" = "Mediciones",
      "mean_pm25_sAOD" = "Mediciones",
      "mean_valor_raster_AOD" = "AOD",
      "mean_valor_raster_sAOD" = "sAOD",
      "sd_pm25" = "sd"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, by = 20)
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.12, 0.85),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

#######################################################
#######################################################

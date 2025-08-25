
#########################################################################
#########################################################################
#########################################################################
###$ SP
#### PREDICCOIN 2024 Con AOD / 
estacion <- "SP"
data_SP <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-XGB-CV-M1-200525-",estacion,".csv",sep=""))
data_SP$date <- as.Date(as.POSIXct(data_SP$date, format = "%Y-%m-%d"))#


data_SP_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-XGB-CV-1-210525-sAOD-",estacion,".csv",sep=""))
data_SP_sAOD$date <- as.Date(as.POSIXct(data_SP_sAOD$date, format = "%Y-%m-%d"))#


data_SP <- data_SP[year(data_SP$date)==2024,]
data_SP_sAOD <- data_SP_sAOD[year(data_SP_sAOD$date)==2024,]
unique(year(data_SP$date))
unique(year(data_SP_sAOD$date))


# Promedios diarios
data_AOD_diario_SP <- data_SP %>%
  group_by(date) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_sAOD_diario_SP <- data_SP_sAOD %>%
 group_by(date) %>%

  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_AOD_diario_SP$date <- as.Date(as.POSIXct(data_AOD_diario_SP$date, format = "%Y-%m-%d"))#
data_sAOD_diario_SP$date <- as.Date(as.POSIXct(data_sAOD_diario_SP$date, format = "%Y-%m-%d"))#

data_AOD_diario_SP <- data_AOD_diario_SP[year(data_AOD_diario_SP$date)==2024,]
data_sAOD_diario_SP <- data_sAOD_diario_SP[year(data_sAOD_diario_SP$date)==2024,]


data_merged <- left_join(data_sAOD_diario_SP, data_AOD_diario_SP, by = "date")
unique(year(data_merged$date))






#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"

data_long_SP <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_sAOD),#mean_valor_raster_AOD,
    names_to = "variable",
    values_to = "valor"
  )
library(ggplot2)
library(scales)  # por si necesitás formatos personalizados

 ggplot(data_long_SP, aes(x = date, y = valor, color = variable)) +
#  ggplot(data_long_SP, aes(x = month, y = valor, color = variable)) +
  geom_line(size = 0.5) +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "#66c2a4",    
      mean_valor_raster_sAOD = "#00441b"
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_sAOD = "Predicción"
    )
  ) +
  # scale_x_date(
  #   date_breaks = "2 month",           # un tick por mes
  #   date_labels = "%B",                # formato: "ene", "feb", etc.
  #   limits = as.Date(c("2024-01-01", "2024-12-31"))  # límites del eje
  # ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)  # << esta línea define los saltos
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
## metricas
mean(data_AOD_diario_SP$mean_pm25_AOD)
sd(data_AOD_diario_SP$mean_pm25_AOD)

mean(data_AOD_diario_SP$mean_valor_raster_AOD)
sd(data_AOD_diario_SP$mean_valor_raster_AOD)
#####
mean(data_sAOD_diario_SP$mean_pm25_sAOD)
sd(data_sAOD_diario_SP$mean_pm25_sAOD)

mean(data_sAOD_diario_SP$mean_valor_raster_sAOD)
sd(data_sAOD_diario_SP$mean_valor_raster_sAOD)

summary(data_AOD_diario_SP$mean_pm25_AOD)
summary(data_AOD_diario_SP$mean_valor_raster_AOD)


data_merged2 <-data_merged[complete.cases(data_merged),]
# Medicion
mean(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
sd(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
#con AOD
mean(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
#Saod
mean(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)


#########################################################################
#########################################################################
###$ CH
#### PREDICCOIN 2024 Con AOD / 
estacion <- "CH"
data_CH <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-XGB-CV-M1-190625-",estacion,".csv",sep=""))
data_CH$date <- as.Date(as.POSIXct(data_CH$date, format = "%Y-%m-%d"))#
data_CH$mean <- data_CH$Registros.completos
data_CH_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-XGB-CV-M1-230625-sAOD-",estacion,".csv",sep=""))
data_CH_sAOD$date <- as.Date(as.POSIXct(data_CH_sAOD$date, format = "%Y-%m-%d"))#
data_CH_sAOD$mean <- data_CH_sAOD$Registros.completos
# Promedios diarios
data_AOD_diario_CH <- data_CH %>%
  group_by(date) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_sAOD_diario_CH <- data_CH_sAOD %>%
  group_by(date) %>%
  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_AOD_diario_CH$date <- as.Date(as.POSIXct(data_AOD_diario_CH$date, format = "%Y-%m-%d"))#
data_sAOD_diario_CH$date <- as.Date(as.POSIXct(data_sAOD_diario_CH$date, format = "%Y-%m-%d"))#

data_AOD_diario_CH <- data_AOD_diario_CH[year(data_AOD_diario_CH$date)==2024,]
data_sAOD_diario_CH <- data_sAOD_diario_CH[year(data_sAOD_diario_CH$date)==2024,]


data_merged <- left_join(data_sAOD_diario_CH, data_AOD_diario_CH, by = "date")

unique(year(data_merged$date))
#Oscuro Prediccion - Claro con mediccion
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",

data_long_CH <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_sAOD),#,
    names_to = "variable",
    values_to = "valor"
  )


ggplot(data_long_CH, aes(x = date, y = valor, color = variable)) +
  geom_line(size = 0.5) +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "#feb24c",    
      mean_valor_raster_sAOD = "#fc4e2a"
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_sAOD = "Predicción"
    )
  ) +
  scale_x_date(
    date_breaks = "2 month",           # un tick por mes
    date_labels = "%B",                # formato: "ene", "feb", etc.
    limits = as.Date(c("2024-01-01", "2024-12-31"))  # límites del eje
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)  # << esta línea define los saltos
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


## metricas
mean(data_AOD_diario_CH$mean_pm25_AOD)
sd(data_AOD_diario_CH$mean_pm25_AOD)
mean(data_AOD_diario_CH$mean_valor_raster_AOD)
sd(data_AOD_diario_CH$mean_valor_raster_AOD)

summary(data_AOD_diario_CH$mean_pm25_AOD)
summary(data_AOD_diario_CH$mean_valor_raster_AOD)


data_merged2 <-data_merged[complete.cases(data_merged),]

# Medicion
mean(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
sd(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
#con AOD
mean(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
#Saod
mean(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)


#########################################################################
#########################################################################
###$ BA
#### PREDICCOIN 2024 Con AOD / 
estacion <- "BA"
data_BA <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-170625-",estacion,".csv",sep=""))
data_BA$date <- as.Date(as.POSIXct(data_BA$date, format = "%Y-%m-%d"))#

data_BA_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-ET-CV-M1-230625-sAOD-",estacion,".csv",sep=""))
data_BA_sAOD$date <- as.Date(as.POSIXct(data_BA_sAOD$date, format = "%Y-%m-%d"))#

# Promedios diarios
data_AOD_diario_BA <- data_BA %>%
  group_by(date) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_sAOD_diario_BA <- data_BA_sAOD %>%
  group_by(date) %>%
  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_AOD_diario_BA$date <- as.Date(as.POSIXct(data_AOD_diario_BA$date, format = "%Y-%m-%d"))#
data_sAOD_diario_BA$date <- as.Date(as.POSIXct(data_sAOD_diario_BA$date, format = "%Y-%m-%d"))#

data_AOD_diario_BA <- data_AOD_diario_BA[year(data_AOD_diario_BA$date)==2024,]
data_sAOD_diario_BA <- data_sAOD_diario_BA[year(data_sAOD_diario_BA$date)==2024,]


data_merged <- left_join(data_sAOD_diario_BA, data_AOD_diario_BA, by = "date")

unique(year(data_merged$date))
#Oscuro Prediccion - Claro con mediccion
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",

data_long_BA <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_sAOD),#,
    names_to = "variable",
    values_to = "valor"
  )


ggplot(data_long_BA, aes(x = date, y = valor, color = variable)) +
  geom_line(size = 0.5) +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "#fb6a4a",
      mean_valor_raster_sAOD = "#99000d"
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_sAOD = "Predicción"
    )
  ) +
  scale_x_date(
    date_breaks = "1 month",           # un tick por mes
    date_labels = "%B",                # formato: "ene", "feb", etc.
    limits = as.Date(c("2024-01-01", "2024-05-01"))  # límites del eje
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)  # << esta línea define los saltos
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

## metricas
mean(data_AOD_diario_BA$mean_pm25_AOD)
sd(data_AOD_diario_BA$mean_pm25_AOD)
mean(data_AOD_diario_BA$mean_valor_raster_AOD)
sd(data_AOD_diario_BA$mean_valor_raster_AOD)

summary(data_AOD_diario_BA$mean_pm25_AOD)
summary(data_AOD_diario_BA$mean_valor_raster_AOD)

data_merged2 <-data_merged[complete.cases(data_merged),]

# Medicion
mean(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
sd(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
#con AOD
mean(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
#Saod
mean(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)
#########################################################################
#########################################################################
###$ MD
#### PREDICCOIN 2024 Con AOD / 
estacion <- "MD"
data_MD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-260525-",estacion,".csv",sep=""))
data_MD$date <- as.Date(as.POSIXct(data_MD$date, format = "%Y-%m-%d"))#

data_MD_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-270525-sAOD-",estacion,".csv",sep=""))
data_MD_sAOD$date <- as.Date(as.POSIXct(data_MD_sAOD$date, format = "%Y-%m-%d"))#

# Promedios diarios
data_AOD_diario_MD <- data_MD %>%
  group_by(date) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_sAOD_diario_MD <- data_MD_sAOD %>%
  group_by(date) %>%
  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_AOD_diario_MD$date <- as.Date(as.POSIXct(data_AOD_diario_MD$date, format = "%Y-%m-%d"))#
data_sAOD_diario_MD$date <- as.Date(as.POSIXct(data_sAOD_diario_MD$date, format = "%Y-%m-%d"))#

data_AOD_diario_MD <- data_AOD_diario_MD[year(data_AOD_diario_MD$date)==2024,]
data_sAOD_diario_MD <- data_sAOD_diario_MD[year(data_sAOD_diario_MD$date)==2024,]


data_merged <- left_join(data_sAOD_diario_MD, data_AOD_diario_MD, by = "date")

unique(year(data_merged$date))
#Oscuro Prediccion - Claro con mediccion
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dMD",

data_long_MD <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_sAOD),#,
    names_to = "variable",
    values_to = "valor"
  )


ggplot(data_long_MD, aes(x = date, y = valor, color = variable)) +
  geom_line(size = 0.5) +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "#4292c6",
      mean_valor_raster_sAOD = "#023858"
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_sAOD = "Predicción"
    )
  ) +
  scale_x_date(
    date_breaks = "2 month",           # un tick por mes
    date_labels = "%B",                # formato: "ene", "feb", etc.
    limits = as.Date(c("2024-01-01", "2024-12-31"))  # límites del eje
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)  # << esta línea define los saltos
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
   # legend.position = "none",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


## metricas
mean(data_AOD_diario_MD$mean_pm25_AOD)
sd(data_AOD_diario_MD$mean_pm25_AOD)
mean(data_AOD_diario_MD$mean_valor_raster_AOD)
sd(data_AOD_diario_MD$mean_valor_raster_AOD)

summary(data_AOD_diario_MD$mean_pm25_AOD)
summary(data_AOD_diario_MD$mean_valor_raster_AOD)


data_merged2 <-data_merged[complete.cases(data_merged),]

# Medicion
mean(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
sd(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
#con AOD
mean(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
#Saod
mean(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)

#########################################################################
#########################################################################
###$ MX
#### PREDICCOIN 2024 Con AOD / 
estacion <- "MX"
data_MX <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-XGB-CV-M1-290525-",estacion,".csv",sep=""))
data_MX$date <- as.Date(as.POSIXct(data_MX$date, format = "%Y-%m-%d"))#

data_MX_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-XGB-CV-M1-230625-sAOD-",estacion,".csv",sep=""))
data_MX_sAOD$date <- as.Date(as.POSIXct(data_MX_sAOD$date, format = "%Y-%m-%d"))#

# Promedios diarios
data_AOD_diario_MX <- data_MX %>%
  group_by(date) %>%
  summarise(
    mean_pm25_AOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_AOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_sAOD_diario_MX <- data_MX_sAOD %>%
  group_by(date) %>%
  summarise(
    mean_pm25_sAOD = mean(mean, na.rm = TRUE),
    mean_valor_raster_sAOD = mean(valor_raster, na.rm = TRUE),
    .groups = "drop"
  )

data_AOD_diario_MX$date <- as.Date(as.POSIXct(data_AOD_diario_MX$date, format = "%Y-%m-%d"))#
data_sAOD_diario_MX$date <- as.Date(as.POSIXct(data_sAOD_diario_MX$date, format = "%Y-%m-%d"))#

data_AOD_diario_MX <- data_AOD_diario_MX[year(data_AOD_diario_MX$date)==2024,]
data_sAOD_diario_MX <- data_sAOD_diario_MX[year(data_sAOD_diario_MX$date)==2024,]


data_merged <- left_join(data_sAOD_diario_MX, data_AOD_diario_MX, by = "date")

unique(year(data_merged$date))
#Oscuro Prediccion - Claro con mediccion
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dMD",

data_long_MX <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_sAOD),#,
    names_to = "variable",
    values_to = "valor"
  )


ggplot(data_long_MX, aes(x = date, y = valor, color = variable)) +
  geom_line(size = 0.5) +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "#807dba",
      mean_valor_raster_sAOD = "#3f007d"
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_sAOD = "Predicción"
    )
  ) +
  scale_x_date(
    date_breaks = "2 month",           # un tick por mes
    date_labels = "%B",                # formato: "ene", "feb", etc.
    limits = as.Date(c("2024-01-01", "2024-12-31"))  # límites del eje
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)  # << esta línea define los saltos
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

## metricas
mean(data_AOD_diario_MX$mean_pm25_AOD)
sd(data_AOD_diario_MX$mean_pm25_AOD)
mean(data_AOD_diario_MX$mean_valor_raster_AOD)
sd(data_AOD_diario_MX$mean_valor_raster_AOD)

summary(data_AOD_diario_MX$mean_pm25_AOD)
summary(data_AOD_diario_MX$mean_valor_raster_AOD)


data_merged2 <-data_merged[complete.cases(data_merged),]

# Medicion
mean(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
sd(data_merged2$mean_pm25_sAOD,na.rm=TRUE)
#con AOD
mean(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_AOD,na.rm=TRUE)
#Saod
mean(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)
sd(data_merged2$mean_valor_raster_sAOD,na.rm=TRUE)

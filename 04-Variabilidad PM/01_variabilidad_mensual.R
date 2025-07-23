
#########################################################################
#########################################################################
#########################################################################
###$ SP
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",

# SP "#00441b",
#ST "#fc4e2a",  
# BA  "#99000d"  
#MD "#023858", 
# MX "#3f007d", 
#### PREDICCOIN 2024 Con AOD / 
estacion <- "SP"
data_SP <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-XGB-CV-M1-200525-",estacion,".csv",sep=""))
data_SP$date <- as.Date(as.POSIXct(data_SP$date, format = "%Y-%m-%d"))#
data_SP$month <- month(data_SP$date)

data_SP_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-XGB-CV-1-210525-sAOD-",estacion,".csv",sep=""))
data_SP_sAOD$date <- as.Date(as.POSIXct(data_SP_sAOD$date, format = "%Y-%m-%d"))#
data_SP_sAOD$month <- month(data_SP_sAOD$date)

data_SP <- data_SP[year(data_SP$date)==2024,]
data_SP_sAOD <- data_SP_sAOD[year(data_SP_sAOD$date)==2024,]
unique(year(data_SP$date))
unique(year(data_SP_sAOD$date))

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

data_merged <- left_join(data_sAOD_mensual_SP, data_AOD_mensual_SP, by = "month")
#unique(year(data_merged$date))
#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"

data_long_SP <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_AOD,mean_valor_raster_sAOD),
    names_to = "variable",
    values_to = "valor"
  )
library(ggplot2)
library(scales)  # por si necesitás formatos personalizados
data_long_SP$month <- factor(
  data_long_SP$month,
  levels = 1:12,
  labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
)

# ggplot(data_long_SP, aes(x = date, y = valor, color = variable)) +
ggplot(data_long_SP, aes(x = month, y = valor, color = variable, group = variable)) +
  geom_line(size = 0.8) +
  #geom_point() +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "black",   
      mean_valor_raster_AOD = "#a1d99b", 
      mean_valor_raster_sAOD = "#41ab5d"
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_AOD = "AOD",
      mean_valor_raster_sAOD = "sAOD"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 20)  # << esta línea define los saltos
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )
## metricas
mean(data_AOD_mensual_SP$mean_pm25_AOD)
sd(data_AOD_mensual_SP$mean_pm25_AOD)
mean(data_AOD_mensual_SP$mean_valor_raster_AOD)
sd(data_AOD_mensual_SP$mean_valor_raster_AOD)

summary(data_AOD_mensual_SP$mean_pm25_AOD)
summary(data_AOD_mensual_SP$mean_valor_raster_AOD)



#########################################################################
#########################################################################
#########################################################################
###$ CH
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",
#### PREDICCOIN 2024 Con AOD / 
estacion <- "CH"
data_CH <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-XGB-CV-M1-190625-",estacion,".csv",sep=""))
data_CH$date <- as.Date(as.POSIXct(data_CH$date, format = "%Y-%m-%d"))#
data_CH$mean <- data_CH$Registros.completos
data_CH_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-XGB-CV-M1-230625-sAOD-",estacion,".csv",sep=""))
data_CH_sAOD$date <- as.Date(as.POSIXct(data_CH_sAOD$date, format = "%Y-%m-%d"))#
data_CH_sAOD$mean <- data_CH_sAOD$Registros.completos
data_CH$month <- month(data_CH$date)

data_CH_sAOD$month <- month(data_CH_sAOD$date)

data_CH <- data_CH[year(data_CH$date)==2024,]
data_CH_sAOD <- data_CH_sAOD[year(data_CH_sAOD$date)==2024,]
unique(year(data_CH$date))
unique(year(data_CH_sAOD$date))

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

data_merged <- left_join(data_sAOD_mensual_CH, data_AOD_mensual_CH, by = "month")
#unique(year(data_merged$date))
#Oscuro SAOD - Claro con AOD
# CH "#00441b","#238b45"

data_long_CH <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_AOD,mean_valor_raster_sAOD),
    names_to = "variable",
    values_to = "valor"
  )
library(ggplot2)
library(scales)  # por si necesitás formatos personalizados

data_long_CH$month <- factor(
  data_long_CH$month,
  levels = 1:12,
  labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
)

# ggplot(data_long_CH, aes(x = date, y = valor, color = variable)) +
ggplot(data_long_CH, aes(x = month, y = valor, color = variable, group = variable)) +
  geom_line(size = 0.8) +
  #geom_point() +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "black",   
      mean_valor_raster_AOD = "#feb24c", 
      mean_valor_raster_sAOD = "#fc4e2a" 
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_AOD = "AOD",
      mean_valor_raster_sAOD = "sAOD"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 20)  # << esta línea define los saltos
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
## metricas
mean(data_AOD_mensual_CH$mean_pm25_AOD)
sd(data_AOD_mensual_CH$mean_pm25_AOD)
mean(data_AOD_mensual_CH$mean_valor_raster_AOD)
sd(data_AOD_mensual_CH$mean_valor_raster_AOD)

summary(data_AOD_mensual_CH$mean_pm25_AOD)
summary(data_AOD_mensual_CH$mean_valor_raster_AOD)

#########################################################################
#########################################################################
#########################################################################
###$ BA
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",
#### PREDICCOIN 2024 Con AOD / 
estacion <- "BA"
data_BA <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-170625-",estacion,".csv",sep=""))
data_BA$date <- as.Date(as.POSIXct(data_BA$date, format = "%Y-%m-%d"))#

data_BA_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-ET-CV-M1-230625-sAOD-",estacion,".csv",sep=""))
data_BA_sAOD$date <- as.Date(as.POSIXct(data_BA_sAOD$date, format = "%Y-%m-%d"))#




data_BA$month <- month(data_BA$date)

data_BA_sAOD$month <- month(data_BA_sAOD$date)

data_BA <- data_BA[year(data_BA$date)==2024,]
data_BA_sAOD <- data_BA_sAOD[year(data_BA_sAOD$date)==2024,]
unique(year(data_BA$date))
unique(year(data_BA_sAOD$date))

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

data_merged <- left_join(data_sAOD_mensual_BA, data_AOD_mensual_BA, by = "month")
#unique(year(data_merged$date))
#Oscuro SAOD - Claro con AOD
# BA "#00441b","#238b45"

data_long_BA <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_AOD,mean_valor_raster_sAOD),
    names_to = "variable",
    values_to = "valor"
  )
library(ggplot2)
library(scales)  # por si necesitás formatos personalizados

data_long_BA$month <- factor(
  data_long_BA$month,
  levels = 1:12,
  labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
)

# ggplot(data_long_BA, aes(x = date, y = valor, color = variable)) +
ggplot(data_long_BA, aes(x = month, y = valor, color = variable, group = variable)) +
  geom_line(size = 0.8) +
  #geom_point() +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "black",   
      mean_valor_raster_AOD = "#fb6a4a",
      mean_valor_raster_sAOD = "#99000d" 
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_AOD = "AOD",
      mean_valor_raster_sAOD = "sAOD"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 20)  # << esta línea define los saltos
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
## metricas
mean(data_AOD_mensual_BA$mean_pm25_AOD)
sd(data_AOD_mensual_BA$mean_pm25_AOD)
mean(data_AOD_mensual_BA$mean_valor_raster_AOD)
sd(data_AOD_mensual_BA$mean_valor_raster_AOD)

summary(data_AOD_mensual_BA$mean_pm25_AOD)
summary(data_AOD_mensual_BA$mean_valor_raster_AOD)




#########################################################################
#########################################################################
#########################################################################
###$ MD
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dMD",
#### PREDICCOIN 2024 Con AOD / 
###$ MD
#### PREDICCOIN 2024 Con AOD / 
estacion <- "MD"
data_MD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-260525-",estacion,".csv",sep=""))
data_MD$date <- as.Date(as.POSIXct(data_MD$date, format = "%Y-%m-%d"))#

data_MD_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-270525-sAOD-",estacion,".csv",sep=""))
data_MD_sAOD$date <- as.Date(as.POSIXct(data_MD_sAOD$date, format = "%Y-%m-%d"))#


data_MD$month <- month(data_MD$date)

data_MD_sAOD$month <- month(data_MD_sAOD$date)

data_MD <- data_MD[year(data_MD$date)==2024,]
data_MD_sAOD <- data_MD_sAOD[year(data_MD_sAOD$date)==2024,]
unique(year(data_MD$date))
unique(year(data_MD_sAOD$date))

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

data_merged <- left_join(data_sAOD_mensual_MD, data_AOD_mensual_MD, by = "month")
#unique(year(data_merged$date))
#Oscuro SAOD - Claro con AOD
# MD "#00441b","#238b45"

data_long_MD <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_AOD,mean_valor_raster_sAOD),
    names_to = "variable",
    values_to = "valor"
  )
library(ggplot2)
library(scales)  # por si necesitás formatos personalizados

data_long_MD$month <- factor(
  data_long_MD$month,
  levels = 1:12,
  labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
)

# ggplot(data_long_MD, aes(x = date, y = valor, color = variable)) +
ggplot(data_long_MD, aes(x = month, y = valor, color = variable, group = variable)) +
  geom_line(size = 0.8) +
  #geom_point() +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "black",   
      mean_valor_raster_AOD = "#9ecae1",#"#4292c6",
      mean_valor_raster_sAOD = "#2171b5"#"#023858"
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_AOD = "AOD",
      mean_valor_raster_sAOD = "sAOD"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 20)  # << esta línea define los saltos
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
## metricas
mean(data_AOD_mensual_MD$mean_pm25_AOD)
sd(data_AOD_mensual_MD$mean_pm25_AOD)
mean(data_AOD_mensual_MD$mean_valor_raster_AOD)
sd(data_AOD_mensual_MD$mean_valor_raster_AOD)

summary(data_AOD_mensual_MD$mean_pm25_AOD)
summary(data_AOD_mensual_MD$mean_valor_raster_AOD)



#########################################################################
#########################################################################
#########################################################################
###$ MX
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dMX",
#### PREDICCOIN 2024 Con AOD / 
###$ MX
#### PREDICCOIN 2024 Con AOD / 
estacion <- "MX"
data_MX <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-XGB-CV-M1-290525-",estacion,".csv",sep=""))
data_MX$date <- as.Date(as.POSIXct(data_MX$date, format = "%Y-%m-%d"))#

data_MX_sAOD <- read.csv(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_02-XGB-CV-M1-230625-sAOD-",estacion,".csv",sep=""))
data_MX_sAOD$date <- as.Date(as.POSIXct(data_MX_sAOD$date, format = "%Y-%m-%d"))#


data_MX$month <- month(data_MX$date)

data_MX_sAOD$month <- month(data_MX_sAOD$date)

data_MX <- data_MX[year(data_MX$date)==2024,]
data_MX_sAOD <- data_MX_sAOD[year(data_MX_sAOD$date)==2024,]
unique(year(data_MX$date))
unique(year(data_MX_sAOD$date))

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

data_merged <- left_join(data_sAOD_mensual_MX, data_AOD_mensual_MX, by = "month")
#unique(year(data_merged$date))
#Oscuro SAOD - Claro con AOD
# MX "#00441b","#238b45"

data_long_MX <- data_merged %>%
  pivot_longer(
    cols = c( mean_pm25_sAOD, mean_valor_raster_AOD,mean_valor_raster_sAOD),
    names_to = "variable",
    values_to = "valor"
  )
library(ggplot2)
library(scales)  # por si necesitás formatos personalizados

data_long_MX$month <- factor(
  data_long_MX$month,
  levels = 1:12,
  labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
)

# ggplot(data_long_MX, aes(x = date, y = valor, color = variable)) +
ggplot(data_long_MX, aes(x = month, y = valor, color = variable, group = variable)) +
  geom_line(size = 0.8) +
  #geom_point() +
  scale_color_manual(
    values = c(
      mean_pm25_sAOD = "black",   
      mean_valor_raster_AOD = "#807dba",
      mean_valor_raster_sAOD = "#810f7c"#"#3f007d"
    ),
    labels = c(
      mean_pm25_sAOD = "Mediciones",
      mean_valor_raster_AOD = "AOD",
      mean_valor_raster_sAOD = "sAOD"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 20)  # << esta línea define los saltos
  ) +
  labs(
    x = "  ",
    y = "  ",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
## metricas
mean(data_AOD_mensual_MX$mean_pm25_AOD)
sd(data_AOD_mensual_MX$mean_pm25_AOD)
mean(data_AOD_mensual_MX$mean_valor_raster_AOD)
sd(data_AOD_mensual_MX$mean_valor_raster_AOD)

summary(data_AOD_mensual_MX$mean_pm25_AOD)
summary(data_AOD_mensual_MX$mean_valor_raster_AOD)

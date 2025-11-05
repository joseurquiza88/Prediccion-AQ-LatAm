#######################################################################
## OBJETIVO: Analisis de las variables predictivas
## series temporales
#######################################################################
estacion<- "SP"

data<- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp.csv",sep=""))

data$date <- as.Date(data$date, format = "%Y-%m-%d")
unique(data$estacion)
data<- data [data$estacion== "Ibirapuera",]


# Serie temporal del AOD_055
aod <- ggplot(data, aes(x = date, y = AOD_055)) +
  geom_line(color = "steelblue") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
       x = " ",
       y = "AOD 550 nm") +
  theme_classic()

aod
################################
BCSMASS_dia <- ggplot(data, aes(x = date, y = BCSMASS_dia)) +
  geom_line(color = "#fb6a4a") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
         x = " ",
       y = "BCSMASS") +
  theme_classic()

BCSMASS_dia


################################
SO2SMASS_dia <- ggplot(data, aes(x = date, y = SO2SMASS_dia)) +
  geom_line(color = "#9e9ac8") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
    x = " ",
    y = "SO2SMASS") +
  theme_classic()

SO2SMASS_dia


################################
SO4SMASS_dia <- ggplot(data, aes(x = date, y = SO4SMASS_dia)) +
  geom_line(color = "#dd3497") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
    x = " ",
    y = "SO4SMASS") +
  theme_classic()

SO4SMASS_dia

################################
SSSMASS_dia <- ggplot(data, aes(x = date, y = SSSMASS_dia)) +
  geom_line(color = "#fd8d3c") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
    x = " ",
    y = "SSSMASS") +
  theme_classic()

SSSMASS_dia

################################
blh_mean <- ggplot(data, aes(x = date, y = blh_mean)) +
  geom_line(color = "#41b6c4") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
    x = "",
    y = "blh") +
  theme_classic()

blh_mean

################################
sp_mean <- ggplot(data, aes(x = date, y = sp_mean)) +
  geom_line(color = "#bdbdbd") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
    x = " ",
    y = "sp") +
  theme_classic()

sp_mean

################################
d2m_mean <- ggplot(data, aes(x = date, y = d2m_mean)) +
  geom_line(color = "#a1d99b") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
    x = "",
    y = "d2m") +
  theme_classic()

d2m_mean

################################
t2m_mean <- ggplot(data, aes(x = date, y = t2m_mean)) +
  geom_line(color = "#c7e9c0") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
    x = " ",
    y = "t2m") +
  theme_classic()

t2m_mean


################################
tp_mean <- ggplot(data, aes(x = date, y = tp_mean)) +
  geom_line(color = "#980043") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
    x = " ",
    y = "tp") +
  theme_classic()

tp_mean

################################
u10_mean <- ggplot(data, aes(x = date, y = u10_mean)) +
  geom_line(color = "#fc4e2a") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
    x = " ",
    y = "u10") +
  theme_classic()

u10_mean


################################
v10_mean <- ggplot(data, aes(x = date, y = v10_mean)) +
  geom_line(color = "#fed976") +
  labs(#title = #paste("Serie temporal de AOD 550 nm -", estacion),
    x = " ",
    y = "v10") +
  theme_classic()

v10_mean


################################
#### NDVI
# Crear una columna de "año-mes"
data <- data %>%
  mutate(ym = floor_date(date, "month"))  # Agrupa al primer día del mes

# Si hay más de un valor por mes y querés promediar:
monthly_data <- data %>%
  group_by(ym) %>%
  summarise(AOD_monthly = mean(ndvi, na.rm = TRUE)) %>%
  ungroup()

ggplot(monthly_data, aes(x = ym, y = AOD_monthly)) +
  geom_line(color = "#99d8c9") +
  #geom_point(color = "darkred") +
  labs(x = " ",
       y = "NDVI") +#,
       #title = paste("Variabilidad mensual de AOD -", estacion)
       
  theme_classic()

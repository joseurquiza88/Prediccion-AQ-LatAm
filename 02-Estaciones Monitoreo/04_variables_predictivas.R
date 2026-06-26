# Objetivo: ----
# Analisis exploratorio de las variables predictivas

estacion<- "SP"

data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/", estacion, 
                       "/proceed/merge_tot/", estacion, "_merge_comp.csv", 
                       sep=""))

data$date <- as.Date(data$date, format = "%Y-%m-%d")
unique(data$estacion)
data<- data [data$estacion == "Ibirapuera",]

# Serie temporal del AOD_055
aod <- ggplot(data, aes(x = date, y = AOD_055)) +
  geom_line(color = "steelblue") +
  labs(x = " ",
       y = "AOD 550 nm") +
  theme_classic ()
aod
# BCSMASS ----
BCSMASS_dia <- ggplot(data, aes(x = date, y = BCSMASS_dia)) +
  geom_line(color = "#fb6a4a") +
  labs(x = " ",
       y = "BCSMASS") +
  theme_classic()

BCSMASS_dia

# SO2SMASS ----
SO2SMASS_dia <- ggplot(data, aes(x = date, y = SO2SMASS_dia)) +
  geom_line(color = "#9e9ac8") +
  labs(
    x = " ",
    y = "SO2SMASS") +
  theme_classic()

SO2SMASS_dia

# SO4SMASS ----
SO4SMASS_dia <- ggplot(data, aes(x = date, 
                                 y = SO4SMASS_dia)) +
  geom_line(color = "#dd3497") +
  labs(
    x = " ",
    y = "SO4SMASS") +
  theme_classic()

SO4SMASS_dia

# SSSMASS ----
SSSMASS_dia <- ggplot(data, aes(x = date, y = SSSMASS_dia)) +
  geom_line(color = "#fd8d3c") +
  labs(
    x = " ",
    y = "SSSMASS") +
  theme_classic()

SSSMASS_dia

# BLH ----
blh_mean <- ggplot(data, aes(x = date, y = blh_mean)) +
  geom_line(color = "#41b6c4") +
  labs(x = "",
    y = "blh") +
  theme_classic()

blh_mean

# SP ----
sp_mean <- ggplot(data, aes(x = date, y = sp_mean)) +
  geom_line(color = "#bdbdbd") +
  labs(
    x = " ",
    y = "sp") +
  theme_classic()

sp_mean

# D2M ----
d2m_mean <- ggplot(data, aes(x = date, y = d2m_mean)) +
  geom_line(color = "#a1d99b") +
  labs(
    x = "",
    y = "d2m") +
  theme_classic()

d2m_mean

# T2M ----
t2m_mean <- ggplot(data, aes(x = date, y = t2m_mean)) +
  geom_line(color = "#c7e9c0") +
  labs(
    x = " ",
    y = "t2m") +
  theme_classic()

t2m_mean

# TP ----
tp_mean <- ggplot(data, aes(x = date, y = tp_mean)) +
  geom_line(color = "#980043") +
  labs(
    x = " ",
    y = "tp") +
  theme_classic()

tp_mean

# U10 ----
u10_mean <- ggplot(data, aes(x = date, y = u10_mean)) +
  geom_line(color = "#fc4e2a") +
  labs(
    x = " ",
    y = "u10") +
  theme_classic()

u10_mean

# V10 ----
v10_mean <- ggplot(data, aes(x = date, y = v10_mean)) +
  geom_line(color = "#fed976") +
  labs(
    x = " ",
    y = "v10") +
  theme_classic()

v10_mean

# NDVI ----
data <- data %>%
  mutate(ym = floor_date(date, "month"))
monthly_data <- data %>%
  group_by(ym) %>%
  summarise(AOD_monthly = mean(ndvi, na.rm = TRUE)) %>%
  ungroup()

ggplot(monthly_data, aes(x = ym, y = AOD_monthly)) +
  geom_line(color = "#99d8c9") +
  labs(x = " ",
       y = "NDVI") +
  theme_classic()

#######################################################################
## OBJETIVO: Generar los plots de las comparativas del desempeño de los
#modelos simples
##
#######################################################################

### ----- Modelo predictivo Ridge y LASSO   -----
# Datos
datos <- data.frame(
  Sitio = c("SP", "ST", "BA", "MD", "MX"),
  Ridge_R2 = c(0.46, 0.59, 0.47, 0.50, 0.49),
  Lasso_R2 = c(0.44, 0.57, 0.46, 0.52, 0.48)
)

# Colores personalizados por sitio
colores <- c("SP" = "#00441b",
             "ST" = "#fc4e2a",
             "BA" = "#99000d",
             "MD" = "#023858",
             "MX" = "#3f007d")
datos$Sitio <- factor(datos$Sitio, levels = c("SP", "ST", "BA", "MD", "MX"))
# Plot sin etiquetas de texto
ggplot(datos, aes(x = Ridge_R2, y = Lasso_R2, color = Sitio)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 6) +
  scale_color_manual(values = colores) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = expression(R^2~"Ridge"),
       y = expression(R^2~"Lasso"),
       color = "Sitio") +
  theme(
         legend.position = "none"#,    
         #legend.title = element_blank(),
         
       )+
  theme_classic(base_size = 14)



##########################################
###########################################
### ----- Modelo predictivo Ridge y LASSO   -----

# Datos
datos <- data.frame(
  Sitio = c("SP", "ST", "BA", "MD", "MX"),
  Ridge_RMSE = c(8.15,10.89,7.58,6.07,7.01),
  Lasso_RMSE = c(8.12,10.83,7.87,6.06,7.10)
)

# Colores personalizados por sitio
colores <- c("SP" = "#00441b",
             "ST" = "#fc4e2a",
             "BA" = "#99000d",
             "MD" = "#023858",
             "MX" = "#3f007d")
datos$Sitio <- factor(datos$Sitio, levels = c("SP", "ST", "BA", "MD", "MX"))
# Plot sin etiquetas de texto
ggplot(datos, aes(x = Ridge_RMSE, y = Lasso_RMSE, color = Sitio)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 6) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black", size = 1.2)+
  scale_color_manual(values = colores) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 2)) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 2)) +
  labs(x = expression(RMSE~"Ridge"),
       y = expression(RMSE~"Lasso"),
       color = "Sitio") +
  theme(
    legend.position = "none"#,    
  )+
  theme_classic(base_size = 14)



##############################################################
###############################################################
### ----- Modelo predictivo GLM (con/sin AOD)   -----
# Datos
datos <- data.frame(
  Sitio = c("SP", "ST", "BA", "MD", "MX"),
  GLM_AOD_R2 = c(0.01,0.01,0.01,0.12,0.17),
  GLM_multiple_R2 = c(0.44,0.57,0.46,0.52,0.49)
)

# Colores personalizados por sitio
colores <- c("SP" = "#00441b",
             "ST" = "#fc4e2a",
             "BA" = "#99000d",
             "MD" = "#023858",
             "MX" = "#3f007d")
datos$Sitio <- factor(datos$Sitio, levels = c("SP", "ST", "BA", "MD", "MX"))
# Plot  sin etiquetas de texto
ggplot(datos, aes(x = GLM_AOD_R2, y = GLM_multiple_R2, color = Sitio)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 6) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black", size = 1.2)+

  scale_color_manual(values = colores) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = expression(R^2~"GLM AOD"),
       y = expression(R^2~"GLM Multiple"),
       color = "Sitio") +
  theme(
    legend.position = "none"#,    
  )+
  theme_classic(base_size = 14)

##########################################
###########################################
### ----- Modelo predictivo GLM (con/sin AOD)   -----

# Datos
datos <- data.frame(
  Sitio = c("SP", "ST", "BA", "MD", "MX"),
  GLM_AOD_RMSE = c(10.82,16.37,10.64,8.19,9.07),
  GLM_multiple_RMSE = c(8.16,10.82,7.85,6.06,7.10)
)

# Colores personalizados por sitio
colores <- c("SP" = "#00441b",
             "ST" = "#fc4e2a",
             "BA" = "#99000d",
             "MD" = "#023858",
             "MX" = "#3f007d")
datos$Sitio <- factor(datos$Sitio, levels = c("SP", "ST", "BA", "MD", "MX"))
# Plot sin etiquetas de texto
ggplot(datos, aes(x = GLM_AOD_RMSE, y = GLM_multiple_RMSE, color = Sitio)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 6) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black", size = 1.2)+
  scale_color_manual(values = colores) +
  scale_x_continuous(limits = c(0, 18), breaks = seq(0, 18, 3)) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, 3)) +
  labs(x = expression(RMSE~"GLM AOD"),
       y = expression(RMSE~"GLM Multiple"),
       color = "Sitio") +
  theme(
    legend.position = "none"
  )+
  theme_classic(base_size = 14)



##############################################################
###############################################################
### ----- Modelo predictivo LME (Simple/Multiple)   -----
# Datos
datos <- data.frame(
  Sitio = c("SP", "ST", "BA", "MD", "MX"),
  LME_AOD_R2 = c(0.65,0.78,0.03,0.48,0.67),
  LME_multiple_R2 = c(0.67,0.79,0.03,0.45,0.69)
)

# Colores personalizados por sitio
colores <- c("SP" = "#00441b",
             "ST" = "#fc4e2a",
             "BA" = "#99000d",
             "MD" = "#023858",
             "MX" = "#3f007d")
datos$Sitio <- factor(datos$Sitio, levels = c("SP", "ST", "BA", "MD", "MX"))
# Plot sin etiquetas de texto
ggplot(datos, aes(x = LME_AOD_R2, y = LME_multiple_R2, color = Sitio)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 6) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "red", size = 1.2)+
  scale_color_manual(values = colores) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = expression(R^2~"LME AOD"),
       y = expression(R^2~"LME Multiple"),
       color = "Sitio") +
  theme(
    legend.position = "none"#,   
  )+
  theme_classic(base_size = 14)

##########################################
###########################################
### ----- Modelo predictivo LME (Simple/Multiple)   -----

# Datos
datos <- data.frame(
  Sitio = c("SP", "ST", "BA", "MD", "MX"),
  LME_AOD_RMSE = c(6.21,10.51,6.09,5.99,7.48),
  LME_multiple_RMSE = c(5.55,10.23,7.96,5.87,7.63)
)

# Colores personalizados por sitio
colores <- c("SP" = "#00441b",
             "ST" = "#fc4e2a",
             "BA" = "#99000d",
             "MD" = "#023858",
             "MX" = "#3f007d")
datos$Sitio <- factor(datos$Sitio, levels = c("SP", "ST", "BA", "MD", "MX"))
# Plot sin etiquetas de texto
ggplot(datos, aes(x = LME_AOD_RMSE, y = LME_multiple_RMSE, color = Sitio)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 6) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "red", size = 1.2)+
  
  scale_color_manual(values = colores) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 3)) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 3)) +
  labs(x = expression(RMSE~"LME AOD"),
       y = expression(RMSE~"LME Multiple"),
       color = "Sitio") +
  theme(
    legend.position = "none"#,    
  )+
  theme_classic(base_size = 14)




##############################################################
###############################################################
### ----- Modelo predictivo GAM (Simple/Multiple)   -----
# Datos
datos <- data.frame(
  Sitio = c("SP", "ST", "BA", "MD", "MX"),
  GAM_AOD_R2 = c(0.03,0.02,0.001,0.17,0.18),
  GAM_multiple_R2 = c(0.51,0.68,0.50,0.63,0.55)
)

# Colores personalizados por sitio
colores <- c("SP" = "#00441b",
             "ST" = "#fc4e2a",
             "BA" = "#99000d",
             "MD" = "#023858",
             "MX" = "#3f007d")
datos$Sitio <- factor(datos$Sitio, levels = c("SP", "ST", "BA", "MD", "MX"))
# PLot sin etiquetas de texto
ggplot(datos, aes(x = GAM_AOD_R2, y = GAM_multiple_R2, color = Sitio)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 6) +
  scale_color_manual(values = colores) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = expression(R^2~"GAM AOD"),
       y = expression(R^2~"GAM Multiple"),
       color = "Sitio") +
  theme(
    legend.position = "none"#,    
  )+
  theme_classic(base_size = 14)

##########################################
###########################################
### ----- Modelo predictivo GAM (Simple/Multiple)   -----

# Datos
datos <- data.frame(
  Sitio = c("SP", "ST", "BA", "MD", "MX"),
  GAM_AOD_RMSE = c(10.67,16.34,10.63,7.93,9.02),
  GAM_multiple_RMSE = c(7.60,9.33,7.54,5.33,6.68)
)

# Colores personalizados por sitio
colores <- c("SP" = "#00441b",
             "ST" = "#fc4e2a",
             "BA" = "#99000d",
             "MD" = "#023858",
             "MX" = "#3f007d")
datos$Sitio <- factor(datos$Sitio, levels = c("SP", "ST", "BA", "MD", "MX"))
# Plot sin etiquetas de texto
ggplot(datos, aes(x = GAM_AOD_RMSE, y = GAM_multiple_RMSE, color = Sitio)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 6) +
  scale_color_manual(values = colores) +
  scale_x_continuous(limits = c(0, 18), breaks = seq(0, 18, 3)) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, 3)) +
  labs(x = expression(RMSE~"GAM AOD"),
       y = expression(RMSE~"GAM Multiple"),
       color = "Sitio") +
  theme(
    legend.position = "none"#,    
  )+
  theme_classic(base_size = 14)

####################################################################################
####################################################################################
####################################################################################
####################################################################################
### ----- Modelo predictivo RLS por bins   -----
##01. --- RLS
# Cargar los datos
estacion <- "MX"
modelo <- "1"

dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))

train_data$NombreColumnaNueva <- 

test_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))




colores <- c("SP" = "#00441b",
             "ST" = "#fc4e2a",
             "BA" = "#99000d",
             "MD" = "#023858",
             "MX" = "#3f007d")
modelo_lm <- lm(PM25 ~ AOD_055, data = train_data)
test_data$pred <- predict(modelo_lm, newdata = test_data)

df <- test_data

# Numero de bins deseado
n_bins <- 10

# Crear bin numerico
df <- df %>%
  mutate(AOD_bin = ntile(AOD_055, n_bins))
df <- df %>%
  mutate(bias = pred - PM25)
# Calcular el rango (min y max) de AOD_055 en cada bin
bin_info <- df %>%
  group_by(AOD_bin) %>%
  summarise(
    min_aod = min(AOD_055, na.rm = TRUE),
    max_aod = max(AOD_055, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    bin_label = paste0(sprintf("%.2f", min_aod), "â€“", sprintf("%.2f", max_aod))
  )

# Unir bin_info al dataframe original

df <- df %>%
  left_join(bin_info, by = "AOD_bin") %>%
  mutate(
    bin_label = factor(bin_label, levels = bin_info$bin_label)  
  )

#revisar esto
df2 <- df[df$bias>-50,]
ggplot(df2, aes(x = bin_label, y = bias)) +
  geom_violin(fill = "#3f007d", alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "#3f007d") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Rango de AOD (cuantiles)", 
    y = "Bias (PM_pred - PM Medido)"
  ) +  coord_cartesian(ylim = c(-60, 40)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



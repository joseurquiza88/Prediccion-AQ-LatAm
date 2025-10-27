# Librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
# Fig xxx. Metricas de  desempeño de modelos de predicción PM2.5

# Datos en formato largo
data <- tribble(
  ~City, ~Model, ~R2, ~RMSE, ~Bias,
  "SP", "SVR", 0.79, 6.06, -0.68,
  "SP", "RF", 0.71, 5.94, 0.11,
  "SP", "ET", 0.72, 5.92, 0.10,
  "SP", "XGB", 0.72, 5.74, -0.12,
  "ST", "SVR", 0.82, 7.12, -0.53,
  "ST", "RF", 0.84, 6.66, 0.12,
  "ST", "ET", 0.83, 6.91, 0.16,
  "ST", "XGB", 0.85, 6.28, 0.11,
  "BA", "SVR", 0.50, 7.52, -0.37,
  "BA", "RF", 0.57, 7.04, 0.28,
  "BA", "ET", 0.59, 7.02, -0.30,
  "BA", "XGB", 0.72, 4.53, -0.11,
  "MD", "SVR", 0.70, 4.67, -0.38,
  "MD", "RF", 0.70, 4.75, -0.001,
  "MD", "ET", 0.70, 4.85, -0.02,
  "MD", "XGB", 0.72, 4.53, -0.11,
  "MX", "SVR", 0.70, 5.50, -0.48,
  "MX", "RF", 0.72, 5.41, 0.15,
  "MX", "ET", 0.72, 5.52, 0.18,
  "MX", "XGB", 0.78, 5.03, -0.004
)

#
# Para mostrar R2
heatmap_data <- dcast(data, City ~ Model, value.var = "R2")

# Convertir a formato largo
heatmap_long <- melt(heatmap_data, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long$City <- factor(heatmap_long$City, levels = c("SP", "ST", "BA", "MD", "MX"))


# Graficar
ggplot(heatmap_long, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#fcbba1", high = "#e31a1c", 
    limits = c(0.5, 1), 
    name = "R²"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )

# Para mostrar R2
heatmap_data_rmse <- dcast(data, City ~ Model, value.var = "RMSE")

# Convertir a formato largo
heatmap_long_rmse <- melt(heatmap_data_rmse, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long_rmse$City <- factor(heatmap_long_rmse$City, levels = c("SP", "ST", "BA", "MD", "MX"))


ggplot(heatmap_long_rmse, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#d9f0d3",   # verde claro (valor bajo)
    high = "#31a354",  # verde oscuro (valor alto)
    limits = c(4, 9),  # valores de RMSE esperados
    name = "RMSE"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )



###################

# Preparar datos para el heatmap de Bias
heatmap_data_bias <- dcast(data, City ~ Model, value.var = "Bias")
heatmap_long_bias <- melt(heatmap_data_bias, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long_bias$City <- factor(heatmap_long_bias$City, levels = c("SP", "ST", "BA", "MD", "MX"))

# Graficar
ggplot(heatmap_long_bias, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#08306b",   # azul oscuro (bias negativo, subestimación)
    high = "#deebf7",  # azul claro (bias más cercano a 0 o positivo)
    limits = c(-0.7, 0.3),
    name = "Bias"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )

#############################################################
##################################################################
# Fig xxx. Tabla 3. Desempeño según tipo de validación cruzada cv random

# Datos en formato largo
# Nuevos datos de desempeño según validación cruzada aleatoria
data_cv_random <- tribble(
  ~City, ~Model, ~R2, ~RMSE, ~Bias,
  "SP", "SVR", 0.69, 6.06, -0.68,
  "SP", "RF", 0.71, 5.94, 0.11,
  "SP", "ET", 0.72, 5.92, 0.10,
  "SP", "XGB", 0.72, 5.74, -0.11,
  "ST", "SVR", 0.82, 7.12, -0.53,
  "ST", "RF", 0.84, 6.66, 0.12,
  "ST", "ET", 0.83, 6.91, 0.16,
  "ST", "XGB", 0.85, 6.28, 0.09,
  "BA", "SVR", 0.50, 7.52, -0.37,
  "BA", "RF", 0.57, 7.04, 0.28,
  "BA", "ET", 0.59, 7.02, 0.30,
  "BA", "XGB", 0.54, 7.35, 0.51,
  "MD", "SVR", 0.78, 4.17, -0.44,
  "MD", "RF", 0.73, 4.57, 0.09,
  "MD", "ET", 0.87, 3.40, 0.03,
  "MD", "XGB", 0.77, 4.18, -0.08,
  "MX", "SVR", 0.67, 5.76, -0.58,
  "MX", "RF", 0.52, 5.42, 0.15,
  "MX", "ET", 0.72, 5.52, 0.17,
  "MX", "XGB", 0.74, 5.03, 0.03
)


#
# Para mostrar R2
heatmap_data <- dcast(data_cv_random, City ~ Model, value.var = "R2")

# Convertir a formato largo
heatmap_long <- melt(heatmap_data, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long$City <- factor(heatmap_long$City, levels = c("SP", "ST", "BA", "MD", "MX"))


# Graficar
ggplot(heatmap_long, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#fcbba1", high = "#e31a1c", 
    limits = c(0.5, 1), 
    name = "R²"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )

# Para mostrar R2
heatmap_data_rmse <- dcast(data_cv_random, City ~ Model, value.var = "RMSE")

# Convertir a formato largo
heatmap_long_rmse <- melt(heatmap_data_rmse, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long_rmse$City <- factor(heatmap_long_rmse$City, levels = c("SP", "ST", "BA", "MD", "MX"))


ggplot(heatmap_long_rmse, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#d9f0d3",   # verde claro (valor bajo)
    high = "#31a354",  # verde oscuro (valor alto)
    limits = c(3, 9),  # valores de RMSE esperados
    name = "RMSE"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )



###################

# Preparar datos para el heatmap de Bias
heatmap_data_bias <- dcast(data_cv_random, City ~ Model, value.var = "Bias")
heatmap_long_bias <- melt(heatmap_data_bias, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long_bias$City <- factor(heatmap_long_bias$City, levels = c("SP", "ST", "BA", "MD", "MX"))

# Graficar
ggplot(heatmap_long_bias, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#08306b",   # azul oscuro (bias negativo, subestimación)
    high = "#deebf7",  # azul claro (bias más cercano a 0 o positivo)
    limits = c(-0.7, 0.7),
    name = "Bias"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )

#############################################################
##################################################################
# Fig xxx. Tabla 3. Desempeño según tipo de validación cruzada cv espacial

# Datos en formato largo
# Nuevos datos de desempeño según validación cruzada aleatoria
data_cv_espacial <- tribble(
  ~City, ~Model, ~R2, ~RMSE, ~Bias,
  "SP", "SVR", 0.70, 6.04, -0.67,
  "SP", "RF",  0.72, 5.89, 0.09,
  "SP", "ET",  0.72, 5.92, 0.09,
  "SP", "XGB", 0.61, 6.57, 0.44,
  
  "ST", "SVR", 0.80, 7.54, -0.71,
  "ST", "RF",  0.84, 7.03, 0.30,
  "ST", "ET",  0.83, 6.92, 0.15,
  "ST", "XGB", 0.76, 8.18, 0.36,
  
  "BA", "SVR", 0.51, 7.81, -0.41,
  "BA", "RF",  0.57, 7.03, 0.30,
  "BA", "ET",  0.59, 6.97, 0.28,
  "BA", "XGB", 0.25, 9.46, -0.70,
  
  "MD", "SVR", 0.68, 4.97, 0.53,
  "MD", "RF",  0.74, 4.57, 0.08,
  "MD", "ET",  0.73, 4.74, 0.06,
  "MD", "XGB", 0.30, 7.22, -0.06,
  
  "MX", "SVR", 0.67, 5.76, -0.58,
  "MX", "RF",  0.72, 5.41, 0.15,
  "MX", "ET",  0.72, 5.53, 0.18,
  "MX", "XGB", 0.60, 6.54, 0.87
)


#
# Para mostrar R2
heatmap_data <- dcast(data_cv_espacial, City ~ Model, value.var = "R2")

# Convertir a formato largo
heatmap_long <- melt(heatmap_data, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long$City <- factor(heatmap_long$City, levels = c("SP", "ST", "BA", "MD", "MX"))


# Graficar
ggplot(heatmap_long, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#fcbba1", high = "#e31a1c", 
    limits = c(0, 1), 
    name = "R²"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )

# Para mostrar R2
heatmap_data_rmse <- dcast(data_cv_espacial, City ~ Model, value.var = "RMSE")

# Convertir a formato largo
heatmap_long_rmse <- melt(heatmap_data_rmse, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long_rmse$City <- factor(heatmap_long_rmse$City, levels = c("SP", "ST", "BA", "MD", "MX"))


ggplot(heatmap_long_rmse, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#d9f0d3",   # verde claro (valor bajo)
    high = "#31a354",  # verde oscuro (valor alto)
    limits = c(3, 10),  # valores de RMSE esperados
    name = "RMSE"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )



###################

# Preparar datos para el heatmap de Bias
heatmap_data_bias <- dcast(data_cv_espacial, City ~ Model, value.var = "Bias")
heatmap_long_bias <- melt(heatmap_data_bias, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long_bias$City <- factor(heatmap_long_bias$City, levels = c("SP", "ST", "BA", "MD", "MX"))

# Graficar
ggplot(heatmap_long_bias, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#08306b",   # azul oscuro (bias negativo, subestimación)
    high = "#deebf7",  # azul claro (bias más cercano a 0 o positivo)
    limits = c(-0.9, 0.9),
    name = "Bias"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )




#############################################################
##################################################################
# Fig xxx. Tabla 3. Desempeño según tipo de validación cruzada cv temporal

# Datos en formato largo
# Nuevos datos de desempeño según validación cruzada aleatoria
# Datos de desempeño con validación cruzada temporal
data_cv_temporal <- tribble(
  ~City, ~Model, ~R2, ~RMSE, ~Bias,
  "SP", "SVR", 0.65, 6.46, -0.75,
  "SP", "RF",  0.72, 5.88, 0.07,
  "SP", "ET",  0.72, 5.93, 0.09,
  "SP", "XGB", 0.86, 6.47, 0.03,
  
  "ST", "SVR", 0.78, 7.77, -0.81,
  "ST", "RF",  0.84, 6.67, 0.12,
  "ST", "ET",  0.83, 6.93, 0.18,
  "ST", "XGB", 0.71, 8.71, -0.21,
  
  "BA", "SVR", 0.53, 7.41, -0.95,
  "BA", "RF",  0.57, 6.99, 0.29,
  "BA", "ET",  0.59, 6.98, 0.27,
  "BA", "XGB", 0.44, 8.01, -0.04,
  
  "MD", "SVR", 0.70, 4.79, -0.51,
  "MD", "RF",  0.44, 4.57, 0.08,
  "MD", "ET",  0.73, 4.73, 0.06,
  "MD", "XGB", 0.53, 5.80, 0.03,
  
  "MX", "SVR", 0.64, 6.04, -0.65,
  "MX", "RF",  0.72, 5.40, 0.14,
  "MX", "ET",  0.72, 5.33, 0.17,
  "MX", "XGB", 0.52, 7.04, -0.14
)


#
# Para mostrar R2
heatmap_data <- dcast(data_cv_temporal, City ~ Model, value.var = "R2")

# Convertir a formato largo
heatmap_long <- melt(heatmap_data, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long$City <- factor(heatmap_long$City, levels = c("SP", "ST", "BA", "MD", "MX"))


# Graficar
ggplot(heatmap_long, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#fcbba1", high = "#e31a1c", 
    limits = c(0, 1), 
    name = "R²"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )

# Para mostrar R2
heatmap_data_rmse <- dcast(data_cv_temporal, City ~ Model, value.var = "RMSE")

# Convertir a formato largo
heatmap_long_rmse <- melt(heatmap_data_rmse, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long_rmse$City <- factor(heatmap_long_rmse$City, levels = c("SP", "ST", "BA", "MD", "MX"))


ggplot(heatmap_long_rmse, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#d9f0d3",   # verde claro (valor bajo)
    high = "#31a354",  # verde oscuro (valor alto)
    limits = c(3, 10),  # valores de RMSE esperados
    name = "RMSE"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )



###################

# Preparar datos para el heatmap de Bias
heatmap_data_bias <- dcast(data_cv_temporal, City ~ Model, value.var = "Bias")
heatmap_long_bias <- melt(heatmap_data_bias, id.vars = "City")
#Reordenar los sitios (factor con niveles específicos)
heatmap_long_bias$City <- factor(heatmap_long_bias$City, levels = c("SP", "ST", "BA", "MD", "MX"))

# Graficar
ggplot(heatmap_long_bias, aes(x = variable, y = City, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#08306b",   # azul oscuro (bias negativo, subestimación)
    high = "#deebf7",  # azul claro (bias más cercano a 0 o positivo)
    limits = c(-1, 1),
    name = "Bias"
  ) +
  labs(
    x = " ", 
    y = " "
  ) +  theme_classic() +theme(
    axis.text.x = element_text(size = 14),  # tamaño mayor para eje x
    axis.text.y = element_text(size = 14)   # tamaño mayor para eje y
  )



##############################################
#############################################
library(dplyr)
library(ggplot2)
library(dplyr)

# Agregar columna CV a cada uno
data_cv_random <- data_cv_random %>% mutate(CV = "Random")
data_cv_espacial <- data_cv_espacial %>% mutate(CV = "Espacial")
data_cv_temporal <- data_cv_temporal %>% mutate(CV = "Temporal")

# Unir todos
data_todo <- bind_rows(data_cv_random, data_cv_espacial, data_cv_temporal)

# Reordenar niveles
data_todo$CV <- factor(data_todo$CV, levels = c("Random", "Espacial", "Temporal"))
data_todo$City <- factor(data_todo$City, levels = c("SP", "ST", "BA", "MD", "MX"))

# Plot con abreviaturas en el eje X
ggplot(data_todo, aes(x = CV, y = R2, color = Model, group = Model)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~ City, ncol = 5) +  # 5 ciudades en una fila
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(labels = c("Random" = "R", "Espacial" = "E", "Temporal" = "T")) +
  labs(
    x = " ",
    y = "R²",
    color = "Modelo"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    strip.text = element_text(size = 12),
    #legend.position = "none"
  )

#### RMSE
# Plot con abreviaturas en el eje X
ggplot(data_todo, aes(x = CV, y = RMSE, color = Model, group = Model)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~ City, ncol = 5) +  # 5 ciudades en una fila
  scale_y_continuous(limits = c(0, 10)) +
  scale_x_discrete(labels = c("Random" = "R", "Espacial" = "E", "Temporal" = "T")) +
  labs(
    x = " ",
    y = "RMSE",
    color = "Modelo"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    strip.text = element_text(size = 12),
    legend.position = "none"
  )


#### Bias
# Plot con abreviaturas en el eje X
ggplot(data_todo, aes(x = CV, y = Bias, color = Model, group = Model)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.1) +
  facet_wrap(~ City, ncol = 5) +  # 5 ciudades en una fila
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_discrete(labels = c("Random" = "R", "Espacial" = "E", "Temporal" = "T")) +
  labs(
    x = " ",
    y = "Bias",
    color = "Modelo"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    strip.text = element_text(size = 12),
    legend.position = "none"
  )



##########################################################################
###########################################################################
library(dplyr)
library(ggplot2)

# 🔹 Filtrar solo la ciudad ST
data_st <- data_todo %>% filter(City == "ST")

# Aseguramos que el orden de CV y Model siga igual
data_st$CV <- factor(data_st$CV, levels = c("Random", "Espacial", "Temporal"))
data_st$CV <- factor(
  data_st$CV,
  levels = c("Random", "Espacial", "Temporal"),
  labels = c("Random", "Spatial", "Temporal")
)
## === 1️⃣ Plot R² ===
plot_R2_st <- ggplot(data_st, aes(x = CV, y = R2, color = Model, group = Model)) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.5, linetype = "dashed")+
  scale_y_continuous(limits = c(0.6, 1)) +
  scale_x_discrete(labels = c("Random" = "Random", "Spatial" = "Spatial", "Temporal" = "Temporal")) +
  labs(
    x = "",
    y = "R²",
    color = "Modelo",
    #title = "Ciudad: ST"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    #legend.position = "bottom"
    legend.position = "none"
  )
plot_R2_st
## === 2️⃣ Plot RMSE ===
plot_RMSE_st <- ggplot(data_st, aes(x = CV, y = RMSE, color = Model, group = Model)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(5, 10)) +
  scale_x_discrete(labels = c("Random" = "Random", "Spatial" = "Spatial", "Temporal" = "Temporal")) +
  labs(
    x = " ",
    y = "RMSE",
    color = "Modelo",
    #title = "Ciudad: ST"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    
    #legend.position = "bottom"
    legend.position = "none"
  )
plot_RMSE_st
## === 3️⃣ Plot Bias ===
plot_Bias_st <- ggplot(data_st, aes(x = CV, y = Bias, color = Model, group = Model)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.3) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_discrete(labels = c("Random" = "Random", "Spatial" = "Spatial", "Temporal" = "Temporal")) +
  labs(
    x = "",#"Tipo de validación cruzada",
    y = "Bias",
    color = "Modelo",
    #title = "Ciudad: ST"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    # legend.position = "bottom"
    legend.position = "none"
  )
plot_Bias_st
# Mostrar los tres
plot_R2_st
plot_RMSE_st
plot_Bias_st


####################
#barras
## === 1️⃣ Plot R² ===
plot_R2_st <- ggplot(data_st, aes(x = CV, y = R2, color = Model, group = Model)) +
  geom_bar(size = 3) +
  # geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0.6, 1)) +
  scale_x_discrete(labels = c("Random" = "Random", "Spatial" = "Spatial", "Temporal" = "Temporal")) +
  labs(
    x = "",
    y = "R²",
    color = "Modelo",
    #title = "Ciudad: ST"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    #legend.position = "bottom"
    legend.position = "none"
  )
plot_R2_st
## === 2️⃣ Plot RMSE ===
plot_RMSE_st <- ggplot(data_st, aes(x = CV, y = RMSE, color = Model, group = Model)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(5, 10)) +
  scale_x_discrete(labels = c("Random" = "Random", "Spatial" = "Spatial", "Temporal" = "Temporal")) +
  labs(
    x = " ",
    y = "RMSE",
    color = "Modelo",
    #title = "Ciudad: ST"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    
    #legend.position = "bottom"
    legend.position = "none"
  )
plot_RMSE_st
## === 3️⃣ Plot Bias ===
plot_Bias_st <- ggplot(data_st, aes(x = CV, y = Bias, color = Model, group = Model)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.3) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_discrete(labels = c("Random" = "Random", "Spatial" = "Spatial", "Temporal" = "Temporal")) +
  labs(
    x = "",#"Tipo de validación cruzada",
    y = "Bias",
    color = "Modelo",
    #title = "Ciudad: ST"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    # legend.position = "bottom"
    legend.position = "none"
  )
plot_Bias_st

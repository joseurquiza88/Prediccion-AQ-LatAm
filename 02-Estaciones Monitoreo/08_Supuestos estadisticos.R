
################################################################################
# DIAGNOSTICO DE SUPUESTOS DEL MODELO DE REGRESION MULTIPLE
################################################################################

library(tidyverse)
library(car)
library(lmtest)
library(patchwork)
library(broom)

################################################################################
# AJUSTE DEL MODELO
################################################################################

modelo <- lm(
  PM25 ~ ndvi +
    BCSMASS_dia +
    DUSMASS_dia +
    t2m_mean +
    AOD_055 +
    SO2SMASS_dia +
    SO4SMASS_dia +
    SSSMASS_dia +
    blh_mean +
    sp_mean +
    d2m_mean +
    v10_mean +
    u10_mean +
    tp_mean +
    DEM +
    dayWeek,
  data = data
)

################################################################################
# DATOS DIAGNOSTICOS
################################################################################

diag_df <- augment(modelo)

diag_df$cooks <- cooks.distance(modelo)

################################################################################
# 1. RESIDUALES VS AJUSTADOS
# (linealidad y homocedasticidad)
################################################################################

p1 <- ggplot(diag_df,
             aes(.fitted, .resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess",
              se = FALSE) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  theme_classic() +
  labs(
    x = "Valores ajustados",
    y = "Residuos"
  )

################################################################################
# 2. QQ-PLOT
# (normalidad)
################################################################################

p2 <- ggplot(diag_df,
             aes(sample = .std.resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    x = "Cuantiles teóricos",
    y = "Cuantiles observados"
  )

################################################################################
# 3. SCALE-LOCATION
# (homocedasticidad)
################################################################################

diag_df$sqrt_std_resid <- sqrt(abs(diag_df$.std.resid))

p3 <- ggplot(diag_df,
             aes(.fitted,
                 sqrt_std_resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess",
              se = FALSE) +
  theme_classic() +
  labs(
    x = "Valores ajustados",
    y = expression(sqrt("|Residuos estandarizados|"))
  )

################################################################################
# 4. DISTANCIA DE COOK
# (observaciones influyentes)
################################################################################

p4 <- ggplot(diag_df,
             aes(seq_along(cooks),
                 cooks)) +
  geom_col() +
  theme_classic() +
  labs(
    x = "Observación",
    y = "Distancia de Cook"
  )

################################################################################
# FIGURA RESUMEN PARA LA TESIS
################################################################################

fig_diag <- (p1 + p2) /
  (p3 + p4)

fig_diag

ggsave(
  "Diagnosticos_modelo.png",
  fig_diag,
  width = 10,
  height = 8,
  dpi = 300
)

################################################################################
# HISTOGRAMA DE RESIDUOS
################################################################################

p_hist <- ggplot(diag_df,
                 aes(.resid)) +
  geom_histogram(
    bins = 30,
    color = "black",
    fill = "grey80"
  ) +
  theme_classic() +
  labs(
    x = "Residuos",
    y = "Frecuencia"
  )

p_hist

################################################################################
# ACF DE RESIDUOS
# (independencia temporal)
################################################################################

acf_df <- as.data.frame(
  acf(
    residuals(modelo),
    plot = FALSE
  )
)

limite <- 1.96 / sqrt(nrow(diag_df))

p_acf <- ggplot(acf_df,
                aes(lag, acf)) +
  geom_segment(
    aes(
      xend = lag,
      yend = 0
    )
  ) +
  geom_hline(
    yintercept = c(-limite, limite),
    linetype = 2
  ) +
  theme_classic() +
  labs(
    x = "Lag",
    y = "ACF"
  )

p_acf

################################################################################
# TESTS PARA REPORTAR EN TABLA
################################################################################

# Normalidad
shapiro_res <- shapiro.test(residuals(modelo))

# Homocedasticidad
bp_res <- bptest(modelo)

# Independencia
dw_res <- dwtest(modelo)

# Multicolinealidad
vif_res <- vif(modelo)

################################################################################
# TABLA RESUMEN
################################################################################

tabla_supuestos <- data.frame(
  Test = c(
    "Shapiro-Wilk",
    "Breusch-Pagan",
    "Durbin-Watson",
    "VIF máximo"
  ),
  Resultado = c(
    round(shapiro_res$p.value, 4),
    round(bp_res$p.value, 4),
    round(dw_res$p.value, 4),
    round(max(vif_res), 2)
  )
)

tabla_supuestos

write.csv(
  tabla_supuestos,
  "Tabla_supuestos_modelo.csv",
  row.names = FALSE
)


############################
#Plots tsis

p1 <- ggplot(diag_df,
             aes(.fitted, .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  geom_smooth(method = "loess",
              se = FALSE) +
  theme_classic() +
  labs(
    x = "Valores ajustados",
    y = "Residuos"
  )

#####
p2 <- ggplot(diag_df,
             aes(sample = .std.resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    x = "Cuantiles teóricos",
    y = "Cuantiles observados"
  )

# los unimos
library(patchwork)

(p1 | p2)

#o
fig_diag <- p1 | p2

ggsave(
  "Diagnosticos_modelo.png",
  fig_diag,
  width = 10,
  height = 5,
  dpi = 300
)


Los supuestos de los modelos fueron evaluados mediante análisis gráficos y estadísticos. 
La linealidad y homocedasticidad se examinaron mediante gráficos de residuos versus valores 
ajustados, mientras que la normalidad de los residuos se evaluó mediante gráficos Q-Q. 
Los resultados indicaron un cumplimiento satisfactorio de los supuestos considerados los cuales se muestran en los anexos.
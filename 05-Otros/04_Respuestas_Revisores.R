# ============================================
# DIAGNÓSTICOS DE REGRESIÓN - RLS (PM2.5 ~ AOD)
# ============================================

library(ggplot2)
library(gridExtra)
library(lmtest)  # Durbin-Watson

# ----- CONFIGURACIÓN -----
archivo <- "BA_merge_comp.csv"  # cambiá por SP, ST, MD, MX
sitio   <- "BA"                  # cambiá el nombre

# ----- CARGAR Y PREPARAR -----
df <- read.csv(archivo)
df <- df[, c("PM25", "AOD_055")]
df <- na.omit(df)
cat("N observaciones:", nrow(df), "\n")

# ----- MODELO -----
modelo <- lm(PM25 ~ AOD_055, data = df)
summary(modelo)

residuos  <- residuals(modelo)
ajustados <- fitted(modelo)
std_res   <- rstandard(modelo)

# ----- TESTS -----
# Normalidad (muestra de 5000 max por limitación de Shapiro)
set.seed(42)
muestra <- sample(residuos, min(5000, length(residuos)))
sw <- shapiro.test(muestra)
cat("Shapiro-Wilk: W =", round(sw$statistic, 4), "p =", sw$p.value, "\n")

# Independencia
dw <- dwtest(modelo)
cat("Durbin-Watson: DW =", round(dw$statistic, 4), "p =", dw$p.value, "\n")

# Cook's distance
cooks  <- cooks.distance(modelo)
umbral <- 4 / nrow(df)
cat("Obs. influyentes (Cook > 4/n):", sum(cooks > umbral), "\n")

# ----- FIGURA -----
png(paste0(sitio, "_diagnosticos_regresion.png"), 
    width = 3000, height = 2400, res = 220)

par(mfrow = c(2, 2), 
    mar = c(4.5, 4.5, 3.5, 1.5),
    oma = c(0, 0, 3, 0))

# 1. Residuos vs ajustados
plot(ajustados, residuos,
     pch = 16, cex = 0.4, col = adjustcolor("steelblue", alpha.f = 0.4),
     xlab = "Valores ajustados (µg/m³)",
     ylab = "Residuos (µg/m³)",
     main = "Residuos vs. Valores ajustados")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

# 2. QQ-plot
qqnorm(residuos, pch = 16, cex = 0.4,
       col = adjustcolor("steelblue", alpha.f = 0.4),
       main = "QQ-plot de residuos")
qqline(residuos, col = "red", lwd = 1.5, lty = 2)
legend("topleft", 
       legend = paste0("Shapiro-Wilk\nW = ", round(sw$statistic, 3),
                       "\np < 0.001"),
       bty = "n", cex = 0.8)

# 3. Scale-Location
plot(ajustados, sqrt(abs(std_res)),
     pch = 16, cex = 0.4, col = adjustcolor("steelblue", alpha.f = 0.4),
     xlab = "Valores ajustados (µg/m³)",
     ylab = expression(sqrt("|Residuos estand.|")),
     main = "Scale-Location")

# 4. Cook's distance
plot(cooks, type = "h", col = "steelblue",
     xlab = "Índice de observación",
     ylab = "Distancia de Cook",
     main = "Distancia de Cook\n(Observaciones influyentes)")
abline(h = umbral, col = "red", lty = 2, lwd = 1.5)
legend("topright",
       legend = paste0("Umbral 4/n = ", round(umbral, 4)),
       col = "red", lty = 2, bty = "n", cex = 0.8)

# Título general
mtext(paste0(sitio, " — Diagnósticos de regresión lineal simple (PM₂.₅ ~ AOD)"),
      outer = TRUE, cex = 1.1, font = 2)

dev.off()
cat("Figura guardada:", paste0(sitio, "_diagnosticos_regresion.png"), "\n")
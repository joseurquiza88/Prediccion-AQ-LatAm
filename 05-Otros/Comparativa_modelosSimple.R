################################################################################
## Comparativa de familias GLM por ciudad
################################################################################

estacion <- "MX"  # Cambiar por: SP, BA, MD, MX
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_",  estacion, ".csv"))

train_data <- train_data[train_data$PM25 > 0, ]
test_data  <- test_data[test_data$PM25  > 0, ]

familias <- list(
  gaussian     = gaussian(),
  gamma_log    = Gamma(link = "log"),
  invgauss_log = inverse.gaussian(link = "log")
)

resultados <- data.frame()

for (nombre in names(familias)) {
  tryCatch({
    mod <- glm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia +
                 DEM + t2m_mean + SO2SMASS_dia + SO4SMASS_dia +
                 SSSMASS_dia + blh_mean + d2m_mean + v10_mean +
                 u10_mean + tp_mean,
               family  = familias[[nombre]],
               data    = train_data,
               control = glm.control(maxit = 200, epsilon = 1e-8))
    
    preds <- predict(mod, newdata = test_data, type = "response")
    preds <- pmax(preds, 0)
    
    r2   <- cor(test_data$PM25, preds)^2
    rmse <- sqrt(mean((test_data$PM25 - preds)^2))
    bias <- mean(preds - test_data$PM25)
    disp <- sum(residuals(mod, type = "pearson")^2) / mod$df.residual
    
    resultados <- rbind(resultados, data.frame(
      ciudad     = estacion,
      familia    = nombre,
      R2         = round(r2,   3),
      RMSE       = round(rmse, 3),
      Bias       = round(bias, 3),
      AIC        = round(AIC(mod), 1),
      dispersion = round(disp, 3)
    ))
    
    cat("OK:", nombre, "\n")
    
  }, error = function(e) {
    cat("Saltando", nombre, "- Error:", e$message, "\n")
  })
}

print(resultados)
# df <- data.frame()
df <- rbind(df,resultados)
df

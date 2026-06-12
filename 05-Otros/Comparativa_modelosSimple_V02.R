################################################################################
## Métricas GLM  - Solo AOD vs Múltiples variables - Todos los sitios
################################################################################

library(dplyr)

estaciones <- c("SP", "CH", "BA", "MD", "MX")
modelo <- "1"

calcular_metricas <- function(mod, test_data, nombre_modelo, ciudad) {
  preds <- predict(mod, newdata = test_data, type = "response")
  preds <- pmax(preds, 0)
  r2    <- cor(test_data$PM25, preds)^2
  rmse  <- sqrt(mean((test_data$PM25 - preds)^2))
  bias  <- mean(preds - test_data$PM25)
  disp  <- sum(residuals(mod, type = "pearson")^2) / mod$df.residual
  
  data.frame(
    ciudad  = ciudad,
    modelo  = nombre_modelo,
    R2      = round(r2,   3),
    RMSE    = round(rmse, 3),
    Bias    = round(bias, 3),
    dispersion = round(disp, 3)
  )
}

resultados_todos <- data.frame()

for (estacion in estaciones) {
  
  dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, 
                "/modelos/ParticionDataSet/")
  
  tryCatch({
    train_data <- read.csv(paste0(dir, "Modelo_", modelo, 
                                  "/M", modelo, "_train_", estacion, ".csv"))
    test_data  <- read.csv(paste0(dir, "Modelo_", modelo, 
                                  "/M", modelo, "_test_",  estacion, ".csv"))
    
    train_data <- train_data[train_data$PM25 > 0, ]
    test_data  <- test_data[test_data$PM25  > 0, ]
    
    # Modelo solo AOD
    mod_aod <- glm(PM25 ~ AOD_055,
                   family  = gaussian(),
                   data    = train_data,
                   control = glm.control(maxit = 200))
    
    # Modelo múltiples variables
    mod_final <- glm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia +
                       DEM + t2m_mean + SO2SMASS_dia + SO4SMASS_dia +
                       SSSMASS_dia + blh_mean + d2m_mean + v10_mean +
                       u10_mean + tp_mean,
                     family  = gaussian(),
                     data    = train_data,
                     control = glm.control(maxit = 200))
    
    resultados_todos <- rbind(
      resultados_todos,
      calcular_metricas(mod_aod,   test_data, "Solo AOD",            estacion),
      calcular_metricas(mod_final, test_data, "Multiples variables",  estacion)
    )
    
    cat("OK:", estacion, "\n")
    
  }, error = function(e) {
    cat("Error en", estacion, ":", e$message, "\n")
  })
}

print(resultados_todos)

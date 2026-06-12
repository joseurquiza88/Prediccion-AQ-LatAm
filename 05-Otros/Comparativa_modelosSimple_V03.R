estaciones <- c("SP", "CH", "BA", "MD", "MX")
modelo <- "1"

resultados_lme <- data.frame()

for (estacion in estaciones) {
  
  dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, 
                "/modelos/ParticionDataSet/")
  
  tryCatch({
    train_data <- read.csv(paste0(dir, "Modelo_", modelo, 
                                  "/M", modelo, "_train_", estacion, ".csv"))
    test_data  <- read.csv(paste0(dir, "Modelo_", modelo, 
                                  "/M", modelo, "_test_",  estacion, ".csv"))
    
    train_data$fecha   <- as.factor(train_data$date)
    test_data$fecha    <- as.factor(test_data$date)
    train_data$estacion <- as.factor(train_data$estacion)
    test_data$estacion  <- as.factor(test_data$estacion)
    train_data <- train_data[train_data$PM25 > 0, ]
    test_data  <- test_data[test_data$PM25  > 0, ]
    
    # Escalar variables
    vars_escalar <- c("AOD_055", "t2m_mean", "d2m_mean", "v10_mean", 
                      "u10_mean", "blh_mean", "tp_mean", "ndvi")
    vars_presentes <- vars_escalar[vars_escalar %in% names(train_data)]
    train_scaled <- train_data
    test_scaled  <- test_data
    train_scaled[vars_presentes] <- scale(train_data[vars_presentes])
    test_scaled[vars_presentes]  <- scale(test_data[vars_presentes])
    
    # Estructuras a comparar
    estructuras <- list(
      "Solo AOD - (1|fecha)"        = PM25 ~ AOD_055 + (1|fecha),
      "Solo AOD - (1|dayWeek)"      = PM25 ~ AOD_055 + (1|dayWeek),
      "Multiple - (1|fecha)"        = PM25 ~ AOD_055 + t2m_mean + d2m_mean + 
        v10_mean + u10_mean + blh_mean + 
        tp_mean + ndvi + (1|fecha),
      "Multiple - (1|dayWeek)"      = PM25 ~ AOD_055 + t2m_mean + d2m_mean + 
        v10_mean + u10_mean + blh_mean + 
        tp_mean + ndvi + (1|dayWeek),
      "Multiple - (1|estacion)"     = PM25 ~ AOD_055 + t2m_mean + d2m_mean + 
        v10_mean + u10_mean + blh_mean + 
        tp_mean + ndvi + (1|estacion)
    )
    
    for (nombre in names(estructuras)) {
      tryCatch({
        mod <- lmer(estructuras[[nombre]], 
                    data    = train_scaled,
                    REML    = FALSE,
                    control = lmerControl(optimizer = "bobyqa"))
        
        preds <- predict(mod, newdata = test_scaled, allow.new.levels = TRUE)
        preds <- pmax(preds, 0)
        
        r2   <- cor(test_data$PM25, preds)^2
        rmse <- sqrt(mean((test_data$PM25 - preds)^2))
        bias <- mean(preds - test_data$PM25)
        
        resultados_lme <- rbind(resultados_lme, data.frame(
          ciudad    = estacion,
          estructura = nombre,
          R2        = round(r2,   3),
          RMSE      = round(rmse, 3),
          Bias      = round(bias, 3)
        ))
        
        cat("OK:", estacion, "-", nombre, "\n")
        
      }, error = function(e) {
        cat("Error:", estacion, "-", nombre, ":", e$message, "\n")
      })
    }
    
  }, error = function(e) {
    cat("Error cargando datos:", estacion, ":", e$message, "\n")
  })
}

print(resultados_lme)

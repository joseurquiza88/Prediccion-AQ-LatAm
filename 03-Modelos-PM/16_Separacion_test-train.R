# Separacion de datos de entrenamiento y testeo
# Parametros iniciales
estacion <- "CH"
modelo <- "1"

# Directorios
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/proceed/merge_tot/")
setwd(dir)

# Lectura de datos
data <- read.csv(paste0(dir,estacion ,"_merge_comp_V02.csv"))

# Fijar semilla para reproducibilidad
set.seed(123)

# Cantidad de registros
n <- nrow(data)

# Selección aleatoria del 70 %
idx_train <- sample(seq_len(n), size = floor(0.70 * n), replace = FALSE)

# Partición del dataset
train_data <- data[idx_train, ]
test_data  <- data[-idx_train, ]

# Verificar partición
nrow(train_data)
nrow(test_data)


# Comprobación de duplicados
# Duplicados en entrenamiento
sum(duplicated(train_data))

# Duplicados en testeo
sum(duplicated(test_data))

# Unir ambos datasets
data_union <- rbind(train_data, test_data)

# Cantidad de registros duplicados en la unión
sum(duplicated(data_union))


# Verificar cantidad de registros
nrow(data) == nrow(data_union)

# Verificar que ambos datasets contienen exactamente los mismos registros
identical(
  data[order(do.call(paste, c(data, sep = "|"))), ],
  data_union[order(do.call(paste, c(data_union, sep = "|"))), ]
)



# Gurdar datasets
dir_guardado <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/Modelo_1_V02/")
setwd(dir_guardado)

write.csv(test_data, paste0("M1_test_",estacion,".csv"))
write.csv(train_data,paste0("M1_train_",estacion,".csv"))


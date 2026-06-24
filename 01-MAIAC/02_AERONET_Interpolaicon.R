# Objetivo ----
# Interpolar AOD de AERONET a la longitud de onda de MAIAC-MODIS (550).

# Funcion para estimar el AOD550 a partir con coef. de angstroment

path <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/datasets_interp_s_L02/original/"
path_write="D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/datasets_interp_s_L02/Latam/"
interpolate <- function(path,path_write){
  df <- data.frame()
  setwd(path)
  archive <- dir(path, pattern = ".csv")
  for(i in 1:length(archive)){
    print(i)
    # Lee todos los archivos en el path ingresado
    data <- read.csv(archive[i],  header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE, na.strings = "-999")
    # Formato fecha
    data$date <- strptime(paste(data$Date.dd.mm.yyyy., data$Time.hh.mm.ss., sep=" "), format="%d:%m:%Y %H:%M", tz="GMT") 

    # Setear nombre a partir del mismo archivo
    name <- substr(archive[i],1,12)
    # Calcular alpha segun Wenming Quin et al., (2021)
    #data$alfa <- -(log(data$AOD_440nm/data$AOD_675nm) )/log(440/675)
    #data$aod_550 <- data$AOD_440nm * (550/440)**data$alfa

    # Calcular  AOD550
    #data$aod_550 <- data$AOD_675nm*(500/675)**(-(data$Angstrom_Exponent_440_675))# change name BO-COLUMN
    #Crear nuevo dataframe
    # data$aod_550 <-  data$AOD_500nm * ((550/500)**(-data$Angstrom_Exponent_440_675))
    data$aod_550 <-  data$AOD_500nm * ((550/500)**(-data$X440.675_Angstrom_Exponent))
    
    df <- data.frame (data$date,data$aod_550)
    # Nombre de las columnas
    names(df) <- c("date", "aod_550")
    # Guardamos archivo en el path ingresado
    write.csv(df,(paste(path_write,name,"_interp-a.csv",sep="")) , row.names = FALSE)
    
  }
  
}
# Correr la funcion
interpolate(path,path_write)


################################################################################

# Funci?n para estimar AOD550 - FUNCI?N DE INTERPOLACI?N CUADR?TICA
# Esta funci?n realiza una interpolaci?n tomando 3 puntos,
# basada en los polinomios de Lagrange.

interpolate_s <- function(path,path_write){
  df <- data.frame()
  # path local
  setwd(path)
  archive <- dir(path, pattern = ".csv")
  interpol_cuad <- function(x, x0, y0, x1, y1, x2, y2){ 
    a = ((x - x1)*(x-x2))/((x0-x1)*(x0-x2))
    b = ((x - x0)*(x-x2))/((x1-x0)*(x1-x2))
    c = ((x - x0)*(x-x1))/((x2-x0)*(x2-x1))
    y = (y0*a) + (y1*b) + (y2*c)
    return(y)
  }
  for(i in 1:length(archive)){
    print(i)
    # Lee todos los archivos en el path ingresado
    data <- read.csv(archive[i],  header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE, na.strings = "-999")
    # Formato fecha
    date <- strptime(paste(data$Date.dd.mm.yyyy., data$Time.hh.mm.ss., sep=" "), format="%d:%m:%Y %H:%M", tz="GMT") 
    # Setear nombre a partir del mismo archivo
    name <- substr(archive[i],1,12)
    
    
    #Unknown value
    x = log(550)
    # First interpolation
    #Point 1
    x0= log(440)
    y0= log(data$AOD_440nm)
    #Point 2
    x1= log(500)    
    y1=log(data$AOD_500nm) 
    #Point 3
    x2= log(675) #500        
    y2= log(data$AOD_675nm)
    # Function 1
    y <- exp(interpol_cuad(x, x0, y0, x1, y1, x2, y2))
    
    #data frame con la interpoilacion 1
    data_aeronet <- data.frame(date, y)
    names(data_aeronet) <- c("date", "AOT_550")
    
    # Second interpolation
    x2= log(870)
    y2= log(data$AOD_870nm)
    # Put de information into the dataframe
    data_aeronet$AOT_550_2  <- exp(interpol_cuad(x, x0, y0, x1, y1, x2, y2))
    
    #Third interpolation
    x2= log(1020)
    y2= log(data$AOD_1020nm)
    # Agregar los datos de la interpolacion al mismo dataframe
    data_aeronet$AOT_550_3 <- exp(interpol_cuad(x, x0, y0, x1, y1, x2, y2))
    # Media de las tres interpolaciones
    data_aeronet$AOT_550_mod <- rowMeans(data_aeronet[,2:4], na.rm = TRUE)
    # Setear nombre de las columnas
    names(data_aeronet) <- c("date", "aod_550","aod_550_2","aod_550","aod_550_mod")
    # Guardar archivo en el path
    write.csv(data_aeronet,(paste(path_write,name,"_interp-s.csv",sep="")) , row.names = FALSE)
    
  }
  
}



# VERSION 02
interpolate(path="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/original",path_write="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/interpolate")

interpolate_s (path="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/original",path_write="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/interpolate/")


# Actualizacion 2023-2024

path <- "D:/Josefina/paper_git/paper_maiac/datasets/V03/aeronet/periodoFaltante_2023-2024/data/USA/"
path_write <- "D:/Josefina/paper_git/paper_maiac/datasets/V03/aeronet/periodoFaltante_2023-2024/proceed/USA/"

interpolate_s(path,path_write)




#######################################################################
## OBJETIVO: promedio de las mediciones de AERONET 
# para un intervalo de tiempo determinado centrado en el paso del satélite,
# para compararlo con el promedio de las recuperaciones de MODIS.
##
#######################################################################


time_correlation <- function(path_aeronet,path_modis,time_buffer){
  #De acuerdo a la literatura, ventanas: 15min, 30min, 60min, 90min, 120min
  # Datos AERONET, previamente procesados
  data_aeronet <- read.csv(path_aeronet, header=TRUE, sep=",", dec=".", na.strings = "NA", stringsAsFactors = FALSE)
  # Formato de la fecha
  data_aeronet$date <- as.POSIXct(strptime(data_aeronet$date, format = "%d/%m/%Y %H:%M", "GMT"))#"%Y-%m-%d %H:%M",
  # Datos MODIS, previamente procesados
  data_sat <- read.csv(path_modis, header=TRUE, sep=",",dec=".", stringsAsFactors = FALSE, na.strings = "NA")
  #Remover NA 
  data_modis <- data_sat  [complete.cases(data_sat$AOD),]
  # Formato de la fecha
  data_modis$date  <- strptime(data_modis$dia, tz= "GMT", format = "%d/%m/%Y")
  data_modis $timestamp <- paste( data_modis$dia, data_modis$hora, sep = " ")
  data_modis $hour <- strptime( data_modis$timestamp, tz= "GMT", format = "%d/%m/%Y %H:%M")
  MODIS_aeronet <- data.frame()
  AOD <- data.frame()
  
  for (i in 1: nrow(data_modis)){ 
    if (i %% 50 == 0) {
      print (i)
    }
    #Se busca la correspondencia día-mes-año entre AERONET y MODIS
    table_aeronet<- data_aeronet 
    eq_year <- which(year(table_aeronet$date) == year(data_modis[i,]$date))
    
    table_aeronet<- table_aeronet[eq_year,] 
    
    eq_month <- which(month(table_aeronet$date) == month(data_modis[i,]$date))
    table_aeronet<- table_aeronet[eq_month,] 
    
    eq_day <- which(day(table_aeronet$date) == day(data_modis[i,]$date))
    table_aeronet<- table_aeronet[eq_day,]
    dim_table <- dim(table_aeronet)
    # Si la dimension de la tabla (no entro nada), hacer un df con NA
    if(dim_table[1] == 0){
      out_data <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA)   
      # SINO se arma un df para agregarle la info que corresponda
    }else{ 
      #If there is a match, the AERONET time window is searched.
      table_dif <-data.frame()
      # Si existe una coincidencia, se busca la ventana de tiempo de AERONET.
      
      mach <- which(abs(difftime(table_aeronet$date, data_modis[i,]$hour,units = "mins")) <time_buffer)
      
      #Filtar la coincidencia
      table_dif <- table_aeronet[mach,]
      dim_table <- dim(table_dif)
      # Se crea el archivo de salida con los datos de MAIAC y AERONET co-localizados.
      if(dim_table[1] == 0){  
        df <- data.frame()
        df <- data.frame(NA, NA,NA, NA, NA,NA,NA,NA)
        names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                       "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
      }else{
        #The output file is created with co-located modis and AERONET data.
        out_data <- data.frame(mean(table_dif[,5],  na.rm=TRUE),
                               median(table_dif[,5],  na.rm=TRUE),
                               sd(table_dif[,5], na.rm=TRUE), (dim_table[1]))
        names(out_data) <- c("mean", "mediana","sd","dim")
        df <- data.frame() 
        df <- data.frame(data_modis[i,10],data_modis[i,4], data_modis[i,8], substr(table_dif[1,1],1,10),out_data[,1:4])
        names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                            "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
        
      }
      AOD <- rbind(AOD, df)
      
      names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                     "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
      AOD <- AOD[complete.cases(AOD),]
    }
  }
  return(AOD)
}


#################################################################
######     -------  Ejemplo para una estacion     -------  ######
rm(list = setdiff(ls(), "time_correlation"))
city <- "SP"
buffer_spatial <- "3KM"
buffer_time <- 120 #minutes
num_site<- "1"
#Change directory
data_modis <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/modis/Latinoamerica/",city,"/",city,"-",buffer_spatial,"-MODIS_V03.csv",sep="")
data_aeronet <-paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/aeronet/datasets_interp_s_L02/Latam/",num_site,"_",city,"_2015-2024_interp-s_V03_L2.csv",sep="")
combinate <- time_correlation (path_aeronet=data_aeronet,path_modis=data_modis,time_buffer=buffer_time)
#View(combinate)
# Save the file with co-located data from AERONET and modis on local path
write.csv (combinate,paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/processed/merge_AER-MODIS/Latam/tot/",num_site,"_",city,"-",buffer_spatial,"-MODIS-",buffer_time,"-AER.csv",sep=""))


###############################################################################
# PROMEDIOS DIARIOS
promedios <- function(combinate){
  rbind_combinate <- data.frame()
  combinate$date <-   as.POSIXct(strptime(combinate$Date_MODIS, format = "%Y-%m-%d %H:%M", "GMT"))
  combinate%>%
    group_by(date) %>%  
    group_split() ->combinate_group
  
  for (i in 1:length(combinate_group)){
    df <- data.frame( date = combinate_group[[i]][["date"]][1],
                      AOD_550_modis = mean(combinate_group[[i]][["AOD_550_modis"]],na.rm=T),
                      AOD_550_AER_mean = mean(combinate_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
    rbind_combinate <- rbind(rbind_combinate,df)
  }
  return(rbind_combinate)
  
}
rm(list = setdiff(ls(), "promedios"))
buffer_time <- 120 #minutes
buffer_spatial <- "3KM"
city <- "SP"
num_estacion <- 1
# BA
combinate <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/processed/merge_AER-MODIS/Latam/tot/",buffer_spatial,"/",num_estacion ,"_",city ,"-",buffer_spatial ,"-MODIS-",buffer_time ,"-AER.csv",sep=""))
combinate_promedios <- promedios(combinate)
write.csv(combinate_promedios,paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/processed/merge_AER-MODIS/Latam/dia/",buffer_spatial,"/",num_estacion,"_",city,"-",buffer_spatial,"-MODIS-",buffer_time,"-AER_MEAN.csv",sep=""))



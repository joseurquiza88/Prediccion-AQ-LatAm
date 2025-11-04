
#######################################################################
## OBJETIVO: Realizar el promedio de las mediciones de AERONET
# para un intervalo de tiempo determinado centrado en el paso del satélite,
# con el fin de compararlo con el promedio de las recuperaciones de MAIAC.
##
#######################################################################

time_correlation <- function(path_aeronet,path_maiac,time_buffer,formato_fecha){

  #De acuerdo a la literatura, ventanas: 15min, 30min, 60min, 90min, 120min
  # Datos AERONET, previamente procesados
   data_aeronet <- read.csv(path_aeronet, header=TRUE, sep=",", dec=".", na.strings = "NA", stringsAsFactors = FALSE)
   # Formato de la fecha
   data_aeronet$date <- as.POSIXct(strptime(data_aeronet$date, format = formato_fecha, "GMT"))
   # Datos MAIAC, previamente procesados
   data_sat <- read.csv(path_maiac, header=TRUE, sep=",",dec=".", stringsAsFactors = FALSE, na.strings = "NA")
   #Remover NA 
   data_maiac <- data_sat[complete.cases(data_sat$AOD_055),]
   # Formato de la fecha
   data_maiac$date  <- strptime(data_maiac$date, tz= "GMT", format = "%Y%j")
   data_maiac$hour  <- strptime(data_maiac$timestamp, tz= "GMT", format = "%Y%j%H%M")
  
  MODIS_aeronet <- data.frame()
  AOD <- data.frame()

  for (i in 1: nrow(data_maiac)){ 
    if (i %% 50 == 0) {
      print (i)
    }
    #Se busca la correspondencia día-mes-año entre AERONET y MAIAC.
    table_aeronet<- data_aeronet 
    eq_year <- which(year(table_aeronet$date) == year(data_maiac[i,]$date))
    
    table_aeronet<- table_aeronet[eq_year,] 
    
    eq_month <- which(month(table_aeronet$date) == month(data_maiac[i,]$date))
    table_aeronet<- table_aeronet[eq_month,] 
    
    eq_day <- which(day(table_aeronet$date) == day(data_maiac[i,]$date))
    table_aeronet<- table_aeronet[eq_day,]
    dim_table <- dim(table_aeronet)
    # Si la dimension de la tabla (no entro nada), hacer un df con NA
    if(dim_table[1] == 0){
      out_data <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA)   
      # SINO se arma un df para agregarle la info que corresponda
    }else{ 
      # Si existe una coincidencia, se busca la ventana de tiempo de AERONET.
      table_dif <-data.frame()
      
      mach <- which(abs(difftime(table_aeronet$date, data_maiac[i,]$hour,units = "mins")) <time_buffer)
      #Filtar la coincidencia
      table_dif <- table_aeronet[mach,]
      dim_table <- dim(table_dif)
      if(dim_table[1] == 0){  
        df <- data.frame()
        df <- data.frame(NA, NA,NA, NA, NA,NA,NA,NA,NA,NA,NA)
        names(df) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
        
      }else{
        #Se crea el archivo de salida con los datos de MAIAC y AERONET co-localizados.
        out_data <- data.frame(mean(table_dif[,5],  na.rm=TRUE),
                             median(table_dif[,5],  na.rm=TRUE),
                             sd(table_dif[,5], na.rm=TRUE), (dim_table[1]))
        names(out_data) <- c("mean", "mediana","sd","dim")
        df <- data.frame() 
        #df <- data.frame(data_maiac[i,2],data_maiac[i,16], data_maiac[i,10:13], substr(table_dif[1,1],1,10),out_data[,1:4])
        df <- data.frame(data_maiac[i,2],data_maiac[i,16], data_maiac[i,10:13], substr(table_dif[1,1],1,10),out_data[,1:4])
        #df <- data.frame(data_maiac[i,1],data_maiac[i,15], data_maiac[i,9:12], substr(table_dif[1,1],1,10),out_data[,1:4])
        names(df) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
      }
      AOD <- rbind(AOD, df)
      
      names(AOD) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
      AOD <- AOD[complete.cases(AOD),]
    }
  }
  return(AOD)
}

#################################################################
######     -------  Ejemplo para una estacion     -------  ######
rm(list = setdiff(ls(), "time_correlation"))
for (i in 1:1){
  buffer_time <- 30 #minutes
  buffer_spatial <- "1km"
  city <- "GT"
  num_site <- "2"
  region <- "USA"
  # BA
  formato_fecha <- "%d/%m/%Y %H:%M"
  data_maiac <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/maiac/",region,"_C61/",city,"/prueba_",buffer_spatial,"_",city,"_C61_tot_V03.csv",sep="")
  data_aeronet <-paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/aeronet/datasets_interp_s_L02/",region,"/",num_site,"_",city,"_2015-2024_interp-s_V03_L2.csv",sep="")
  combinate_BA <- time_correlation (path_aeronet=data_aeronet,path_maiac=data_maiac,time_buffer=buffer_time,formato_fecha)
  #View(combinate_BA)
  # Guardar
  write.csv (combinate_BA,paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/processed/merge_AER-MAIAC/USA_C61/tot/",buffer_spatial,"/",num_site,"_",city,"-",buffer_spatial,"-MAIAC-",buffer_time,"-AER_C61.csv",sep=""))
}
###############################################################################
###############################################################################
###############################################################################
# PROMEDIOS DIARIOS
promedios <- function(combinate){
  rbind_combinate <- data.frame()
  combinate$date <-   as.POSIXct(strptime(combinate$Date_MODIS, format = "%Y-%m-%d", "GMT"))
  #Agrupar info por fecha
  combinate%>%
    group_by(date) %>%  
    group_split() ->combinate_group
  # Hacer media por cada subgrupo de fecha
  for (i in 1:length(combinate_group)){
    df <- data.frame( date = combinate_group[[i]][["date"]][1],
                      AOD_550_maiac_mean = mean(combinate_group[[i]][["AOD_550_maiac"]],na.rm=T),
                      AOD_550_AER_mean = mean(combinate_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
    rbind_combinate <- rbind(rbind_combinate,df)
  }
  return(rbind_combinate)
  
}
#### --------------------------- ####
# Eliminar info salvo "promedios
rm(list = setdiff(ls(), "promedios"))

for(i in 1:1){
  buffer_time <- 60 #minutes
  buffer_spatial <- "15km"
  city <- "CT"
  num_estacion <- 4
  region <- "USA"
  # BA
  combinate <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/processed/merge_AER-MAIAC/",region,"_C61/tot/",buffer_spatial,"/",num_estacion ,"_",city ,"-",buffer_spatial ,"-MAIAC-",buffer_time ,"-AER_C61.csv",sep=""))
  combinate_promedios <- promedios(combinate)
  write.csv(combinate_promedios,paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/processed/merge_AER-MAIAC/",region,"_C61/dia/",buffer_spatial,"/",buffer_time,"mins/",num_estacion,"_",city,"-",buffer_spatial,"-MAIAC-",buffer_time,"-AER_MEAN_C61.csv",sep=""))
}
###################################v##########

rbind_combinate_MDC <- data.frame()
combinate_MDC$date <-   as.POSIXct(strptime(combinate_MDC$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_MDC%>%
  group_by(date) %>%  
  group_split() ->combinate_MDC_group

for (i in 1:length(combinate_MDC_group)){
  df <- data.frame( date = combinate_MDC_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_MDC_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_maiac_mean = mean(combinate_MDC_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_MDC <- rbind(rbind_combinate_MDC,df)
  }

write.csv(rbind_combinate_MDC,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/MDC-25KM-MAIAC-60-AER_MEAN.csv")


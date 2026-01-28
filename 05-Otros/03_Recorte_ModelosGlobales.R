
################
# Objetivo: recortar  productos globales de PM2.5 para zona de interes

## librerias
library(tidyverse)
library(raster)
library(rasterVis)
library(ncdf4)
library(RColorBrewer)


### Producto: https://sites.wustl.edu/acag/satellites/surface-pm2-5-archive/#V6.GL.01
setwd("D:/Josefina/Proyectos/ProyectoChile/CH/Comparativas_resultados/PM_WUSTL/")

# Nombre del archivo
file <- "V5GL04.HybridPM25.Global.202001-202012.nc"

#conviete en raster
#mp <- raster(file) #Verificar la capa que corresponde a las concentraciones
mp <- raster(file,varname = "GWRPM25",band = 1)
# Vemos la metadata del archivo
mp
# Se ve todo a nivel global
plot(mp)
# Se corta extension de todo chile
# abajo izq ==> -54.85,-76
# arriba derecha ==> -17.5,-66
mp_chile <- crop(mp, extent(-76,-66, -54.85, -17.5))
plot(mp_chile)


#####################################################################################
### Producto: https://zenodo.org/records/10795662

setwd("D:/Josefina/Proyectos/ProyectoChile/CH/Comparativas_resultados/PM_WEI/")

# Nombre del archivo
file <- "GHAP_PM2.5_M1K_202204_V1.nc" #Mensual

#conviete en raster
#mp <- raster(file) #Verificar la capa que corresponde a las concentraciones
mp <- raster(file,varname = "PM2.5")
# Vemos la metadata del archivo
mp
# Se ve todo a nivel global
plot(mp)
# Se corta extension de todo chile
# abajo izq ==> -54.85,-76
# arriba derecha ==> -17.5,-66
mp_chile <- crop(mp, extent(-76,-66, -54.85, -17.5))
plot(mp_chile)



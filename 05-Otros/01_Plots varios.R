#######################################################################
## OBJETIVO: Plots usados para la tesis
##
#######################################################################
library(ggplot2)

# Plot que muestra el % de paises que tienen o no info de monitoreo superficial
categorias <- c("Publican informacion", 
                "Sin redes de monitoreo", 
                "Miden pero no publican")
valores <- c(41, 35, 23)

# Crear dataframe
df <- data.frame(
  categorias = categorias,
  valores = valores
)

# Plot de torta con ggplot2
ggplot(df, aes(x = "", y = valores, fill = categorias)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(valores, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#74c476", "#ef3b2c","#ffeda0")) +
  theme_void() #+
  # ggtitle("Disponibilidad de información sobre calidad del aire")

  
  
  library(ggplot2)
  
  # Datos
  categorias <- c("Publican información", 
                  "Sin redes de monitoreo", 
                  "Miden pero no publican")
  valores <- c(41, 35, 23)
  
  df <- data.frame(
    categorias = factor(categorias, levels = c("Publican información", 
                                               "Sin redes de monitoreo", 
                                               "Miden pero no publican")),
    valores = valores
  )
  
  # Gráfico de donut limpio y profesional
  ggplot(df, aes(x = 2, y = valores, fill = categorias)) +
    geom_bar(stat = "identity", width = 1, color = "white", size = 1.2) + # borde blanco más visible
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +   # espacio central para efecto donut
    scale_fill_manual(values = c("Publican información" = "#66c2a4",   # verde
                                 "Sin redes de monitoreo" = "#d7301f",  # rojo
                                 "Miden pero no publican" = "#ffeda0")) + # naranja
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none") #+
    ggtitle("Disponibilidad de información sobre calidad del aire")
  
    
    
    library(ggplot2)
    
    df <- data.frame(
      grupo = c("Países sin monitoreo", "Países que no publican datos"),
      aumento = c(20, 35)
    )
    
    ggplot(df, aes(x = aumento, y = grupo, fill = grupo)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = paste0(aumento, "%")), hjust =-1, size = 5) +
      scale_fill_manual(values = c("#ef3b2c", "#ffb347")) +
      theme_classic() +
      theme(legend.position = "none") +
      xlab("Aumento de mortalidad atribuible a contaminación del aire") +
      ylab("") #+
      ggtitle("Impacto de la falta de monitoreo y publicación de datos")
 
      
      
      ####################################
      
      
      
      df <- read.csv("D:/Josefina/Proyectos/Tesis/data/modelos.csv")
      names(df)
      
      library(ggplot2)
      
      ggplot(data = df, aes(x = year, y = frecuencia, color = Modelo)) +
        #geom_line() +       # si querés líneas
        geom_point() +      # para que se vean los puntos
        labs(x = "Año", y = "Frecuencia", color = "Modelo") +
        theme_classic()
      
      library(ggplot2)
      
      ggplot(data = df, aes(x = year, y = frecuencia, color = Modelo)) +
        geom_point(size = 3) +
        labs(x = "Año", y = "Frecuencia", color = "Modelo") +
        theme_classic() +
        scale_color_brewer(palette = "Set1")
      
      
      library(ggplot2)
      
      colores_11 <- c(
        "#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
        "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe",
        "#008080"
      )
      
      ggplot(df, aes(x = year, y = frecuencia, color = Modelo)) +
        geom_point(size = 3) +
        labs(x = "Año", y = "Frecuencia", color = "Modelo") +
        theme_classic() +
        scale_color_manual(values = colores_11)
      
      # Ordeno los niveles según la columna 'ordenado'
      niveles_ordenados <- df %>%
        dplyr::arrange(ordenado) %>%
        dplyr::pull(Modelo) %>%
        unique()
      
      # Convierto 'Modelo' en factor con niveles ordenados
      df$Modelo <- factor(df$Modelo, levels = niveles_ordenados)
      
      # Ahora el ggplot respetará ese orden en la leyenda
      ggplot(df, aes(x = year, y = frecuencia, color = Modelo)) +
        geom_point(size = 3) +
        labs(x = "Año", y = "Frecuencia", color = "Modelo") +
        theme_classic() +
        scale_color_manual(values = colores_11)
      ggplot(df, aes(x = year, y = frecuencia, color = Modelo)) +
        geom_point(size = 3) +
        labs(x = "Año", y = "Frecuencia (%)", color = "Modelo") +
        theme_classic() +
        scale_color_manual(values = colores_11) +
        scale_x_continuous(breaks = seq(2000, 2020, by = 2))
      # ticks para todos los años entre 2000 y 2020
      
      
      
      
      ################################
      #Producto satelital
      df <- data.frame(sensor = c("GOME", "MODIS","MISR", "MODIS","SCIAMACHY", "OMI",
                                  "GOME-2", "VIIRS", "OLI", "MSI", "TROPOMI"),
                       plataforma =c("ERS-2", "TERRA","TERRA", "AQUA", "ENVISAT", "AURA",
                                     "METOP-A",  "Suomi NPP", "Landsat-8", "Sentinel-2",
                                     "Sentinel-5P"),
                       fecha_inicio = c(1995, 2000 , 2000, 2002, 2002, 2004, 2006,2011,
                                        2013, 2015, 2017),
                       fecha_final = c(2003, 2024 , 2024, 2024, 2012, 2024, 2024,2024,
                                       2024, 2024, 2024))
      df$sensorPlat <- paste (df$sensor," (",df$plataforma,")",sep="")
      
      library(ggplot2)
      
      library(ggplot2)
      
      ggplot(df, aes(x = fecha_inicio, xend = fecha_final, 
                     y = factor(sensorPlat, levels = sensorPlat), 
                     yend = factor(sensorPlat, levels = sensorPlat))) +
        geom_segment(size = 6, color = "steelblue") +
        theme_classic() +
        scale_x_continuous(breaks = seq(1995, 2024, by = 5)) +
        labs(x = " ", y = "Sensor (Plataforma)")+
        #    title = "Período de operación de sensores satelitales") +
        theme(axis.text.y = element_text(size = 10),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
      
# Cargar librería
library(ggplot2)

# Datos
categorias <- c("Publican información", 
                "Sin redes de monitoreo", 
                "Miden pero no publican")
valores <- c(41, 35, 23)

# Crear dataframe
df <- data.frame(
  categorias = categorias,
  valores = valores
)

# Crear gráfico de torta con ggplot2
ggplot(df, aes(x = "", y = valores, fill = categorias)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(valores, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#74c476", "#ef3b2c","#ffeda0")) +
  theme_void() #+
  ggtitle("Disponibilidad de información sobre calidad del aire")

  
  
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
      
##############################################################################
# app.R
##############################################################################
setwd("D:/Josefina/Proyectos/Tesis/Code/app")
library(shiny)
library(leaflet)
library(terra)
library(dplyr)
library(stringr)
library(ggplot2)

# -----------------------------
# CONFIG: carpeta con TIFFs
# -----------------------------
ruta <- "./data"  # Asegurate que todos los TIFF estén aquí

# -----------------------------
# Función para leer y parsear nombres
# -----------------------------
leer_info_archivos <- function(ruta) {
  files <- list.files(ruta, pattern = "\\.tif$", full.names = TRUE)
  if(length(files) == 0) return(NULL)
  nombres <- basename(files)
  
  pat <- "^[^_]+_PM2\\.5_M_(\\d{2})-(\\d{4})_([A-Za-z]{2,3})"
  m <- str_match(nombres, pat)
  
  df <- data.frame(
    archivo = files,
    nombre  = nombres,
    mes_txt = m[,2],
    anio_txt = m[,3],
    ciudad_txt = toupper(m[,4]),
    stringsAsFactors = FALSE
  )
  
  df_clean <- df %>%
    filter(!is.na(mes_txt), !is.na(anio_txt), !is.na(ciudad_txt)) %>%
    transmute(
      archivo = archivo,
      nombre  = nombre,
      mes     = as.integer(mes_txt),
      anio    = as.integer(anio_txt),
      ciudad  = ciudad_txt
    ) %>%
    arrange(ciudad, anio, mes)
  
  if(nrow(df_clean) == 0) return(NULL)
  return(df_clean)
}

info <- leer_info_archivos(ruta)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Concentraciones mensuales de PM2.5 (2015-2024)"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("ui_ciudad"),
      uiOutput("ui_anio"),
      uiOutput("ui_mes"),
      sliderInput("opacity", "Opacidad:", min = 0, max = 1, value = 0.8),
      br(),
      verbatimTextOutput("debug_msg")
    ),
    
    mainPanel(
      leafletOutput("mapa", height = 520),
      hr(),
      h4("Valor del píxel seleccionado:"),
      textOutput("valor_pixel"),
      hr(),
      h4("Serie temporal del píxel"),
      plotOutput("serie_plot", height = 300)
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  if(is.null(info)) {
    output$ui_ciudad <- renderUI({ h4("No se encontraron TIFF mensuales.") })
    return()
  }
  
  # UI dinámicos
  output$ui_ciudad <- renderUI({
    selectInput("ciudad", "Ciudad:", choices = sort(unique(info$ciudad)))
  })
  
  output$ui_anio <- renderUI({
    selectInput("anio", "Año:", choices = sort(unique(info$anio)))
  })
  
  output$ui_mes <- renderUI({
    sliderInput("mes", "Mes:", min = 1, max = 12, value = 1)
  })
  
  observeEvent(input$ciudad, {
    años <- sort(unique(info %>% filter(ciudad == input$ciudad) %>% pull(anio)))
    updateSelectInput(session, "anio", choices = años, selected = min(años))
  })
  
  # Paleta de colores
  pal <- colorNumeric(
    palette = c("#1a9850", "#ffffbf", "#FF8000","#d73027", "#8B00FF"),
    domain = c(0,60), na.color = "transparent"
  )
  
  archivo_filtrado <- reactive({
    req(input$ciudad, input$anio, input$mes)
    fila <- info %>% filter(ciudad == input$ciudad, anio == input$anio, mes == input$mes)
    if(nrow(fila)==0) return(NULL)
    fila$archivo[1]
  })
  
  # Mapa base
  output$mapa <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng=-60, lat=-12, zoom=4)
  })
  
  # Mostrar raster en el mapa
  observe({
    ruta_tif <- archivo_filtrado()
    if(is.null(ruta_tif)) return()
    
    r <- tryCatch(terra::rast(ruta_tif), error=function(e) NULL)
    if(is.null(r)) return()
    
    leafletProxy("mapa") %>%
      clearImages() %>% clearControls() %>%
      addRasterImage(r, colors = pal, opacity = input$opacity) %>%
      addLegend(pal = pal, values = c(0,60), title = "PM2.5 (µg/m³)")
    
    output$debug_msg <- renderText(paste("Mostrando:", basename(ruta_tif)))
  })
  
  # Guardar info del clic
  click_info <- reactiveVal(NULL)
  
  observeEvent(input$mapa_click, {
    click <- input$mapa_click
    ruta_tif <- archivo_filtrado()
    if(is.null(ruta_tif)) return()
    
    r <- tryCatch(terra::rast(ruta_tif), error=function(e) NULL)
    if(is.null(r)) return()
    
    val <- terra::extract(r, matrix(c(click$lng, click$lat), ncol=2))[1,1]
    
    click_info(list(lon = click$lng, lat = click$lat, value = val))
    
    leafletProxy("mapa") %>%
      clearPopups() %>%
      addPopups(click$lng, click$lat,
                if(!is.na(val)) paste0("<b>", round(val,2), " µg/m³</b>") else "Sin datos")
    
    output$valor_pixel <- renderText({
      if(is.na(val)) "Sin datos"
      else paste0("Lon: ", round(click$lng,4),
                  " | Lat: ", round(click$lat,4),
                  " | PM2.5: ", round(val,2), " µg/m³")
    })
  })
  
  # Serie temporal automática al clic
  observe({
    xy <- click_info()
    req(xy)
    
    ciu <- input$ciudad
    info_ciudad <- info %>% filter(ciudad == ciu) %>% arrange(anio, mes)
    
    
    vals <- sapply(info_ciudad$archivo, function(f) {
      r <- try(terra::rast(f), silent = TRUE)
      if(inherits(r, "try-error")) return(NA)  # si falla, devolvemos NA
      terra::extract(r, matrix(c(xy$lon, xy$lat), ncol=2))[1,1]
    })
    
    
    
    
    df_series <- info_ciudad %>%
      mutate(fecha = as.Date(paste(anio, mes, "01", sep="-")),
             pm25 = as.numeric(vals))
    
    output$serie_plot <- renderPlot({
      ggplot(df_series, aes(x=fecha, y=pm25)) +
        geom_line(size=0.9, na.rm=TRUE) +
        geom_point(size=1.8, na.rm=TRUE) +
        geom_vline(xintercept=as.Date(paste(input$anio, input$mes, "01", sep="-")),
                   color="red", linetype="dashed") +
        labs(x="Fecha", y="PM2.5 (µg/m³)") +
        theme_minimal(base_size=14) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
    })
  })
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui, server)


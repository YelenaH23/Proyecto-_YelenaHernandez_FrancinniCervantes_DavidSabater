
library(shiny)
library(dplyr)
library(readr)



base_final <- read_csv("base_final.csv")


ui <- fluidPage(
  titlePanel(
    "Competitividad cantonal, femicidios y matrícula estatal en Costa Rica"
  ),
  
  sidebarLayout(
    
      sidebarPanel(
      
      h4("Filtros de exploración"),
      
      # Filtro por año
      selectInput(
        inputId = "anio",
        label = "Seleccione el año:",
        choices = sort(unique(base_final$anio)),
        selected = sort(unique(base_final$anio)),
        multiple = TRUE
      ),
      
      # Filtro por canton
      selectInput(
        inputId = "canton",
        label = "Seleccione el cantón:",
        choices = sort(unique(base_final$canton_original)),
        selected = NULL,
        multiple = TRUE,
        selectize = TRUE
      ),
      
      # Filtro por categoria de competitividad
      selectInput(
        inputId = "categoria",
        label = "Categoría de competitividad:",
        choices = c(
          "Todas",
          sort(unique(base_final$categoria_competitividad))
        ),
        selected = "Todas"
      ),
      
      # Filtro por rango del indice de competitividad cantonal
      sliderInput(
        inputId = "rango_icc",
        label = "Rango del índice de competitividad:",
        min = floor(min(base_final$indice_competitividad, na.rm = TRUE)),
        max = ceiling(max(base_final$indice_competitividad, na.rm = TRUE)),
        value = c(
          floor(min(base_final$indice_competitividad, na.rm = TRUE)),
          ceiling(max(base_final$indice_competitividad, na.rm = TRUE))
        ),
        step = 1
      )
    ),
    

    
    # aqui parte de chiquillos
    mainPanel(
      h3("Resultados del análisis")
    )
  )
)

server <- function(input, output, session) {
  
 
  datos_filtrados <- reactive({
    
    datos <- base_final %>%
      filter(
        anio %in% input$anio,
        indice_competitividad >= input$rango_icc[1],
        indice_competitividad <= input$rango_icc[2]
      )
    
    
    if (length(input$canton) > 0) {
      datos <- datos %>%
        filter(canton_original %in% input$canton)
    }
 
    if (input$categoria != "Todas") {
      datos <- datos %>%
        filter(categoria_competitividad == input$categoria)
    }
    
   
    datos
  })
  
  # aqui los chiquillos agregan renderPlot(), renderTable() etc
}


shinyApp(ui = ui, server = server)

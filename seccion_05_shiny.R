library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)   # para pivot_longer


# Cargar datos
base_final <- read_csv("base_final.csv")


resumen_estadistico <- function(datos, variable) {
  valores <- datos[[variable]]
  data.frame(
    variable = variable,
    minimo = min(valores, na.rm = TRUE),
    media = mean(valores, na.rm = TRUE),
    mediana = median(valores, na.rm = TRUE),
    desviacion_estandar = sd(valores, na.rm = TRUE),
    maximo = max(valores, na.rm = TRUE)
  )
}

medidas_posicion <- function(datos, variable) {
  valores <- datos[[variable]]
  data.frame(
    variable = variable,
    Q1 = quantile(valores, 0.25, na.rm = TRUE),
    Mediana = quantile(valores, 0.50, na.rm = TRUE),
    Q3 = quantile(valores, 0.75, na.rm = TRUE),
    varianza = var(valores, na.rm = TRUE),
    desviacion_estandar = sd(valores, na.rm = TRUE)
  )
}

resumen_estadistico_canton <- function(datos, variable) {
  cantones <- unique(datos$canton)
  resultado <- do.call(rbind, lapply(cantones, function(c) {
    datos_filtrados <- datos[datos$canton == c, ]
    valores <- datos_filtrados[[variable]]
    data.frame(
      variable = variable,
      canton = c,
      minimo = min(valores, na.rm = TRUE),
      media = mean(valores, na.rm = TRUE),
      mediana = median(valores, na.rm = TRUE),
      desviacion_estandar = sd(valores, na.rm = TRUE),
      maximo = max(valores, na.rm = TRUE)
    )
  }))
  return(resultado)
}

medidas_posicion_por_canton <- function(datos, variable) {
  cantones <- unique(datos$canton)
  resultado <- do.call(rbind, lapply(cantones, function(c) {
    datos_filtrados <- datos[datos$canton == c, ]
    valores <- datos_filtrados[[variable]]
    data.frame(
      canton = c,
      variable = variable,
      Q1 = quantile(valores, 0.25, na.rm = TRUE),
      Mediana = quantile(valores, 0.50, na.rm = TRUE),
      Q3 = quantile(valores, 0.75, na.rm = TRUE),
      varianza = var(valores, na.rm = TRUE),
      desviacion_estandar = sd(valores, na.rm = TRUE)
    )
  }))
  return(resultado)
}

# Lista de variables por dimensión
variables_por_dimension <- list(
  competitividad = c("indice_competitividad", "pilar_economico", "pilar_empresarial",
                     "pilar_gobierno", "pilar_laboral", "pilar_infraestructura",
                     "pilar_innovacion", "pilar_calidad_vida"),
  violencia = c("femicidio_art_21", "femicidio_otros_contextos", "femicidio_ampliado",
                "sospecha_femicidio", "femicidios_registrados", "muertes_violentas_mujeres_total"),
  educacion = c("matricula_estatal_total", "matricula_mujeres", "matricula_hombres",
                "matricula_sexo_no_especificado", "porcentaje_mujeres", "porcentaje_hombres",
                "matricula_primer_ingreso", "matricula_no_primer_ingreso")
)



ui <- fluidPage(
  titlePanel(
    "Competitividad cantonal, femicidios y matrícula estatal en Costa Rica"
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtros de exploración"),
      
      selectInput(
        inputId = "anio",
        label = "Seleccione el año:",
        choices = sort(unique(base_final$anio)),
        selected = sort(unique(base_final$anio)),
        multiple = TRUE
      ),
      
      selectInput(
        inputId = "canton",
        label = "Seleccione el cantón:",
        choices = sort(unique(base_final$canton_original)),
        selected = NULL,
        multiple = TRUE,
        selectize = TRUE
      ),
      
      selectInput(
        inputId = "categoria",
        label = "Categoría de competitividad:",
        choices = c("Todas", sort(unique(base_final$categoria_competitividad))),
        selected = "Todas"
      ),
      
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
    
    mainPanel(
      h3("Resultados del análisis"),
      tabsetPanel(
        tabPanel("Femicidios por año", plotOutput("grafico1")),
        tabPanel("ICC y femicidios", plotlyOutput("grafico2")),
        tabPanel("Top Cantones", plotOutput("grafico3")),
        tabPanel("Matrícula femenina (boxplot)", plotlyOutput("grafico4")),
        tabPanel("Matrícula femenina vs Femicidios", plotOutput("grafico5")),
        tabPanel("Evolución matrícula", plotlyOutput("grafico6")),
        # NUEVA PESTAÑA: Estadísticas Descriptivas
        tabPanel(
          "Estadísticas Descriptivas",
          fluidRow(
            column(3,
                   selectInput("dimension", "Dimensión:",
                               choices = c("Educación" = "educacion",
                                           "Competitividad" = "competitividad",
                                           "Violencia" = "violencia"))
            ),
            column(3,
                   selectInput("nivel", "Nivel:",
                               choices = c("Nacional" = "nacional",
                                           "Por Cantón" = "canton"))
            ),
            column(3,
                   selectInput("tipo_estad", "Tipo de estadística:",
                               choices = c("Resumen estadístico" = "resumen",
                                           "Medidas de posición" = "posicion"))
            ),
            column(3,
                   downloadButton("descargar_tabla", "Descargar CSV",
                                  class = "btn-primary")
            )
          ),
          br(),
          fluidRow(
            DTOutput("tabla_descriptiva")
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
  # Datos reactivos filtrados según los controles
  datos_filtrados <- reactive({
    datos <- base_final %>%
      filter(
        anio %in% input$anio,
        indice_competitividad >= input$rango_icc[1],
        indice_competitividad <= input$rango_icc[2]
      )
    
    if (length(input$canton) > 0) {
      datos <- datos %>% filter(canton_original %in% input$canton)
    }
    
    if (input$categoria != "Todas") {
      datos <- datos %>% filter(categoria_competitividad == input$categoria)
    }
    
    datos
  })
  
  # Tabla descriptiva reactiva (se usa para mostrar y descargar)
  tabla_descriptiva_reactive <- reactive({
    datos <- datos_filtrados()
    dim <- input$dimension
    nivel <- input$nivel
    tipo <- input$tipo_estad
    
    vars <- variables_por_dimension[[dim]]
    if (is.null(vars)) return(NULL)
    
    if (tipo == "resumen") {
      if (nivel == "nacional") {
        tabla <- do.call(rbind, lapply(vars, function(v) resumen_estadistico(datos, v)))
      } else {
        tabla <- do.call(rbind, lapply(vars, function(v) resumen_estadistico_canton(datos, v)))
      }
    } else { # posicion
      if (nivel == "nacional") {
        tabla <- do.call(rbind, lapply(vars, function(v) medidas_posicion(datos, v)))
      } else {
        tabla <- do.call(rbind, lapply(vars, function(v) medidas_posicion_por_canton(datos, v)))
      }
    }
    tabla
  })
  
  # Renderizar tabla con DT
  output$tabla_descriptiva <- renderDT({
    tabla <- tabla_descriptiva_reactive()
    if (is.null(tabla)) return(NULL)
    datatable(tabla, options = list(scrollX = TRUE, pageLength = 15, dom = 'Bfrtip'),
              rownames = FALSE)
  })
  
  # Descarga de la tabla en CSV
  output$descargar_tabla <- downloadHandler(
    filename = function() {
      paste0("estadisticas_", input$dimension, "_", input$nivel, "_", input$tipo_estad, ".csv")
    },
    content = function(file) {
      tabla <- tabla_descriptiva_reactive()
      if (!is.null(tabla)) write.csv(tabla, file, row.names = FALSE)
    }
  )
  
  
  output$grafico1 <- renderPlot({
    femicidios_anio <- datos_filtrados() %>%
      group_by(anio) %>%
      summarise(femicidios_registrados = sum(femicidios_registrados, na.rm = TRUE),
                .groups = "drop")
    ggplot(femicidios_anio, aes(x = factor(anio), y = femicidios_registrados)) +
      geom_col(fill = "darkseagreen") +
      geom_text(aes(label = femicidios_registrados), vjust = -0.4) +
      labs(title = "Femicidios por año", x = "Año", y = "Cantidad de femicidios") +
      theme_minimal()
  })
  
  output$grafico2 <- renderPlotly({
    datos <- datos_filtrados() %>%
      mutate(texto = paste(
        "Cantón:", canton,
        "<br>Año:", anio,
        "<br>ICC:", round(indice_competitividad, 2),
        "<br>Categoría ICC:", categoria_competitividad,
        "<br>Femicidios registrados:", femicidios_registrados
      ))
    p <- ggplot(datos, aes(x = indice_competitividad, y = femicidios_registrados,
                           color = categoria_competitividad, text = texto)) +
      geom_point(size = 3, alpha = 0.8) +
      labs(title = "Relación entre competitividad cantonal y femicidios",
           x = "Índice de Competitividad Cantonal",
           y = "Femicidios registrados",
           color = "Categoría ICC") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$grafico3 <- renderPlot({
    top15 <- datos_filtrados() %>%
      group_by(canton_original) %>%
      summarise(femicidios_registrados = sum(femicidios_registrados, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(desc(femicidios_registrados)) %>%
      slice_head(n = 15) %>%
      mutate(canton_original = factor(canton_original, levels = rev(canton_original)))
    ggplot(top15, aes(x = canton_original, y = femicidios_registrados)) +
      geom_col(fill = "darkslategray3") +
      coord_flip() +
      geom_text(aes(label = femicidios_registrados), hjust = -0.8) +
      labs(title = "Top 15 cantones con más femicidios registrados",
           x = "Cantón", y = "Femicidios acumulados") +
      theme_minimal()
  })
  
  output$grafico4 <- renderPlotly({
    p <- ggplot(datos_filtrados(),
                aes(x = categoria_competitividad, y = porcentaje_mujeres,
                    fill = categoria_competitividad,
                    text = paste("Categoría ICC:", categoria_competitividad,
                                 "<br>Porcentaje mujeres:", round(porcentaje_mujeres, 1), "%"))) +
      geom_boxplot(alpha = 0.8) +
      labs(title = "Porcentaje de matrícula femenina según categoría ICC",
           x = "Categoría de competitividad", y = "Porcentaje mujeres matriculadas") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$grafico5 <- renderPlot({
    comparacion_larga <- base_final %>%
      group_by(categoria_competitividad) %>%
      summarise(
        `Matrícula femenina (%)` = mean(porcentaje_mujeres, na.rm = TRUE),
        `Femicidios promedio` = mean(femicidios_registrados, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(`Matrícula femenina (%)`, `Femicidios promedio`),
                   names_to = "variable", values_to = "valor")
    
    ggplot(comparacion_larga, aes(x = categoria_competitividad, y = valor, fill = categoria_competitividad)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = round(valor, 1)), vjust = -0.2, size = 3.5) +
      labs(title = "Matrícula femenina y femicidios por categoría ICC",
           x = "Categoría ICC", y = "Valor") +
      theme_minimal()
  })
  
  output$grafico6 <- renderPlotly({
    matricula_anio_cat <- base_final %>%
      group_by(anio, categoria_competitividad) %>%
      summarise(porcentaje_mujeres = mean(porcentaje_mujeres, na.rm = TRUE), .groups = "drop")
    p <- ggplot(matricula_anio_cat,
                aes(x = anio, y = porcentaje_mujeres, color = categoria_competitividad,
                    group = categoria_competitividad,
                    text = paste("Año:", anio,
                                 "<br>Categoría ICC:", categoria_competitividad,
                                 "<br>Porcentaje mujeres:", round(porcentaje_mujeres, 1), "%"))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = "Evolución del porcentaje de matrícula femenina",
           x = "Año", y = "Porcentaje de mujeres matriculadas",
           color = "Categoría ICC") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)

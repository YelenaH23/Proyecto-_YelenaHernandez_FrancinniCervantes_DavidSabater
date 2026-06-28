#Scrip de Yelena


#Librerias

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(readr)
library(openxlsx)
library(plotly)

# Carga de las base de datos 


#```{r}
base_final <- read_csv("base_final.csv")
View(base_final)
names(base_final)
#```


#*Visualizaciones*
  
  
#  Gráfico 1: 
#  ```{r echo=FALSE}
femicidios_anio <- aggregate(femicidios_registrados ~ anio, data = base_final, FUN = sum)

grafico1 <- ggplot(femicidios_anio, aes(x = factor(anio), y = femicidios_registrados)) +
  geom_col(fill = "darkseagreen") + 
  geom_text(aes(label = femicidios_registrados), vjust = -0.4) +
  labs( title = "Femicidios por año", 
        x = "Año", y = "Cantidad de femicidios") +
  theme_minimal()
#```

#Gráfico 2: 
#  ```{r echo=FALSE}
# Se crea una columna de texto personalizada.
# Este texto será el que aparecerá cuando se pase el cursor sobre cada punto.
base_final$texto <- paste(
  "Cantón:", base_final$canton,
  "<br>Año:", base_final$anio,
  "<br>ICC:", round(base_final$indice_competitividad, 2),
  "<br>Categoría ICC:", base_final$categoria_competitividad,
  "<br>Femicidios registrados:", base_final$femicidios_registrados
)

# Se construye el gráfico base con ggplot.
# En el eje x se coloca el Índice de Competitividad Cantonal.
# En el eje y se coloca la cantidad de femicidios registrados.
# El color de cada punto depende de la categoría de competitividad.
grafico2 <- ggplot( base_final,
                                  aes( x = indice_competitividad, y = femicidios_registrados,
                                       color = categoria_competitividad,
                                       text = texto)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Relación entre competitividad cantonal y femicidios",
    x = "Índice de Competitividad Cantonal",
    y = "Femicidios registrados",
    color = "Categoría ICC"
  ) +
  theme_minimal()

# Se convierte el gráfico de ggplot en un gráfico interactivo.
# tooltip = "text" indica que se debe mostrar el texto personalizado
# cuando se pasa el cursor sobre cada punto.
ggplotly(grafico2, tooltip = "text")

#```


#Gráfico 3: 
#  ```{r echo=FALSE}
# Se agrupan los datos por cantón
femicidios_canton <- aggregate(femicidios_registrados ~ canton, data = base_final, FUN = sum)

#Se ordena la tabla de mayor a menor según la cantidad de femicidios registrados.
femicidios_canton <- femicidios_canton[order(femicidios_canton$femicidios_registrados, decreasing = TRUE), ]

#Se seleccionan únicamente los primeros 15 cantones
top15 <- head(femicidios_canton, 15)

#Se convierte la variable canton en factor
top15$canton <- factor(top15$canton, levels = rev(top15$canton)) 
# Se usa rev() para que el cantón con más femicidios aparezca arriba
# después de aplicar coord_flip().


#Construccion del grafico
grafico3 <- ggplot(top15, aes(x = canton, y = femicidios_registrados)) +
  geom_col(fill = "darkslategray3") +
  coord_flip() +
  geom_text(aes(label = femicidios_registrados), hjust = -0.8) +
  labs( title = "Top 15 cantones con más femicidios registrados",
        x = "Cantón", y = "Femicidios acumulados") +
  theme_minimal()
#```

#Gráfico 4: 
#```{r echo=FALSE}
grafico4 <- ggplot(base_final, 
                   aes(x = categoria_competitividad, y = porcentaje_mujeres, 
                       fill = categoria_competitividad, 
                       text = paste( "Categoría ICC:", categoria_competitividad, 
                                     "<br>Porcentaje mujeres:", round(porcentaje_mujeres, 1), "%"))) + 
  geom_boxplot(alpha = 0.8) + 
  labs( title = "Porcentaje de matricula femenina segun categoria ICC",
        x = "Categoria de competitividad", y = "Porcentaje mujeres matriculadas") 
ggplotly(grafico4, tooltip = "text")
#```

#Gráfico 5: 
# ```{r echo=FALSE}
# Hago primero una tabla resumida en formato largo para poder hacer graficos de 
#barras agrupadas
comparacion_larga <- base_final %>% 
  group_by(categoria_competitividad) %>% 
  summarise(
    `Matrícula femenina (%)` = mean(porcentaje_mujeres, na.rm = TRUE),
    `Femicidios promedio` = mean(femicidios_registrados, na.rm = TRUE),
    .groups = "drop"
  ) %>%
 pivot_longer(cols = c(`Matrícula femenina (%)`,`Femicidios promedio`),
               names_to = "variable", values_to = "valor")
#Construccion del grafico
grafico5 <- ggplot(comparacion_larga, aes(x = categoria_competitividad, y = valor, fill = categoria_competitividad)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = round(valor, 1)),
            vjust = -0.2, size = 3.5) +
  labs(
    title = "Matrícula femenina y femicidios por categoría ICC",
    x = "Categoría ICC",
    y = "Valor",
    fill = "Variable"
  ) +
  theme_minimal()
#```

#Gráfico 6: 
# ```{r echo=FALSE}
matricula_anio_cat <- aggregate(porcentaje_mujeres ~ anio + categoria_competitividad, data = base_final, FUN = mean)

grafico6 <- ggplot(matricula_anio_cat, aes(x = anio, y = porcentaje_mujeres, color = categoria_competitividad, group = categoria_competitividad, text = paste("Año:", anio, "<br>Categoría ICC:", categoria_competitividad, "<br>Porcentaje mujeres:", round(porcentaje_mujeres, 1), "%"))) +  
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs( title = "Evolución del porcentaje de matrícula femenina",
        x = "Año", y = "Porcentaje de mujeres matriculadas", color = "Categoría ICC") +
  theme_minimal()

ggplotly(grafico6, tooltip = "text")
#```


# Seccion 2: Programacion en R.

# Cargar librerias 

library(dplyr)
library(stringr)


# Importamos la base final unificada


base_final <- read.csv(
  "base_final_icc_femicidios_matricula_2022_2024.csv",
  fileEncoding = "UTF-8"
)


# Hacemos revision inicial de la base (mejor confirmar)

head(base_final)
str(base_final)
dim(base_final)



# VECTORES DE VARIABLES NUMERICAS 

# NOTA: Se crea este vector para reunir los nombres de las variables cuantitativas,
#       principales de la base. Esto nos puede permitir seleccionar y reutilizarlas 
#       posteriormente en funciones, tablas estadistias, graficos y analisis sin tener 
#       que escribir el nombre manualmente.


variables_numericas <- c(
  "indice_competitividad",
  "femicidios_registrados",
  "muertes_violentas_mujeres_total",
  "matricula_estatal_total",
  "matricula_mujeres",
  "matricula_hombres",
  "matricula_sexo_no_especificado",
  "porcentaje_mujeres",
  "porcentaje_hombres",
  "matricula_primer_ingreso",
  "matricula_no_primer_ingreso"
)

variables_numericas


# LISTA DE VARIABLES POR DIMENSION 

# NOTA: Se crea la lista para agrupar las variables segun las tres dimienciones
#       principales del proyecto; competitividad, violencia contra las mujeres y educacion 
#       univercitaria estatal
#       Buscamos que facilite la organizacion al seleccionar grupos de variables para analisis,
#       graficos y filtros dentro de shiny


variables_por_dimension <- list(
  competitividad = c( "indice_competitividad",
                       "pilar_economico",
                       "pilar_empresarial",
                       "pilar_gobierno",
                       "pilar_laboral",
                       "pilar_infraestructura",
                       "pilar_innovacion",
                       "pilar_calidad_vida"
    ),
  violencia = c( "femicidio_art_21",
                 "femicidio_otros_contextos",
                 "femicidio_ampliado",
                 "sospecha_femicidio",
                 "femicidios_registrados",
                 "muertes_violentas_mujeres_total"
    ),
  educacion = c("matricula_estatal_total",
                "matricula_mujeres",
                "matricula_hombres",
                "matricula_sexo_no_especificado",
                "porcentaje_mujeres",
                "porcentaje_hombres",
                "matricula_primer_ingreso",
                "matricula_no_primer_ingreso"
    
  )
)

variables_por_dimension$educacion

# FUNCION PARA VERIFICAR VARIABLES 

# NOTA: Esta funcion verifica si una variable indicada por el usuario
#       existe dentro de la base de datos antes de utilizarla en analisis,
#       graficos o filtros para shiny


verificar_variable <- function(datos, variable) {
  
  if (variable %in% names(datos)) {
    
    return(paste("La variable", variable, "sí existe en la base de datos."))
    
  } else {
    
    return(paste("La variable", variable, "no existe en la base de datos."))
  }
}

# Ejemplo de prueba

verificar_variable(
  datos = base_final,
  variable = "indice_competitividad"
)

verificar_variable(
  datos = base_final,
  variable = "variable_inexistente"
)


# FUNCION PARA CREAR ETIQUETAS LEGIBLES

# NOTA: Esta funcion recibe un vector con los nombres de variable y crea etiquetas mas faciles de leer
#       Por ejemplo: "matricula_primer_ingreso" pasa a "Matricula primer ingreso" 
#       Se puede utilizar en tablas, graficos y menus de shiny


crear_etiquetas_variables <- function(variables) {
  
  etiquetas <- character(length(variables))
  
  for (i in seq_along(variables)) {
    
    etiquetas[i] <- variables[i] %>%
      str_replace_all("_", " ") %>%
      str_to_sentence()
  }
  
  names(etiquetas) <- variables
  
  return(etiquetas)
}

etiquetas_variables <- crear_etiquetas_variables(  # crea etiquetas para las variables numericas del proyecto
  variables_numericas
)

# Mostrar 

etiquetas_variables

# Ejemplo

etiquetas_variables["femicidios_registrados"]


# FUNCION PARA CONTAR VALORES FALTANTES 
# NOTA: Esta funcion recibe una base de datos y cuenta valores faltantes existentes en cada una de sus columnas 
#       Se usa saply para aplicar la misma operacion a todas las variables de la base de datos de forma automatica 


contar_faltantes <- function(datos) {
  
  sapply(datos, function(columna) {
    sum(is.na(columna))
  })
}

# Aplicamos la función a la base final

faltantes_base_final <- contar_faltantes(base_final)

# Mostrar la cantidad de valores faltantes por variable

faltantes_base_final

# En el caso de evaluar una variable
contar_faltantes(base_final["indice_competitividad"])


# FUNCION PARA ORGANIZAR VARIABLES POR DIMENSION
# NOTA: Esta funcion recorre la lista de variables por dimencion  y revisa cuales de esas variables
#       existen realmente en la base final.
#       Se utiliza lapply porque la funcion trabaja sobre cada elemento de la lista

verificar_variables_por_dimension <- function(datos, lista_variables) {
  
  lapply(lista_variables, function(grupo_variables) {
    
    grupo_variables[grupo_variables %in% names(datos)]
  })
}

# Aplicamos la funcion a la lista creada anteriormente

variables_disponibles <- verificar_variables_por_dimension(
  datos = base_final,
  lista_variables = variables_por_dimension
)

# Mostrar las variables disponibles por cada dimension

variables_disponibles



# NOTA IMPORTANTE: El punto c queda pendiente porque hay que definir el documento final 
# source("02_funciones_programacion.R")

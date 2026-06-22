# Proyecto programacion para Estadistica I
# Procesamiento de las bases de datos ICC y Femicidios


# =====
 
# BASES DE DATOS DEL ICC

# =====


# CARGAR LAS LIBRERIAS 


library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(janitor)


# FUNCION PARA LIMPIAR EL TEXTO 


limpiar_texto <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII")
}

# IMPORTAMOS LAS DOS BASES DE DATOS 

icc_2022_2023_original <- read_excel(
  "Base del ICC 2022-2023_vf.xls",
  sheet = "Datos"
)

icc_2023_2024_original <- read_excel(
  "Base ICC 2023_2024.xlsx",
  sheet = "Datos"
)

# LIMPIAR ICC 2022 

#NOTA: el primer archivo se utiliza solamente 2022, porque 2023 tambien aparece en el segundo archivo.

icc_2022_limpio <- icc_2022_2023_original %>%
  filter(ano == 2022) %>%  # filtra
  transmute(               # crea una base con las variables que nos funcionan
    anio = as.integer(ano),
    codigo_canton = as.integer(cid),
    canton_original = canton,
    canton = limpiar_texto(canton),
    
    indice_competitividad = icc_p,
    ranking_competitividad = rank_icc_p,
    categoria_competitividad = icc_p_categ,
    
    pilar_economico = peconomico,
    pilar_empresarial = pempresarial,
    pilar_gobierno = pgobierno,
    pilar_laboral = plaboral,
    pilar_infraestructura = pinfraestructura,
    pilar_innovacion = pinnovacion,
    pilar_calidad_vida = pcalidadvida
  )


# LIMPIAR ICC 2023 Y 2024


icc_2023_2024_limpio <- icc_2023_2024_original %>%
  filter(year %in% c(2023, 2024)) %>%
  transmute(
    anio = as.integer(year),
    codigo_canton = as.integer(cid),
    canton_original = canton,
    canton = limpiar_texto(canton),
    
    indice_competitividad = icc,
    ranking_competitividad = icc_rank,
    categoria_competitividad = icc_categ,
    
    pilar_economico = peconomico,
    pilar_empresarial = pempresarial,
    pilar_gobierno = pgobierno,
    pilar_laboral = plaboral,
    pilar_infraestructura = pinfraestructura,
    pilar_innovacion = pinnovacion,
    pilar_calidad_vida = pcalidadvida
  )


# UNIFICAR ICC 2022, 2023 Y 2024 

# NOTA: se usa bind_rows() porque ambas bases tienen las mismas variables, es como agregar una debajo de otra.

icc_unificado <- bind_rows(
  icc_2022_limpio,
  icc_2023_2024_limpio
) %>%
  arrange(codigo_canton, anio)


# Verificamos

# cantidad de registros por ano
icc_unificado %>% # es correcto que en 2024 de 82 en lugar de 81, dado que en este alo se anadio el canton de Rio Cuarto 
  count(anio)

# total de registros
nrow(icc_unificado) 


# revisar si existe algun canton repetido en el mismo ano 
icc_unificado %>%
  count(anio, codigo_canton) %>% 
  filter(n > 1)

# revisar estructura 
glimpse(icc_unificado)


# HOMOGENIZAR LA ESCALA DE LOS INDICADORES

# NOTA:  
#       En 2022 los indices estan entre 0 y 1.
#       En 2023 y 2024 estsn entre 0 y 100.
#       Es por eso que unicamente los datos de 2022 se multiplican por 100.


icc_unificado <- icc_unificado %>%
  mutate(
    across(
      c(
        indice_competitividad,
        pilar_economico,
        pilar_empresarial,
        pilar_gobierno,
        pilar_laboral,
        pilar_infraestructura,
        pilar_innovacion,
        pilar_calidad_vida
      ),
      ~ if_else(anio == 2022, .x * 100, .x)
    )
  )

#  CORREGIR CATEGORIA DE COMPETITIVIDAD

# NOTA: Se reconstruye la categorIa directamente a partir del Indice ya homologado en escala de 0 a 100.


icc_unificado <- icc_unificado %>%
  mutate(
    categoria_competitividad = case_when(
      indice_competitividad < 20 ~ "Muy bajo",
      indice_competitividad < 40 ~ "Bajo",
      indice_competitividad < 60 ~ "Medio",
      indice_competitividad < 80 ~ "Alto",
      TRUE ~ "Muy alto"
    )
  )


# Verificar cuantos cantones quedaron por ano 

icc_unificado %>%
  count(anio, categoria_competitividad) %>%
  arrange(anio, categoria_competitividad)


# CREAR CATALOGO DE CANTONES

# NOTA: esta tabla servira mas adelante para asignar codigos cantonales a matricula y femicidios

catalogo_cantones <- icc_unificado %>%
  select(codigo_canton, canton, canton_original) %>%
  distinct() %>%
  arrange(codigo_canton)

# Verificaciones 

nrow(catalogo_cantones)

# revisar si rio cuarto a aprece en el catalogo 

 catalogo_cantones %>% 
   filter(canton == "RIO CUARTO")
 
 # revisar si hay faltantes en variables principales
 
 icc_unificado %>%
   summarise(
     faltantes_anio = sum(is.na(anio)),
     faltantes_codigo = sum(is.na(codigo_canton)),
     faltantes_canton = sum(is.na(canton)),
     faltantes_icc = sum(is.na(indice_competitividad))
   )
 
 
# GUARDAR LOS RESULTADOS DE LA UNION DE ICC 
 
 write.csv(
   icc_unificado,
   "icc_unificado_2022_2024_limpio.csv",
   row.names = FALSE,
   fileEncoding = "UTF-8"
 )
 
 write.csv(
   catalogo_cantones,
   "catalogo_cantones_icc.csv",
   row.names = FALSE,
   fileEncoding = "UTF-8"
 )
 
 
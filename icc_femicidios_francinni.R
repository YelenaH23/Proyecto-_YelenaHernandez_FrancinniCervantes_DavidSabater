# Proyecto programacion para Estadistica I
# Procesamiento de las bases de datos ICC y Femicidios


#CARGAR LAS LIBRERIAS 

library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(janitor)



# =====
 
# BASES DE DATOS DEL ICC

# =====


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


# Verificamos:

# Cantidad de registros por ano
# NOTA: # es correcto que en 2024 de 82 en lugar de 81, dado que en este alo se anadio el canton de Rio Cuarto 

icc_unificado %>% 
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
 
 
 
 
# =====
 
# BASES DE DATOS DE FEMICIDIOS
 
# =====


# IMPORTAR LA BASE DE DATOS DE FEMICIDIOS

# NOTA: La base utilizada corresponde a homicidiosndoloroos y contine diferentes 
#       clasificaciones de muertes violentas de mujeres.A partir de ella se 
#       identificaron y resumieron las categorias especificas de femicidio para 
#       construir indicadores cantonales anuales.
 
 homicidios_original <- read_excel(
   "Datos_PJCROD_FEMICIDIOS_V2.xlsx",
   sheet = "Homicidios_Dolosos"
 )

# LIMPIAR LA BASE 
 
 homicidios_limpio <- homicidios_original %>%
   rename(
     anio = Periodo,
     tipo_homicidio = DesTipoHomicidio,
     provincia = Provincia_Hecho,
     canton = Canton_Hecho,
     cantidad = Cantidad
   ) %>%
   mutate(
     anio = as.integer(anio),
     cantidad = as.integer(cantidad),
     
     provincia_original = provincia,
     canton_original = canton,
     
     provincia = limpiar_texto(provincia),
     canton = limpiar_texto(canton),

# NOTA: se le aplica limpiar_texto () tambien a tipo de homicidio para evitar 
#       fallos silenciosos por tildes o variaciones.
     
     tipo_homicidio = limpiar_texto(tipo_homicidio)
   ) %>%
   filter(anio %in% c(2022, 2023, 2024))

 

# CREAR BASE CANTONAL RESUMIDA 
# NOTA: Se excluye unicamente el registro que no tiene canton, ya que no 
#       puede asociarse a un codigo cantonal del ICC.
 

 homicidios_cantonal <- homicidios_limpio %>%
   filter(!is.na(canton), canton != "") %>%
   mutate(
     femicidio_art_21 = if_else(
       tipo_homicidio == "FEMICIDIO ARTICULO 21 LPVCM",
       cantidad,
       0L
     ),
     
     femicidio_otros_contextos = if_else(
       tipo_homicidio == "FEMICIDIO EN OTROS CONTEXTOS (ART 21 BIS)",
       cantidad,
       0L
     ),
     
     femicidio_ampliado = if_else(
       tipo_homicidio == "FEMICIDIO AMPLIADO (SEGUN CONVENCION BELEM DO PARA)",
       cantidad,
       0L
     ),
     
     sospecha_femicidio = if_else(
       tipo_homicidio == "SOSPECHA DE FEMICIDIO",
       cantidad,
       0L
     ),
     
     homicidios_mujeres_no_femicidio = if_else(
       tipo_homicidio == "HOMICIDIOS DE MUJERES/NO FEMICIDIOS",
       cantidad,
       0L
     ),
     
     homicidios_mujeres_sin_revision = if_else(
       tipo_homicidio ==
         "HOMICIDIO (MUERTE VIOLENTA DE MUJER INFORME SIN REVISION INICIAL)",
       cantidad,
       0L
     ),
     
     homicidios_mujeres_pendientes = if_else(
       tipo_homicidio ==
         "HOMICIDIO (MUERTE VIOLENTA DE MUJER INFORME PENDIENTE)",
       cantidad,
       0L
     ),
     homicidios_indeterminado = if_else(
       tipo_homicidio == "HOMICIDIOS INDETERMINADO",
       cantidad, 
       0L
     )
     
   ) %>%
   group_by(anio, provincia, canton) %>%
   summarise(
     femicidio_art_21 = sum(femicidio_art_21),
     femicidio_otros_contextos = sum(femicidio_otros_contextos),
     femicidio_ampliado = sum(femicidio_ampliado),
     sospecha_femicidio = sum(sospecha_femicidio),
     homicidios_mujeres_no_femicidio = sum(homicidios_mujeres_no_femicidio),
     homicidios_mujeres_sin_revision = sum(homicidios_mujeres_sin_revision),
     homicidios_mujeres_pendientes = sum(homicidios_mujeres_pendientes),
     homicidios_indeterminado = sum(homicidios_indeterminado),
     
# Variable principal de femicidios para análisis.

 femicidios_registrados = sum(femicidio_art_21) + sum(femicidio_otros_contextos),

# Total de todas las categorías territoriales disponibles.
     
 muertes_violentas_mujeres_total =
       sum( femicidio_art_21) +
       sum( femicidio_otros_contextos) +
       sum( femicidio_ampliado) +
       sum( sospecha_femicidio) +
       sum( homicidios_mujeres_no_femicidio) +
       sum( homicidios_mujeres_sin_revision) +
       sum( homicidios_mujeres_pendientes) +
       sum( homicidios_indeterminado),
     
     .groups = "drop"
   ) %>%
   arrange(anio, provincia, canton)

 
# CATALOGO DE CANTONES 

# NOTA: si se corre el scrit completo, el catalogo ya existe en memoria desde la 
#       seccion del ICC y se omite la lectura del csv, si no se carga desde el archivo guardado
 
 if (!exists("catalogo_cantones")) {
   
   catalogo_cantones <- read.csv(
     "catalogo_cantones_icc.csv",
     fileEncoding = "UTF-8",
     stringsAsFactors = FALSE
   )
   
 }

 
# ASIGNAR CODIGO CANTONAL DESDE EL ICC 
 
 
 homicidios_con_codigo <- homicidios_cantonal %>%
   left_join(
     catalogo_cantones %>%
       select(codigo_canton, canton),
     by = "canton"
   )

 
 names(homicidios_con_codigo)
 
# Revisar si hay cantones que no recibieron codigo 
 
 homicidios_con_codigo %>% 
   filter(is.na(codigo_canton)) %>% 
   select(anio, provincia, canton) %>% 
   distinct()
 
 
# BASE FINAL DE FEMICIDIOS 
 
 femicidios_resumen <- homicidios_con_codigo %>%
   filter(!is.na(codigo_canton)) %>%
   select(
     anio,
     codigo_canton,
     provincia,
     canton,
     femicidio_art_21,
     femicidio_otros_contextos,
     femicidio_ampliado,
     sospecha_femicidio,
     homicidios_mujeres_no_femicidio,
     homicidios_mujeres_sin_revision,
     homicidios_mujeres_pendientes,
     homicidios_indeterminado,          
     femicidios_registrados,
     muertes_violentas_mujeres_total
   ) %>%
   arrange(anio, codigo_canton)

 
# VERIFICAMOS CONTROLES DE CALIDAD 
 
# Totales de casos antes del resumen territorial
 
 homicidios_limpio %>%
   group_by(anio) %>%
   summarise(
     total_original = sum(cantidad),
     .groups = "drop"
   )
 
# Totales territoriales después de excluir el caso sin cantón
 
 homicidios_cantonal %>%
   group_by(anio) %>%
   summarise(
     femicidios_registrados = sum(femicidios_registrados),
     total_territorial = sum(muertes_violentas_mujeres_total),
     .groups = "drop"
   )
 
 
# Revisamos cantones que no recibieron código
 
 homicidios_con_codigo %>%
   filter(is.na(codigo_canton)) %>%
   select(anio, provincia, canton) %>%
   distinct()
 
# Revisamos duplicados por llave final
 
 femicidios_resumen %>%
   count(anio, codigo_canton) %>%
   filter(n > 1)
 
# Revisamos cantidad de registros por año
 
 femicidios_resumen %>%
   count(anio)

# GUARDAMOS LA BASE DE FEMICIDIOS LIMPIA
 
 write.csv(
   femicidios_resumen,
   "femicidios_resumen_2022_2024.csv",
   row.names = FALSE,
   fileEncoding = "UTF-8"
 )
 

 
# =====
 
# UNION ENTRE EL ICC Y FEMICIDIOS
 
# =====
 
 
# ASEGURAMOS EL FORMATO DE LAS VARIABLES DE UNIÓN
 
 icc_unificado <- icc_unificado %>%
   mutate(
     anio = as.integer(anio),
     codigo_canton = as.integer(codigo_canton)
   )
 
 femicidios_resumen <- femicidios_resumen %>%
   mutate(
     anio = as.integer(anio),
     codigo_canton = as.integer(codigo_canton)
   )

# REVISAMOS LOS DUBLICADOS
 
 icc_unificado %>%
   count(anio, codigo_canton) %>%
   filter(n > 1)
 
 
 
 femicidios_resumen %>%
   count(anio, codigo_canton) %>%
   filter(n > 1)
 
 
# UNION DE BASES 
 
# NOTA : Utilizamos left_join() porque ICC es la base principal.
#        Asi se conservan todos los cantones disponibles en ICC,
#        aunque no tengan registros de femicidios en ese año.
       
 
 icc_femicidios <- icc_unificado %>%
   left_join(
     femicidios_resumen %>%
       select(
         anio,
         codigo_canton,
         femicidio_art_21,
         femicidio_otros_contextos,
         femicidio_ampliado,
         sospecha_femicidio,
         homicidios_mujeres_no_femicidio,
         homicidios_mujeres_sin_revision,
         homicidios_mujeres_pendientes,
         homicidios_indeterminado,
         femicidios_registrados,
         muertes_violentas_mujeres_total
       ),
     by = c("anio", "codigo_canton")
   )
 
# Verificamos que el numero de filas no cambio 
 
 nrow(icc_unificado)
 nrow(icc_femicidios)
 

# REEMPLAZAMOS LOS NA POR CERO
 
#NOTA: Los NA significan que ese canton y año no tuvo registros
#      en la base territorial de homicidios dolosos.
 
 icc_femicidios <- icc_femicidios %>%
   mutate(
     across(
       c(
         femicidio_art_21,
         femicidio_otros_contextos,
         femicidio_ampliado,
         sospecha_femicidio,
         homicidios_mujeres_no_femicidio,
         homicidios_mujeres_sin_revision,
         homicidios_mujeres_pendientes,
         femicidios_registrados,
         homicidios_indeterminado,
         muertes_violentas_mujeres_total
       ),
       ~ replace_na(.x, 0L)
     )
   )

 
# CONTROLES DE CALIDAD DE LA UNION
 
# Verificamos la cantidad de registros por ano 
 
 icc_femicidios %>%
   count(anio)
 
# Verificamos el total de filas 
 
 nrow(icc_femicidios)
 
# Revisamos si hay dublicados en la llave principal
 
 icc_femicidios %>% 
   count(anio, codigo_canton) %>%
   filter(n>1)

# Revisamos si quedaron valores faltantes en variables de femicidios
 
 icc_femicidios %>%
   summarise(
     filas_totales = n(),
     faltantes_femicidios = sum(is.na(femicidios_registrados)),
     faltantes_muertes_violentas =
       sum(is.na(muertes_violentas_mujeres_total))
   )
 
# Comparamos los totales antes y despues del join.
 
 femicidios_resumen %>%
   group_by(anio) %>%
   summarise(
     femicidios_origen = sum(femicidios_registrados),
     muertes_origen = sum(muertes_violentas_mujeres_total),
     .groups = "drop"
   )
 
 icc_femicidios %>%
   group_by(anio) %>%
   summarise(
     femicidios_despues_join = sum(femicidios_registrados),
     muertes_despues_join = sum(muertes_violentas_mujeres_total),
     .groups = "drop"
   )
 

# GUARDAR LA BASE FINAL UNIFICADA 
 
 
 write.csv(
   icc_femicidios,
   "icc_femicidios_2022_2024.csv",
   row.names = FALSE,
   fileEncoding = "UTF-8"
 )
 

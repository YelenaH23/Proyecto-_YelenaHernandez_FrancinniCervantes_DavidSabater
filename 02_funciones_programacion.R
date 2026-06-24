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

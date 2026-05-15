# =========================================================
# Tarea 2 - ACS
# Archivo: 00_config.R
# Objetivo: configurar librerías, rutas y parámetros globales
# =========================================================

options(stringsAsFactors = FALSE)

paquetes <- c(
  "dplyr",
  "readr",
  "stringr",
  "forcats",
  "FactoMineR",
  "factoextra",
  "ggplot2",
  "janitor"
)

instalar_si_falta <- function(pkgs) {
  faltantes <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(faltantes) > 0) {
    install.packages(faltantes, dependencies = TRUE)
  }
}

instalar_si_falta(paquetes)
invisible(lapply(paquetes, library, character.only = TRUE))

ruta_raiz <- "."
ruta_data_raw <- file.path(ruta_raiz, "data", "raw")
ruta_data_processed <- file.path(ruta_raiz, "data", "processed")
ruta_scripts <- file.path(ruta_raiz, "R")
ruta_tablas <- file.path(ruta_raiz, "output", "tablas")
ruta_figuras <- file.path(ruta_raiz, "output", "figuras")
ruta_reportes <- file.path(ruta_raiz, "output", "reportes")
ruta_docs <- file.path(ruta_raiz, "docs")

archivo_fuente <- file.path(
  ruta_data_raw,
  "Resultados_de_Analisis_de_Laboratorio_Suelos_en_Colombia_20260428.csv"
)

var_fila <- "Cultivo"
var_columna <- "Topografia"

top_n_cultivos <- 10
incluir_no_indica <- FALSE
agrupar_topografias_raras <- FALSE
umbral_topografia_rara <- 100

cat("\n=== Configuración ACS cargada ===\n")
cat("Archivo fuente:", archivo_fuente, "\n")
cat("Variable fila:", var_fila, "\n")
cat("Variable columna:", var_columna, "\n")
cat("Top N cultivos:", top_n_cultivos, "\n")
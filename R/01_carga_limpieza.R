# =========================================================
# Tarea 2 - ACS
# Archivo: 01_carga_limpieza.R
# Objetivo: cargar el CSV original y construir una base
# categórica limpia para el ACS
# =========================================================

source("R/00_config.R")

if (!file.exists(archivo_fuente)) {
  stop(
    paste0(
      "No se encontró el archivo fuente en: ", archivo_fuente,
      "\nCopia el CSV original dentro de data/raw/ antes de ejecutar este script."
    )
  )
}

cat("\n=== 01_carga_limpieza.R ===\n")
cat("Archivo fuente:", archivo_fuente, "\n")

vars_categoricas <- c(
  "Secuencial",
  "Fecha de Análisis",
  "Departamento",
  "Municipio",
  "Cultivo",
  "Estado",
  "Tiempo de establecimiento",
  "Topografia",
  "Drenaje",
  "Riego",
  "Fertilizantes aplicados"
)

faltantes_vars <- setdiff(vars_categoricas, names(readr::read_csv(archivo_fuente, n_max = 0, show_col_types = FALSE)))
if (length(faltantes_vars) > 0) {
  stop(paste("Faltan columnas esperadas en el CSV:", paste(faltantes_vars, collapse = ", ")))
}

raw_cat <- readr::read_csv(
  archivo_fuente,
  col_select = dplyr::all_of(vars_categoricas),
  show_col_types = FALSE,
  locale = readr::locale(encoding = "UTF-8")
)

cat("Dimensiones base leída:", dim(raw_cat), "\n")

limpiar_texto <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "N/A", "NULL", "null", "<NA>")] <- NA_character_
  x
}

normalizar_no_indica <- function(x) {
  x <- limpiar_texto(x)
  x_low <- stringr::str_to_lower(x)
  x[x_low %in% c("no indica", "no indíca", "no_indica", "no  indica")] <- "No indica"
  x
}

normalizar_estado <- function(x) {
  x <- normalizar_no_indica(x)
  x_low <- stringr::str_to_lower(x)
  x[x_low == "establecido"] <- "Establecido"
  x[x_low == "por establecer"] <- "Por establecer"
  x
}

normalizar_tiempo <- function(x) {
  x <- normalizar_no_indica(x)
  x_low <- stringr::str_to_lower(x)

  x[x_low %in% c("de 0 a 1 año", "de 0 a 1 ano")] <- "De 0 a 1 año"
  x[x_low %in% c("de 1 a 5 años", "de 1 a 5 anos")] <- "De 1 a 5 años"
  x[x_low %in% c("de 5 a 10 años", "de 5 a 10 anos")] <- "De 5 a 10 años"
  x[x_low %in% c("mas de 10 años", "más de 10 años", "mas de 10 anos", "más de 10 anos")] <- "Más de 10 años"
  x[x_low %in% c("no aplica")] <- "No aplica"
  x
}

normalizar_topografia <- function(x) {
  x <- normalizar_no_indica(x)
  x_low <- stringr::str_to_lower(x)

  x[x_low == "plano"] <- "Plano"
  x[x_low == "ondulado"] <- "Ondulado"
  x[x_low == "pendiente"] <- "Pendiente"
  x[x_low == "pendiente leve"] <- "Pendiente leve"
  x[x_low == "pendiente moderada"] <- "Pendiente moderada"
  x[x_low == "pendiente fuerte"] <- "Pendiente fuerte"
  x[x_low == "ligeramente ondulado"] <- "Ligeramente ondulado"
  x[x_low == "moderadamente ondulado"] <- "Moderadamente ondulado"
  x[x_low == "fuertemente ondulado"] <- "Fuertemente ondulado"
  x[x_low == "ondulado y pendiente"] <- "Ondulado y pendiente"
  x[x_low == "plano y ondulado"] <- "Plano y ondulado"
  x[x_low == "plano y pendiente"] <- "Plano y pendiente"
  x
}

normalizar_cultivo <- function(x) {
  x <- normalizar_no_indica(x)
  x <- limpiar_texto(x)

  x[x %in% c("Pasto", "pasto", "PASTO")] <- "Pastos"
  x[x %in% c("Cafe", "CAFE")] <- "Café"
  x[x %in% c("Platano", "PLATANO")] <- "Plátano"
  x[x %in% c("Maiz", "MAIZ")] <- "Maíz"
  x
}

normalizar_departamento <- function(x) {
  x <- normalizar_no_indica(x)
  x <- stringr::str_to_upper(x)
  x
}

normalizar_municipio <- function(x) {
  x <- normalizar_no_indica(x)
  x
}

normalizar_riego <- function(x) {
  x <- normalizar_no_indica(x)
  x[x == "No Tiene"] <- "No tiene"
  x
}

normalizar_drenaje <- function(x) {
  x <- normalizar_no_indica(x)
  x[x == "Buen drenaje"] <- "Buen drenaje"
  x[x == "Regular drenaje"] <- "Regular drenaje"
  x[x == "Mal drenaje"] <- "Mal drenaje"
  x[x == "Muy buen drenaje"] <- "Muy buen drenaje"
  x[x == "Muy mal drenaje"] <- "Muy mal drenaje"
  x
}

normalizar_fertilizantes <- function(x) {
  x <- normalizar_no_indica(x)
  x[x %in% c("NO", "No", "Ninguno")] <- "No"
  x[x %in% c("SI", "Sí", "Si")] <- "Sí"
  x
}

df_cat <- raw_cat %>%
  dplyr::mutate(
    Secuencial = as.character(Secuencial),
    `Fecha de Análisis` = as.character(`Fecha de Análisis`),
    Departamento = normalizar_departamento(Departamento),
    Municipio = normalizar_municipio(Municipio),
    Cultivo = normalizar_cultivo(Cultivo),
    Estado = normalizar_estado(Estado),
    `Tiempo de establecimiento` = normalizar_tiempo(`Tiempo de establecimiento`),
    Topografia = normalizar_topografia(Topografia),
    Drenaje = normalizar_drenaje(Drenaje),
    Riego = normalizar_riego(Riego),
    `Fertilizantes aplicados` = normalizar_fertilizantes(`Fertilizantes aplicados`)
  )

resumen_categoricas <- data.frame(
  variable = names(df_cat),
  n = nrow(df_cat),
  n_na = sapply(df_cat, function(x) sum(is.na(x))),
  n_categorias = sapply(df_cat, function(x) dplyr::n_distinct(x, na.rm = TRUE)),
  stringsAsFactors = FALSE
)

readr::write_csv(df_cat, file.path(ruta_data_processed, "suelos_categoricas.csv"))
readr::write_csv(resumen_categoricas, file.path(ruta_tablas, "01_resumen_variables_categoricas.csv"))

cat("\nResumen de variables categóricas:\n")
print(resumen_categoricas)

cat("\nTop 15 cultivos después de limpiar:\n")
print(sort(table(df_cat$Cultivo), decreasing = TRUE)[1:15])

cat("\nDistribución de topografía después de limpiar:\n")
print(sort(table(df_cat$Topografia), decreasing = TRUE))

cat("\nArchivo guardado en data/processed/suelos_categoricas.csv\n")
cat("Tabla resumen guardada en output/tablas/01_resumen_variables_categoricas.csv\n")
cat("\n01_carga_limpieza.R ejecutado correctamente.\n")
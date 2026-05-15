# =========================================================
# Tarea 2 - ACS
# Archivo: 03_tabla_contingencia.R
# Objetivo: construir la tabla de contingencia y las tablas
# de perfiles necesarias para el Análisis de Correspondencias
# Simple (ACS)
# =========================================================

source("R/00_config.R")

archivo_base_acs <- file.path(ruta_data_processed, "suelos_acs_base.csv")
if (!file.exists(archivo_base_acs)) {
  stop(
    paste0(
      "No se encontró 'suelos_acs_base.csv' en: ", archivo_base_acs,
      "\nCorre primero R/02_seleccion_variables_acs.R"
    )
  )
}

cat("\n=== 03_tabla_contingencia.R ===\n")
cat("Archivo de entrada:", archivo_base_acs, "\n")

base_acs <- readr::read_csv(
  archivo_base_acs,
  show_col_types = FALSE,
  locale = readr::locale(encoding = "UTF-8")
)

columnas_esperadas <- c("id", "cultivo", "topografia")
faltantes <- setdiff(columnas_esperadas, names(base_acs))
if (length(faltantes) > 0) {
  stop(paste("Faltan columnas esperadas:", paste(faltantes, collapse = ", ")))
}

base_acs <- base_acs %>%
  dplyr::mutate(
    cultivo = stringr::str_squish(as.character(cultivo)),
    topografia = stringr::str_squish(as.character(topografia))
  ) %>%
  dplyr::filter(
    !is.na(cultivo),
    !is.na(topografia),
    cultivo != "",
    topografia != ""
  )

cat("Registros válidos para tabla de contingencia:", nrow(base_acs), "\n")
cat("Número de cultivos:", dplyr::n_distinct(base_acs$cultivo), "\n")
cat("Número de topografías:", dplyr::n_distinct(base_acs$topografia), "\n")

# =========================================================
# 1. Tabla de contingencia absoluta
# =========================================================
matriz_contingencia <- xtabs(~ cultivo + topografia, data = base_acs)

tabla_contingencia <- as.data.frame.matrix(matriz_contingencia)
tabla_contingencia <- tibble::rownames_to_column(tabla_contingencia, var = "cultivo")

# =========================================================
# 2. Totales marginales
# =========================================================
frecuencia_filas <- data.frame(
  cultivo = rownames(matriz_contingencia),
  frecuencia = as.numeric(rowSums(matriz_contingencia)),
  stringsAsFactors = FALSE
) %>%
  dplyr::arrange(dplyr::desc(frecuencia))

frecuencia_columnas <- data.frame(
  topografia = colnames(matriz_contingencia),
  frecuencia = as.numeric(colSums(matriz_contingencia)),
  stringsAsFactors = FALSE
) %>%
  dplyr::arrange(dplyr::desc(frecuencia))

n_total <- sum(matriz_contingencia)

# =========================================================
# 3. Perfiles para ACS
# =========================================================
perfil_filas <- prop.table(matriz_contingencia, margin = 1)
perfil_columnas <- prop.table(matriz_contingencia, margin = 2)
proporciones_globales <- prop.table(matriz_contingencia)

perfil_filas_df <- as.data.frame.matrix(round(perfil_filas, 6))
perfil_filas_df <- tibble::rownames_to_column(perfil_filas_df, var = "cultivo")

perfil_columnas_df <- as.data.frame.matrix(round(perfil_columnas, 6))
perfil_columnas_df <- tibble::rownames_to_column(perfil_columnas_df, var = "cultivo")

proporciones_globales_df <- as.data.frame(as.table(proporciones_globales), stringsAsFactors = FALSE)
names(proporciones_globales_df) <- c("cultivo", "topografia", "proporcion")
proporciones_globales_df <- proporciones_globales_df %>%
  dplyr::mutate(proporcion = round(proporcion, 6)) %>%
  dplyr::arrange(dplyr::desc(proporcion))

# =========================================================
# 4. Frecuencias esperadas para referencia
# =========================================================
chi_ref <- suppressWarnings(chisq.test(matriz_contingencia))
frecuencias_esperadas <- as.data.frame(as.table(chi_ref$expected), stringsAsFactors = FALSE)
names(frecuencias_esperadas) <- c("cultivo", "topografia", "frecuencia_esperada")
frecuencias_esperadas <- frecuencias_esperadas %>%
  dplyr::mutate(frecuencia_esperada = round(frecuencia_esperada, 4)) %>%
  dplyr::arrange(cultivo, topografia)

# =========================================================
# 5. Resumen general
# =========================================================
resumen_tabla <- data.frame(
  indicador = c(
    "Total registros",
    "Número de filas (cultivos)",
    "Número de columnas (topografías)",
    "Celdas de la tabla",
    "Celdas con frecuencia cero",
    "Porcentaje de celdas cero"
  ),
  valor = c(
    n_total,
    nrow(matriz_contingencia),
    ncol(matriz_contingencia),
    length(matriz_contingencia),
    sum(matriz_contingencia == 0),
    round(100 * mean(matriz_contingencia == 0), 2)
  ),
  stringsAsFactors = FALSE
)

# =========================================================
# 6. Guardar salidas
# =========================================================
readr::write_csv(tabla_contingencia, file.path(ruta_tablas, "03_tabla_contingencia_absoluta.csv"))
readr::write_csv(frecuencia_filas, file.path(ruta_tablas, "03_frecuencias_filas.csv"))
readr::write_csv(frecuencia_columnas, file.path(ruta_tablas, "03_frecuencias_columnas.csv"))
readr::write_csv(perfil_filas_df, file.path(ruta_tablas, "03_perfiles_fila.csv"))
readr::write_csv(perfil_columnas_df, file.path(ruta_tablas, "03_perfiles_columna.csv"))
readr::write_csv(proporciones_globales_df, file.path(ruta_tablas, "03_proporciones_globales.csv"))
readr::write_csv(frecuencias_esperadas, file.path(ruta_tablas, "03_frecuencias_esperadas.csv"))
readr::write_csv(resumen_tabla, file.path(ruta_tablas, "03_resumen_tabla_contingencia.csv"))
saveRDS(matriz_contingencia, file.path(ruta_data_processed, "03_matriz_contingencia.rds"))

cat("\nResumen de la tabla de contingencia:\n")
print(resumen_tabla)

cat("\nFrecuencia marginal por cultivo:\n")
print(frecuencia_filas)

cat("\nFrecuencia marginal por topografía:\n")
print(frecuencia_columnas)

cat("\nArchivos guardados:\n")
cat("- output/tablas/03_tabla_contingencia_absoluta.csv\n")
cat("- output/tablas/03_frecuencias_filas.csv\n")
cat("- output/tablas/03_frecuencias_columnas.csv\n")
cat("- output/tablas/03_perfiles_fila.csv\n")
cat("- output/tablas/03_perfiles_columna.csv\n")
cat("- output/tablas/03_proporciones_globales.csv\n")
cat("- output/tablas/03_frecuencias_esperadas.csv\n")
cat("- output/tablas/03_resumen_tabla_contingencia.csv\n")
cat("- data/processed/03_matriz_contingencia.rds\n")
cat("\n03_tabla_contingencia.R ejecutado correctamente.\n")
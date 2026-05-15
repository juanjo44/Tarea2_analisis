# =========================================================
# Tarea 2 - ACS
# Archivo: 02_seleccion_variables_acs.R
# Objetivo: seleccionar y justificar la base analítica para
# el ACS (top N cultivos vs topografía)
# =========================================================

source("R/00_config.R")

archivo_limpio <- file.path(ruta_data_processed, "suelos_categoricas.csv")
if (!file.exists(archivo_limpio)) {
  stop(
    paste0(
      "No se encontró 'suelos_categoricas.csv' en: ", archivo_limpio,
      "\nCorre primero R/01_carga_limpieza.R"
    )
  )
}

cat("\n=== 02_seleccion_variables_acs.R ===\n")
cat("Archivo de entrada:", archivo_limpio, "\n")
cat("Variable fila:", var_fila, "\n")
cat("Variable columna:", var_columna, "\n")
cat("Top N cultivos:", top_n_cultivos, "\n")

acs_base <- readr::read_csv(
  archivo_limpio,
  col_select = dplyr::all_of(c("Secuencial", var_fila, var_columna)),
  show_col_types = FALSE
)

cat("Dimensiones base limpia:", dim(acs_base), "\n")

# =========================================================
# 1. Filtrar registros válidos para las dos variables activas
# =========================================================
acs_base <- acs_base %>%
  dplyr::mutate(
    !!var_fila := stringr::str_squish(.data[[var_fila]]),
    !!var_columna := stringr::str_squish(.data[[var_columna]])
  )

if (!incluir_no_indica) {
  acs_base <- acs_base %>%
    dplyr::filter(
      !is.na(.data[[var_fila]]),
      !is.na(.data[[var_columna]]),
      stringr::str_to_lower(.data[[var_fila]]) != "no indica",
      stringr::str_to_lower(.data[[var_columna]]) != "no indica"
    )
}

cat("Registros después de excluir NA/No indica:", nrow(acs_base), "\n")

# =========================================================
# 2. Seleccionar top N cultivos por frecuencia
# =========================================================
frecuencia_cultivos <- acs_base %>%
  dplyr::count(.data[[var_fila]], sort = TRUE, name = "frecuencia") %>%
  dplyr::rename(cultivo = 1)

top_cultivos <- frecuencia_cultivos %>%
  dplyr::slice_head(n = top_n_cultivos)

cat("\nTop cultivos seleccionados:\n")
print(top_cultivos)

base_top <- acs_base %>%
  dplyr::semi_join(top_cultivos, by = setNames("cultivo", var_fila))

cat("Registros después de filtrar top", top_n_cultivos, "cultivos:", nrow(base_top), "\n")

# =========================================================
# 3. Agrupar topografías raras si así se configura
# =========================================================
frecuencia_topografia_original <- base_top %>%
  dplyr::count(.data[[var_columna]], sort = TRUE, name = "frecuencia") %>%
  dplyr::rename(topografia = 1)

if (agrupar_topografias_raras) {
  topografias_validas <- frecuencia_topografia_original %>%
    dplyr::filter(frecuencia >= umbral_topografia_rara) %>%
    dplyr::pull(topografia)

  base_top <- base_top %>%
    dplyr::mutate(
      !!var_columna := ifelse(
        .data[[var_columna]] %in% topografias_validas,
        .data[[var_columna]],
        "Otras topografías"
      )
    )

  cat("Topografías raras agrupadas con umbral:", umbral_topografia_rara, "\n")
} else {
  cat("Topografías raras no agrupadas.\n")
}

frecuencia_topografia_final <- base_top %>%
  dplyr::count(.data[[var_columna]], sort = TRUE, name = "frecuencia") %>%
  dplyr::rename(topografia = 1)

# =========================================================
# 4. Definir base final para el ACS
# =========================================================
base_acs_final <- base_top %>%
  dplyr::transmute(
    id = Secuencial,
    cultivo = .data[[var_fila]],
    topografia = .data[[var_columna]]
  )

cat("\nDimensiones base final ACS:", dim(base_acs_final), "\n")
cat("Número de cultivos finales:", dplyr::n_distinct(base_acs_final$cultivo), "\n")
cat("Número de topografías finales:", dplyr::n_distinct(base_acs_final$topografia), "\n")

# =========================================================
# 5. Guardar salidas
# =========================================================
readr::write_csv(base_acs_final, file.path(ruta_data_processed, "suelos_acs_base.csv"))
readr::write_csv(top_cultivos, file.path(ruta_tablas, "02_top_cultivos_seleccionados.csv"))
readr::write_csv(frecuencia_topografia_original, file.path(ruta_tablas, "02_topografia_frecuencia_original.csv"))
readr::write_csv(frecuencia_topografia_final, file.path(ruta_tablas, "02_topografia_frecuencia_final.csv"))

decisiones_acs <- data.frame(
  criterio = c(
    "Variable fila",
    "Variable columna",
    "Exclusión de 'No indica'",
    "Top cultivos seleccionados",
    "Agrupación de topografías raras"
  ),
  decision = c(
    var_fila,
    var_columna,
    ifelse(incluir_no_indica, "No", "Sí"),
    paste0("Top ", top_n_cultivos, " por frecuencia"),
    ifelse(
      agrupar_topografias_raras,
      paste0("Sí, umbral = ", umbral_topografia_rara),
      "No"
    )
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(decisiones_acs, file.path(ruta_tablas, "02_decisiones_seleccion_acs.csv"))

cat("\nFrecuencia final de topografía:\n")
print(frecuencia_topografia_final)

cat("\nArchivos guardados:\n")
cat("- data/processed/suelos_acs_base.csv\n")
cat("- output/tablas/02_top_cultivos_seleccionados.csv\n")
cat("- output/tablas/02_topografia_frecuencia_original.csv\n")
cat("- output/tablas/02_topografia_frecuencia_final.csv\n")
cat("- output/tablas/02_decisiones_seleccion_acs.csv\n")
cat("\n02_seleccion_variables_acs.R ejecutado correctamente.\n")
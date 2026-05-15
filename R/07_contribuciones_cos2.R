# =========================================================
# Tarea 2 - ACS
# Archivo: 07_contribuciones_cos2.R
# Objetivo: sintetizar contribuciones y cos2 de cultivos y
# topografías en las principales dimensiones del ACS
# =========================================================

source("R/00_config.R")

archivo_filas <- file.path(ruta_tablas, "05_resultados_filas_acs.csv")
archivo_columnas <- file.path(ruta_tablas, "05_resultados_columnas_acs.csv")

if (!file.exists(archivo_filas) || !file.exists(archivo_columnas)) {
  stop(
    paste0(
      "No se encontraron las salidas del 05:\n",
      "- ", archivo_filas, "\n",
      "- ", archivo_columnas, "\n",
      "Corre primero R/05_modelo_acs.R"
    )
  )
}

cat("\n=== 07_contribuciones_cos2.R ===\n")
cat("Archivo filas:    ", archivo_filas, "\n")
cat("Archivo columnas: ", archivo_columnas, "\n")

resultado_filas <- readr::read_csv(archivo_filas, show_col_types = FALSE)
resultado_columnas <- readr::read_csv(archivo_columnas, show_col_types = FALSE)

# Chequeo mínimo de columnas esperadas
cols_esperadas <- c(
  "categoria",
  "masa",
  "Dim1_coord", "Dim2_coord",
  "Dim1_contrib", "Dim2_contrib",
  "Dim1_cos2", "Dim2_cos2"
)

if (!all(cols_esperadas %in% names(resultado_filas))) {
  warning("El archivo de filas no tiene todas las columnas esperadas; se usará lo disponible.")
}

if (!all(cols_esperadas %in% names(resultado_columnas))) {
  warning("El archivo de columnas no tiene todas las columnas esperadas; se usará lo disponible.")
}

# =========================================================
# 1. Top contribuciones por dimensión (filas y columnas)
# =========================================================
top_n <- 10

filas_top_contrib_dim1 <- resultado_filas %>%
  dplyr::arrange(dplyr::desc(Dim1_contrib)) %>%
  dplyr::slice_head(n = top_n)

filas_top_contrib_dim2 <- resultado_filas %>%
  dplyr::arrange(dplyr::desc(Dim2_contrib)) %>%
  dplyr::slice_head(n = top_n)

columnas_top_contrib_dim1 <- resultado_columnas %>%
  dplyr::arrange(dplyr::desc(Dim1_contrib)) %>%
  dplyr::slice_head(n = top_n)

columnas_top_contrib_dim2 <- resultado_columnas %>%
  dplyr::arrange(dplyr::desc(Dim2_contrib)) %>%
  dplyr::slice_head(n = top_n)

# =========================================================
# 2. Top cos2 (calidad de representación)
# =========================================================
filas_top_cos2_dim1 <- resultado_filas %>%
  dplyr::arrange(dplyr::desc(Dim1_cos2)) %>%
  dplyr::slice_head(n = top_n)

filas_top_cos2_dim2 <- resultado_filas %>%
  dplyr::arrange(dplyr::desc(Dim2_cos2)) %>%
  dplyr::slice_head(n = top_n)

columnas_top_cos2_dim1 <- resultado_columnas %>%
  dplyr::arrange(dplyr::desc(Dim1_cos2)) %>%
  dplyr::slice_head(n = top_n)

columnas_top_cos2_dim2 <- resultado_columnas %>%
  dplyr::arrange(dplyr::desc(Dim2_cos2)) %>%
  dplyr::slice_head(n = top_n)

# =========================================================
# 3. Resúmenes combinados para Dim 1 y 2
#    (útiles para hablar de contribución total y cos2 total)
# =========================================================
resumen_filas_dim12 <- resultado_filas %>%
  dplyr::mutate(
    contrib_total_dim12 = Dim1_contrib + Dim2_contrib,
    cos2_total_dim12 = Dim1_cos2 + Dim2_cos2
  ) %>%
  dplyr::select(
    categoria, masa,
    Dim1_coord, Dim2_coord,
    Dim1_contrib, Dim2_contrib, contrib_total_dim12,
    Dim1_cos2, Dim2_cos2, cos2_total_dim12
  )

resumen_columnas_dim12 <- resultado_columnas %>%
  dplyr::mutate(
    contrib_total_dim12 = Dim1_contrib + Dim2_contrib,
    cos2_total_dim12 = Dim1_cos2 + Dim2_cos2
  ) %>%
  dplyr::select(
    categoria, masa,
    Dim1_coord, Dim2_coord,
    Dim1_contrib, Dim2_contrib, contrib_total_dim12,
    Dim1_cos2, Dim2_cos2, cos2_total_dim12
  )

filas_top_contrib_total <- resumen_filas_dim12 %>%
  dplyr::arrange(dplyr::desc(contrib_total_dim12)) %>%
  dplyr::slice_head(n = top_n)

columnas_top_contrib_total <- resumen_columnas_dim12 %>%
  dplyr::arrange(dplyr::desc(contrib_total_dim12)) %>%
  dplyr::slice_head(n = top_n)

filas_top_cos2_total <- resumen_filas_dim12 %>%
  dplyr::arrange(dplyr::desc(cos2_total_dim12)) %>%
  dplyr::slice_head(n = top_n)

columnas_top_cos2_total <- resumen_columnas_dim12 %>%
  dplyr::arrange(dplyr::desc(cos2_total_dim12)) %>%
  dplyr::slice_head(n = top_n)

# =========================================================
# 4. Guardar salidas
# =========================================================
readr::write_csv(
  filas_top_contrib_dim1,
  file.path(ruta_tablas, "07_filas_top_contrib_dim1_acs.csv")
)
readr::write_csv(
  filas_top_contrib_dim2,
  file.path(ruta_tablas, "07_filas_top_contrib_dim2_acs.csv")
)
readr::write_csv(
  columnas_top_contrib_dim1,
  file.path(ruta_tablas, "07_columnas_top_contrib_dim1_acs.csv")
)
readr::write_csv(
  columnas_top_contrib_dim2,
  file.path(ruta_tablas, "07_columnas_top_contrib_dim2_acs.csv")
)

readr::write_csv(
  filas_top_cos2_dim1,
  file.path(ruta_tablas, "07_filas_top_cos2_dim1_acs.csv")
)
readr::write_csv(
  filas_top_cos2_dim2,
  file.path(ruta_tablas, "07_filas_top_cos2_dim2_acs.csv")
)
readr::write_csv(
  columnas_top_cos2_dim1,
  file.path(ruta_tablas, "07_columnas_top_cos2_dim1_acs.csv")
)
readr::write_csv(
  columnas_top_cos2_dim2,
  file.path(ruta_tablas, "07_columnas_top_cos2_dim2_acs.csv")
)

readr::write_csv(
  resumen_filas_dim12,
  file.path(ruta_tablas, "07_resumen_filas_dim12_acs.csv")
)
readr::write_csv(
  resumen_columnas_dim12,
  file.path(ruta_tablas, "07_resumen_columnas_dim12_acs.csv")
)

readr::write_csv(
  filas_top_contrib_total,
  file.path(ruta_tablas, "07_filas_top_contrib_total_dim12_acs.csv")
)
readr::write_csv(
  columnas_top_contrib_total,
  file.path(ruta_tablas, "07_columnas_top_contrib_total_dim12_acs.csv")
)
readr::write_csv(
  filas_top_cos2_total,
  file.path(ruta_tablas, "07_filas_top_cos2_total_dim12_acs.csv")
)
readr::write_csv(
  columnas_top_cos2_total,
  file.path(ruta_tablas, "07_columnas_top_cos2_total_dim12_acs.csv")
)

# =========================================================
# 5. Resumen en consola
# =========================================================
cat("\nTop cultivos por contribución a Dim 1:\n")
print(filas_top_contrib_dim1)

cat("\nTop topografías por contribución a Dim 1:\n")
print(columnas_top_contrib_dim1)

cat("\nTop cultivos por cos2 total Dim 1 y 2:\n")
print(filas_top_cos2_total)

cat("\nTop topografías por cos2 total Dim 1 y 2:\n")
print(columnas_top_cos2_total)

cat("\nArchivos guardados en output/tablas:\n")
cat("- 07_filas_top_contrib_dim1_acs.csv\n")
cat("- 07_filas_top_contrib_dim2_acs.csv\n")
cat("- 07_columnas_top_contrib_dim1_acs.csv\n")
cat("- 07_columnas_top_contrib_dim2_acs.csv\n")
cat("- 07_filas_top_cos2_dim1_acs.csv\n")
cat("- 07_filas_top_cos2_dim2_acs.csv\n")
cat("- 07_columnas_top_cos2_dim1_acs.csv\n")
cat("- 07_columnas_top_cos2_dim2_acs.csv\n")
cat("- 07_resumen_filas_dim12_acs.csv\n")
cat("- 07_resumen_columnas_dim12_acs.csv\n")
cat("- 07_filas_top_contrib_total_dim12_acs.csv\n")
cat("- 07_columnas_top_contrib_total_dim12_acs.csv\n")
cat("- 07_filas_top_cos2_total_dim12_acs.csv\n")
cat("- 07_columnas_top_cos2_total_dim12_acs.csv\n")
cat("\n07_contribuciones_cos2.R ejecutado correctamente.\n")
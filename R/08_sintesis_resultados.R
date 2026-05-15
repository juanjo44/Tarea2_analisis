# =========================================================
# Tarea 2 - ACS
# Archivo: 08_sintesis_resultados.R
# Objetivo: integrar y sintetizar los principales resultados
# del ACS, la prueba Chi-cuadrado y los indicadores de
# contribuciones/cos2 para dejar tablas finales de apoyo
# al informe
# =========================================================

source("R/00_config.R")

archivo_chi2 <- file.path(ruta_tablas, "04_resumen_prueba_chi2.csv")
archivo_interpretacion_chi2 <- file.path(ruta_tablas, "04_interpretacion_prueba_chi2.csv")
archivo_eigen <- file.path(ruta_tablas, "05_valores_propios_acs.csv")
archivo_filas_dim12 <- file.path(ruta_tablas, "07_resumen_filas_dim12_acs.csv")
archivo_columnas_dim12 <- file.path(ruta_tablas, "07_resumen_columnas_dim12_acs.csv")
archivo_top_filas_contrib <- file.path(ruta_tablas, "07_filas_top_contrib_total_dim12_acs.csv")
archivo_top_columnas_contrib <- file.path(ruta_tablas, "07_columnas_top_contrib_total_dim12_acs.csv")
archivo_top_filas_cos2 <- file.path(ruta_tablas, "07_filas_top_cos2_total_dim12_acs.csv")
archivo_top_columnas_cos2 <- file.path(ruta_tablas, "07_columnas_top_cos2_total_dim12_acs.csv")

archivos_requeridos <- c(
  archivo_chi2,
  archivo_interpretacion_chi2,
  archivo_eigen,
  archivo_filas_dim12,
  archivo_columnas_dim12,
  archivo_top_filas_contrib,
  archivo_top_columnas_contrib,
  archivo_top_filas_cos2,
  archivo_top_columnas_cos2
)

faltantes <- archivos_requeridos[!file.exists(archivos_requeridos)]
if (length(faltantes) > 0) {
  stop(
    paste0(
      "Faltan archivos necesarios para la síntesis:\n",
      paste0("- ", faltantes, collapse = "\n"),
      "\nEjecuta primero 04_prueba_chi2.R, 05_modelo_acs.R y 07_contribuciones_cos2.R"
    )
  )
}

cat("\n=== 08_sintesis_resultados.R ===\n")

resumen_chi2 <- readr::read_csv(archivo_chi2, show_col_types = FALSE)
interpretacion_chi2 <- readr::read_csv(archivo_interpretacion_chi2, show_col_types = FALSE)
valores_propios <- readr::read_csv(archivo_eigen, show_col_types = FALSE)
resumen_filas_dim12 <- readr::read_csv(archivo_filas_dim12, show_col_types = FALSE)
resumen_columnas_dim12 <- readr::read_csv(archivo_columnas_dim12, show_col_types = FALSE)
filas_top_contrib <- readr::read_csv(archivo_top_filas_contrib, show_col_types = FALSE)
columnas_top_contrib <- readr::read_csv(archivo_top_columnas_contrib, show_col_types = FALSE)
filas_top_cos2 <- readr::read_csv(archivo_top_filas_cos2, show_col_types = FALSE)
columnas_top_cos2 <- readr::read_csv(archivo_top_columnas_cos2, show_col_types = FALSE)

# =========================================================
# 1. Resumen ejecutivo cuantitativo
# =========================================================
chi2_valor <- resumen_chi2 %>% dplyr::filter(estadistico == "Chi-cuadrado") %>% dplyr::pull(valor)
gl_valor <- resumen_chi2 %>% dplyr::filter(estadistico == "gl") %>% dplyr::pull(valor)
p_valor <- resumen_chi2 %>% dplyr::filter(estadistico == "pvalor") %>% dplyr::pull(valor)

inercia_dim1 <- valores_propios %>% dplyr::filter(dimension_num == 1) %>% dplyr::pull(porcentaje_inercia)
inercia_dim2 <- valores_propios %>% dplyr::filter(dimension_num == 2) %>% dplyr::pull(porcentaje_inercia)
inercia_acum_2 <- valores_propios %>% dplyr::filter(dimension_num == 2) %>% dplyr::pull(porcentaje_acumulado)

conclusion_chi2 <- interpretacion_chi2 %>%
  dplyr::filter(aspecto == "Conclusión") %>%
  dplyr::pull(detalle)

resumen_ejecutivo <- data.frame(
  indicador = c(
    "Chi-cuadrado",
    "Grados de libertad",
    "p-valor",
    "Conclusión prueba de independencia",
    "Inercia Dimensión 1 (%)",
    "Inercia Dimensión 2 (%)",
    "Inercia acumulada Dimensiones 1 y 2 (%)"
  ),
  valor = c(
    chi2_valor,
    gl_valor,
    p_valor,
    conclusion_chi2,
    round(inercia_dim1, 4),
    round(inercia_dim2, 4),
    round(inercia_acum_2, 4)
  ),
  stringsAsFactors = FALSE
)

# =========================================================
# 2. Perfiles dominantes para interpretación
# =========================================================
filas_clave <- resumen_filas_dim12 %>%
  dplyr::arrange(dplyr::desc(contrib_total_dim12), dplyr::desc(cos2_total_dim12)) %>%
  dplyr::mutate(tipo = "Cultivo") %>%
  dplyr::slice_head(n = 10)

columnas_clave <- resumen_columnas_dim12 %>%
  dplyr::arrange(dplyr::desc(contrib_total_dim12), dplyr::desc(cos2_total_dim12)) %>%
  dplyr::mutate(tipo = "Topografía") %>%
  dplyr::slice_head(n = 10)

perfiles_clave <- dplyr::bind_rows(filas_clave, columnas_clave) %>%
  dplyr::select(
    tipo,
    categoria,
    masa,
    Dim1_coord,
    Dim2_coord,
    contrib_total_dim12,
    cos2_total_dim12
  )

# =========================================================
# 3. Tablas sintéticas para narrativa
# =========================================================
plantilla_filas <- filas_top_contrib %>%
  dplyr::left_join(
    filas_top_cos2 %>% dplyr::select(categoria, cos2_total_dim12),
    by = "categoria",
    suffix = c("", "_cos2")
  ) %>%
  dplyr::select(
    categoria,
    masa,
    Dim1_coord,
    Dim2_coord,
    contrib_total_dim12,
    cos2_total_dim12
  ) %>%
  dplyr::mutate(interpretacion = dplyr::case_when(
    Dim1_coord > 0 & Dim2_coord > 0 ~ "Cuadrante I",
    Dim1_coord < 0 & Dim2_coord > 0 ~ "Cuadrante II",
    Dim1_coord < 0 & Dim2_coord < 0 ~ "Cuadrante III",
    Dim1_coord > 0 & Dim2_coord < 0 ~ "Cuadrante IV",
    TRUE ~ "Sobre un eje"
  ))

plantilla_columnas <- columnas_top_contrib %>%
  dplyr::left_join(
    columnas_top_cos2 %>% dplyr::select(categoria, cos2_total_dim12),
    by = "categoria",
    suffix = c("", "_cos2")
  ) %>%
  dplyr::select(
    categoria,
    masa,
    Dim1_coord,
    Dim2_coord,
    contrib_total_dim12,
    cos2_total_dim12
  ) %>%
  dplyr::mutate(interpretacion = dplyr::case_when(
    Dim1_coord > 0 & Dim2_coord > 0 ~ "Cuadrante I",
    Dim1_coord < 0 & Dim2_coord > 0 ~ "Cuadrante II",
    Dim1_coord < 0 & Dim2_coord < 0 ~ "Cuadrante III",
    Dim1_coord > 0 & Dim2_coord < 0 ~ "Cuadrante IV",
    TRUE ~ "Sobre un eje"
  ))

# =========================================================
# 4. Texto de apoyo al informe
# =========================================================
texto_sintesis <- data.frame(
  seccion = c(
    "Prueba de independencia",
    "Calidad global del plano factorial",
    "Cultivos más relevantes",
    "Topografías más relevantes"
  ),
  sintesis = c(
    paste0(
      "La prueba Chi-cuadrado reportó un estadístico de ", chi2_valor,
      " con ", gl_valor,
      " grados de libertad y p-valor ", p_valor,
      ", por lo que ", tolower(conclusion_chi2), "."
    ),
    paste0(
      "Las dos primeras dimensiones del ACS explican en conjunto ",
      round(inercia_acum_2, 2),
      "% de la inercia total; la Dimensión 1 explica ",
      round(inercia_dim1, 2),
      "% y la Dimensión 2 explica ",
      round(inercia_dim2, 2), "% ."
    ),
    paste0(
      "Los cultivos con mayor aporte combinado a las dimensiones 1 y 2 son: ",
      paste(head(filas_top_contrib$categoria, 5), collapse = ", "),
      "."
    ),
    paste0(
      "Las topografías con mayor aporte combinado a las dimensiones 1 y 2 son: ",
      paste(head(columnas_top_contrib$categoria, 5), collapse = ", "),
      "."
    )
  ),
  stringsAsFactors = FALSE
)

# =========================================================
# 5. Guardar salidas
# =========================================================
readr::write_csv(resumen_ejecutivo, file.path(ruta_tablas, "08_resumen_ejecutivo_acs.csv"))
readr::write_csv(perfiles_clave, file.path(ruta_tablas, "08_perfiles_clave_acs.csv"))
readr::write_csv(plantilla_filas, file.path(ruta_tablas, "08_plantilla_interpretacion_filas_acs.csv"))
readr::write_csv(plantilla_columnas, file.path(ruta_tablas, "08_plantilla_interpretacion_columnas_acs.csv"))
readr::write_csv(texto_sintesis, file.path(ruta_tablas, "08_texto_sintesis_resultados_acs.csv"))

cat("\nResumen ejecutivo ACS:\n")
print(resumen_ejecutivo)

cat("\nPrimeros perfiles clave:\n")
print(utils::head(perfiles_clave, 10))

cat("\nArchivos guardados:\n")
cat("- output/tablas/08_resumen_ejecutivo_acs.csv\n")
cat("- output/tablas/08_perfiles_clave_acs.csv\n")
cat("- output/tablas/08_plantilla_interpretacion_filas_acs.csv\n")
cat("- output/tablas/08_plantilla_interpretacion_columnas_acs.csv\n")
cat("- output/tablas/08_texto_sintesis_resultados_acs.csv\n")
cat("\n08_sintesis_resultados.R ejecutado correctamente.\n")

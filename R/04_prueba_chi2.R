# =========================================================
# Tarea 2 - ACS
# Archivo: 04_prueba_chi2.R
# Objetivo: realizar la prueba de independencia Chi-cuadrado
# de Pearson sobre la tabla de contingencia del ACS y dejar
# insumos para la interpretación previa al análisis
# =========================================================

source("R/00_config.R")

archivo_matriz <- file.path(ruta_data_processed, "03_matriz_contingencia.rds")
archivo_base <- file.path(ruta_data_processed, "suelos_acs_base.csv")

if (!file.exists(archivo_matriz)) {
  stop(
    paste0(
      "No se encontró '03_matriz_contingencia.rds' en: ", archivo_matriz,
      "\nCorre primero R/03_tabla_contingencia.R"
    )
  )
}

cat("\n=== 04_prueba_chi2.R ===\n")
cat("Archivo matriz:", archivo_matriz, "\n")

matriz_contingencia <- readRDS(archivo_matriz)

if (!is.matrix(matriz_contingencia) && !is.table(matriz_contingencia)) {
  stop("El objeto cargado no es una matriz o tabla de contingencia válida.")
}

matriz_contingencia <- as.table(matriz_contingencia)

cat("Dimensiones de la tabla:", dim(matriz_contingencia), "\n")
cat("Total de observaciones:", sum(matriz_contingencia), "\n")

# =========================================================
# 1. Prueba Chi-cuadrado de independencia
# =========================================================
prueba_chi2 <- suppressWarnings(chisq.test(matriz_contingencia, correct = FALSE))

estadistico_chi2 <- unname(prueba_chi2$statistic)
gl <- unname(prueba_chi2$parameter)
p_valor <- unname(prueba_chi2$p.value)

cat("\nResultado prueba Chi-cuadrado:\n")
cat("Chi2 =", round(estadistico_chi2, 4), "\n")
cat("gl   =", gl, "\n")
cat("p    =", format(p_valor, scientific = TRUE), "\n")

# =========================================================
# 2. Frecuencias observadas y esperadas
# =========================================================
observadas_df <- as.data.frame(as.table(matriz_contingencia), stringsAsFactors = FALSE)
names(observadas_df) <- c("cultivo", "topografia", "frecuencia_observada")

esperadas_df <- as.data.frame(as.table(prueba_chi2$expected), stringsAsFactors = FALSE)
names(esperadas_df) <- c("cultivo", "topografia", "frecuencia_esperada")

residuales_df <- as.data.frame(as.table(prueba_chi2$residuals), stringsAsFactors = FALSE)
names(residuales_df) <- c("cultivo", "topografia", "residual_pearson")

stdres_df <- as.data.frame(as.table(prueba_chi2$stdres), stringsAsFactors = FALSE)
names(stdres_df) <- c("cultivo", "topografia", "residual_estandarizado")

celdas_chi2 <- observadas_df %>%
  dplyr::left_join(esperadas_df, by = c("cultivo", "topografia")) %>%
  dplyr::left_join(residuales_df, by = c("cultivo", "topografia")) %>%
  dplyr::left_join(stdres_df, by = c("cultivo", "topografia")) %>%
  dplyr::mutate(
    contribucion_chi2 = ((frecuencia_observada - frecuencia_esperada)^2) / frecuencia_esperada,
    contribucion_pct = 100 * contribucion_chi2 / sum(contribucion_chi2),
    asociacion = dplyr::case_when(
      residual_estandarizado >= 2 ~ "Sobre-representación",
      residual_estandarizado <= -2 ~ "Sub-representación",
      TRUE ~ "Sin evidencia fuerte"
    )
  ) %>%
  dplyr::arrange(dplyr::desc(abs(residual_estandarizado)))

# =========================================================
# 3. Verificaciones de supuestos de la prueba
# =========================================================
resumen_supuestos <- data.frame(
  indicador = c(
    "Total de celdas",
    "Frecuencia esperada mínima",
    "Celdas con esperada < 5",
    "Porcentaje celdas con esperada < 5",
    "Celdas con esperada < 1"
  ),
  valor = c(
    length(prueba_chi2$expected),
    round(min(prueba_chi2$expected), 4),
    sum(prueba_chi2$expected < 5),
    round(100 * mean(prueba_chi2$expected < 5), 2),
    sum(prueba_chi2$expected < 1)
  ),
  stringsAsFactors = FALSE
)

# =========================================================
# 4. Resumen interpretativo para el informe
# =========================================================
interpretacion_chi2 <- data.frame(
  aspecto = c(
    "Hipótesis nula",
    "Hipótesis alternativa",
    "Regla de decisión",
    "Decisión",
    "Conclusión"
  ),
  detalle = c(
    "Cultivo y topografía son independientes",
    "Cultivo y topografía son dependientes",
    "Rechazar H0 si p-valor < 0.05",
    ifelse(p_valor < 0.05, "Se rechaza H0", "No se rechaza H0"),
    ifelse(
      p_valor < 0.05,
      "Existe evidencia estadísticamente significativa de asociación entre cultivo y topografía",
      "No existe evidencia estadísticamente significativa de asociación entre cultivo y topografía"
    )
  ),
  stringsAsFactors = FALSE
)

resumen_prueba <- data.frame(
  estadistico = c("Chi-cuadrado", "gl", "p_valor"),
  valor = c(round(estadistico_chi2, 6), gl, signif(p_valor, 6)),
  stringsAsFactors = FALSE
)

principales_celdas <- celdas_chi2 %>%
  dplyr::select(cultivo, topografia, frecuencia_observada, frecuencia_esperada,
                residual_pearson, residual_estandarizado,
                contribucion_chi2, contribucion_pct, asociacion) %>%
  dplyr::slice_head(n = 20)

# =========================================================
# 5. Guardar salidas
# =========================================================
readr::write_csv(resumen_prueba, file.path(ruta_tablas, "04_resumen_prueba_chi2.csv"))
readr::write_csv(interpretacion_chi2, file.path(ruta_tablas, "04_interpretacion_prueba_chi2.csv"))
readr::write_csv(resumen_supuestos, file.path(ruta_tablas, "04_supuestos_prueba_chi2.csv"))
readr::write_csv(celdas_chi2, file.path(ruta_tablas, "04_celdas_prueba_chi2.csv"))
readr::write_csv(principales_celdas, file.path(ruta_tablas, "04_top_celdas_residuales_chi2.csv"))
saveRDS(prueba_chi2, file.path(ruta_data_processed, "04_prueba_chi2.rds"))

cat("\nResumen de supuestos:\n")
print(resumen_supuestos)

cat("\nTop 10 celdas por |residual estandarizado|:\n")
print(principales_celdas[1:min(10, nrow(principales_celdas)), ])

cat("\nArchivos guardados:\n")
cat("- output/tablas/04_resumen_prueba_chi2.csv\n")
cat("- output/tablas/04_interpretacion_prueba_chi2.csv\n")
cat("- output/tablas/04_supuestos_prueba_chi2.csv\n")
cat("- output/tablas/04_celdas_prueba_chi2.csv\n")
cat("- output/tablas/04_top_celdas_residuales_chi2.csv\n")
cat("- data/processed/04_prueba_chi2.rds\n")
cat("\n04_prueba_chi2.R ejecutado correctamente.\n")

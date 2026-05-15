# =========================================================
# Tarea 2 - ACS
# Archivo: 05_modelo_acs.R
# Objetivo: ajustar el modelo de Análisis de Correspondencias
# Simple (ACS), extraer indicadores principales y guardar
# tablas y figuras para la interpretación
# =========================================================

source("R/00_config.R")

archivo_matriz <- file.path(ruta_data_processed, "03_matriz_contingencia.rds")
archivo_chi2 <- file.path(ruta_data_processed, "04_prueba_chi2.rds")

if (!file.exists(archivo_matriz)) {
  stop(
    paste0(
      "No se encontró '03_matriz_contingencia.rds' en: ", archivo_matriz,
      "\nCorre primero R/03_tabla_contingencia.R"
    )
  )
}

cat("\n=== 05_modelo_acs.R ===\n")
cat("Archivo matriz:", archivo_matriz, "\n")

matriz_contingencia <- readRDS(archivo_matriz)
matriz_contingencia <- as.matrix(matriz_contingencia)

if (nrow(matriz_contingencia) < 2 || ncol(matriz_contingencia) < 2) {
  stop("La tabla de contingencia debe tener al menos 2 filas y 2 columnas.")
}

cat("Dimensiones de la tabla:", dim(matriz_contingencia), "\n")
cat("Total de observaciones:", sum(matriz_contingencia), "\n")
cat(
  "Máximo número de dimensiones posibles:",
  min(nrow(matriz_contingencia) - 1, ncol(matriz_contingencia) - 1),
  "\n"
)

if (file.exists(archivo_chi2)) {
  prueba_chi2 <- readRDS(archivo_chi2)
  cat("\nPrueba Chi-cuadrado previa:\n")
  cat("Chi2 =", round(unname(prueba_chi2$statistic), 4), "\n")
  cat("gl   =", unname(prueba_chi2$parameter), "\n")
  cat("p    =", format(unname(prueba_chi2$p.value), scientific = TRUE), "\n")
}

# =========================================================
# 1. Ajustar modelo ACS con FactoMineR
# =========================================================
res_acs <- FactoMineR::CA(matriz_contingencia, graph = FALSE)

# =========================================================
# 2. Inercia y valores propios
# =========================================================
valores_propios <- as.data.frame(res_acs$eig)
valores_propios <- tibble::rownames_to_column(valores_propios, var = "dimension")

names(valores_propios) <- c(
  "dimension",
  "valor_propio",
  "porcentaje_inercia",
  "porcentaje_acumulado"
)

valores_propios <- valores_propios %>%
  dplyr::mutate(dimension_num = dplyr::row_number()) %>%
  dplyr::select(
    dimension,
    dimension_num,
    valor_propio,
    porcentaje_inercia,
    porcentaje_acumulado
  )

# =========================================================
# 3. Resultados de filas y columnas
# =========================================================
filas_coord <- as.data.frame(res_acs$row$coord)
filas_contrib <- as.data.frame(res_acs$row$contrib)
filas_cos2 <- as.data.frame(res_acs$row$cos2)

columnas_coord <- as.data.frame(res_acs$col$coord)
columnas_contrib <- as.data.frame(res_acs$col$contrib)
columnas_cos2 <- as.data.frame(res_acs$col$cos2)

n_total <- sum(matriz_contingencia)
masa_filas <- rowSums(matriz_contingencia) / n_total
masa_columnas <- colSums(matriz_contingencia) / n_total

n_dim_filas <- ncol(filas_coord)
n_dim_columnas <- ncol(columnas_coord)

names(filas_coord) <- paste0("Dim", seq_len(n_dim_filas), "_coord")
names(filas_contrib) <- paste0("Dim", seq_len(n_dim_filas), "_contrib")
names(filas_cos2) <- paste0("Dim", seq_len(n_dim_filas), "_cos2")

names(columnas_coord) <- paste0("Dim", seq_len(n_dim_columnas), "_coord")
names(columnas_contrib) <- paste0("Dim", seq_len(n_dim_columnas), "_contrib")
names(columnas_cos2) <- paste0("Dim", seq_len(n_dim_columnas), "_cos2")

resultado_filas <- data.frame(
  categoria = rownames(filas_coord),
  masa = as.numeric(masa_filas[rownames(filas_coord)]),
  stringsAsFactors = FALSE
) %>%
  dplyr::bind_cols(filas_coord, filas_contrib, filas_cos2)

resultado_columnas <- data.frame(
  categoria = rownames(columnas_coord),
  masa = as.numeric(masa_columnas[rownames(columnas_coord)]),
  stringsAsFactors = FALSE
) %>%
  dplyr::bind_cols(columnas_coord, columnas_contrib, columnas_cos2)

# =========================================================
# 4. Resumen para dimensiones 1 y 2
# =========================================================
resumen_dim12_filas <- resultado_filas %>%
  dplyr::mutate(
    coord_dim1 = Dim1_coord,
    coord_dim2 = if ("Dim2_coord" %in% names(.)) Dim2_coord else NA_real_,
    contrib_dim1 = Dim1_contrib,
    contrib_dim2 = if ("Dim2_contrib" %in% names(.)) Dim2_contrib else NA_real_,
    cos2_dim1 = Dim1_cos2,
    cos2_dim2 = if ("Dim2_cos2" %in% names(.)) Dim2_cos2 else NA_real_,
    cos2_dim1_dim2 = dplyr::coalesce(cos2_dim1, 0) + dplyr::coalesce(cos2_dim2, 0)
  ) %>%
  dplyr::select(
    categoria, masa,
    coord_dim1, coord_dim2,
    contrib_dim1, contrib_dim2,
    cos2_dim1, cos2_dim2,
    cos2_dim1_dim2
  ) %>%
  dplyr::arrange(dplyr::desc(contrib_dim1 + dplyr::coalesce(contrib_dim2, 0)))

resumen_dim12_columnas <- resultado_columnas %>%
  dplyr::mutate(
    coord_dim1 = Dim1_coord,
    coord_dim2 = if ("Dim2_coord" %in% names(.)) Dim2_coord else NA_real_,
    contrib_dim1 = Dim1_contrib,
    contrib_dim2 = if ("Dim2_contrib" %in% names(.)) Dim2_contrib else NA_real_,
    cos2_dim1 = Dim1_cos2,
    cos2_dim2 = if ("Dim2_cos2" %in% names(.)) Dim2_cos2 else NA_real_,
    cos2_dim1_dim2 = dplyr::coalesce(cos2_dim1, 0) + dplyr::coalesce(cos2_dim2, 0)
  ) %>%
  dplyr::select(
    categoria, masa,
    coord_dim1, coord_dim2,
    contrib_dim1, contrib_dim2,
    cos2_dim1, cos2_dim2,
    cos2_dim1_dim2
  ) %>%
  dplyr::arrange(dplyr::desc(contrib_dim1 + dplyr::coalesce(contrib_dim2, 0)))

# =========================================================
# 5. Elementos más influyentes
# =========================================================
filas_top_dim1 <- resumen_dim12_filas %>%
  dplyr::arrange(dplyr::desc(contrib_dim1)) %>%
  dplyr::slice_head(n = 10)

filas_top_dim2 <- resumen_dim12_filas %>%
  dplyr::arrange(dplyr::desc(contrib_dim2)) %>%
  dplyr::slice_head(n = 10)

columnas_top_dim1 <- resumen_dim12_columnas %>%
  dplyr::arrange(dplyr::desc(contrib_dim1)) %>%
  dplyr::slice_head(n = 10)

columnas_top_dim2 <- resumen_dim12_columnas %>%
  dplyr::arrange(dplyr::desc(contrib_dim2)) %>%
  dplyr::slice_head(n = 10)

# =========================================================
# 6. Figuras base del ACS
# =========================================================
fig_scree <- factoextra::fviz_screeplot(res_acs, addlabels = TRUE, ylim = c(0, 100)) +
  ggplot2::labs(
    title = "Inercia explicada por dimensión - ACS",
    x = "Dimensión",
    y = "Porcentaje de inercia"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "05_screeplot_acs.png"),
  plot = fig_scree,
  width = 9,
  height = 6,
  dpi = 300
)

fig_filas <- factoextra::fviz_ca_row(
  res_acs,
  repel = TRUE,
  col.row = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  ggplot2::labs(title = "Mapa factorial de filas (cultivos)") +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "05_mapa_filas_acs.png"),
  plot = fig_filas,
  width = 10,
  height = 7,
  dpi = 300
)

fig_columnas <- factoextra::fviz_ca_col(
  res_acs,
  repel = TRUE,
  col.col = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  ggplot2::labs(title = "Mapa factorial de columnas (topografías)") +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "05_mapa_columnas_acs.png"),
  plot = fig_columnas,
  width = 10,
  height = 7,
  dpi = 300
)

fig_biplot <- factoextra::fviz_ca_biplot(
  res_acs,
  repel = TRUE
) +
  ggplot2::labs(title = "Biplot del Análisis de Correspondencias Simple") +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "05_biplot_acs.png"),
  plot = fig_biplot,
  width = 11,
  height = 8,
  dpi = 300
)

# =========================================================
# 7. Guardar salidas
# =========================================================
readr::write_csv(valores_propios, file.path(ruta_tablas, "05_valores_propios_acs.csv"))
readr::write_csv(resultado_filas, file.path(ruta_tablas, "05_resultados_filas_acs.csv"))
readr::write_csv(resultado_columnas, file.path(ruta_tablas, "05_resultados_columnas_acs.csv"))
readr::write_csv(resumen_dim12_filas, file.path(ruta_tablas, "05_resumen_dim12_filas_acs.csv"))
readr::write_csv(resumen_dim12_columnas, file.path(ruta_tablas, "05_resumen_dim12_columnas_acs.csv"))
readr::write_csv(filas_top_dim1, file.path(ruta_tablas, "05_top_filas_dim1_acs.csv"))
readr::write_csv(filas_top_dim2, file.path(ruta_tablas, "05_top_filas_dim2_acs.csv"))
readr::write_csv(columnas_top_dim1, file.path(ruta_tablas, "05_top_columnas_dim1_acs.csv"))
readr::write_csv(columnas_top_dim2, file.path(ruta_tablas, "05_top_columnas_dim2_acs.csv"))
saveRDS(res_acs, file.path(ruta_data_processed, "05_modelo_acs.rds"))

cat("\nValores propios e inercia:\n")
print(valores_propios)

cat("\nTop filas por contribución a Dim 1:\n")
print(filas_top_dim1)

cat("\nTop columnas por contribución a Dim 1:\n")
print(columnas_top_dim1)

cat("\nArchivos guardados:\n")
cat("- output/tablas/05_valores_propios_acs.csv\n")
cat("- output/tablas/05_resultados_filas_acs.csv\n")
cat("- output/tablas/05_resultados_columnas_acs.csv\n")
cat("- output/tablas/05_resumen_dim12_filas_acs.csv\n")
cat("- output/tablas/05_resumen_dim12_columnas_acs.csv\n")
cat("- output/tablas/05_top_filas_dim1_acs.csv\n")
cat("- output/tablas/05_top_filas_dim2_acs.csv\n")
cat("- output/tablas/05_top_columnas_dim1_acs.csv\n")
cat("- output/tablas/05_top_columnas_dim2_acs.csv\n")
cat("- data/processed/05_modelo_acs.rds\n")
cat("- output/figuras/05_screeplot_acs.png\n")
cat("- output/figuras/05_mapa_filas_acs.png\n")
cat("- output/figuras/05_mapa_columnas_acs.png\n")
cat("- output/figuras/05_biplot_acs.png\n")
cat("\n05_modelo_acs.R ejecutado correctamente.\n")
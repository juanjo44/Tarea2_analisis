# =========================================================
# Tarea 2 - ACS
# Archivo: 06_graficos_acs.R
# Objetivo: generar gráficos específicos del ACS
# (inercia, mapas factoriales, contribuciones y cos2)
# pensados para el informe de la tarea
# =========================================================

source("R/00_config.R")

archivo_modelo <- file.path(ruta_data_processed, "05_modelo_acs.rds")
archivo_resumen_filas <- file.path(ruta_tablas, "05_resumen_dim12_filas_acs.csv")
archivo_resumen_columnas <- file.path(ruta_tablas, "05_resumen_dim12_columnas_acs.csv")

if (!file.exists(archivo_modelo)) {
  stop(
    paste0(
      "No se encontró '05_modelo_acs.rds' en: ", archivo_modelo,
      "\nCorre primero R/05_modelo_acs.R"
    )
  )
}

cat("\n=== 06_graficos_acs.R ===\n")
cat("Archivo modelo ACS:", archivo_modelo, "\n")

res_acs <- readRDS(archivo_modelo)

# (Opcional) leer resúmenes para etiquetar manualmente
if (file.exists(archivo_resumen_filas)) {
  resumen_dim12_filas <- readr::read_csv(archivo_resumen_filas, show_col_types = FALSE)
} else {
  resumen_dim12_filas <- NULL
}

if (file.exists(archivo_resumen_columnas)) {
  resumen_dim12_columnas <- readr::read_csv(archivo_resumen_columnas, show_col_types = FALSE)
} else {
  resumen_dim12_columnas <- NULL
}

# =========================================================
# 1. Scree plot (ya está en 05, pero lo dejamos aquí por si
#    quieres regenerarlo con otro tema o tamaño)
# =========================================================
fig_scree <- factoextra::fviz_screeplot(
  res_acs,
  addlabels = TRUE,
  ylim = c(0, 100)
) +
  ggplot2::labs(
    title = "Inercia explicada por dimensión - ACS",
    x = "Dimensión",
    y = "Porcentaje de inercia"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "06_screeplot_acs.png"),
  plot = fig_scree,
  width = 9,
  height = 6,
  dpi = 300
)

# =========================================================
# 2. Mapas factoriales de filas y columnas (Dim 1 vs Dim 2)
#    coloreados por contribución
# =========================================================
fig_filas <- factoextra::fviz_ca_row(
  res_acs,
  axes = c(1, 2),
  repel = TRUE,
  col.row = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  ggplot2::labs(
    title = "Mapa factorial de filas (cultivos)",
    x = "Dimensión 1",
    y = "Dimensión 2"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "06_mapa_filas_acs.png"),
  plot = fig_filas,
  width = 10,
  height = 7,
  dpi = 300
)

fig_columnas <- factoextra::fviz_ca_col(
  res_acs,
  axes = c(1, 2),
  repel = TRUE,
  col.col = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  ggplot2::labs(
    title = "Mapa factorial de columnas (topografías)",
    x = "Dimensión 1",
    y = "Dimensión 2"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "06_mapa_columnas_acs.png"),
  plot = fig_columnas,
  width = 10,
  height = 7,
  dpi = 300
)

fig_biplot <- factoextra::fviz_ca_biplot(
  res_acs,
  axes = c(1, 2),
  repel = TRUE
) +
  ggplot2::labs(
    title = "Biplot ACS (cultivos y topografías)",
    x = "Dimensión 1",
    y = "Dimensión 2"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "06_biplot_acs.png"),
  plot = fig_biplot,
  width = 11,
  height = 8,
  dpi = 300
)

# =========================================================
# 3. Gráficos de contribución por dimensión
#    (top 10 cultivos / topografías)
# =========================================================
fig_contrib_filas_dim1 <- factoextra::fviz_contrib(
  res_acs,
  choice = "row",
  axes = 1,
  top = 10
) +
  ggplot2::labs(
    title = "Contribución de cultivos a la Dimensión 1"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "06_contrib_filas_dim1_acs.png"),
  plot = fig_contrib_filas_dim1,
  width = 9,
  height = 6,
  dpi = 300
)

fig_contrib_filas_dim2 <- factoextra::fviz_contrib(
  res_acs,
  choice = "row",
  axes = 2,
  top = 10
) +
  ggplot2::labs(
    title = "Contribución de cultivos a la Dimensión 2"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "06_contrib_filas_dim2_acs.png"),
  plot = fig_contrib_filas_dim2,
  width = 9,
  height = 6,
  dpi = 300
)

fig_contrib_columnas_dim1 <- factoextra::fviz_contrib(
  res_acs,
  choice = "col",
  axes = 1,
  top = 10
) +
  ggplot2::labs(
    title = "Contribución de topografías a la Dimensión 1"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "06_contrib_columnas_dim1_acs.png"),
  plot = fig_contrib_columnas_dim1,
  width = 9,
  height = 6,
  dpi = 300
)

fig_contrib_columnas_dim2 <- factoextra::fviz_contrib(
  res_acs,
  choice = "col",
  axes = 2,
  top = 10
) +
  ggplot2::labs(
    title = "Contribución de topografías a la Dimensión 2"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "06_contrib_columnas_dim2_acs.png"),
  plot = fig_contrib_columnas_dim2,
  width = 9,
  height = 6,
  dpi = 300
)

# =========================================================
# 4. Gráficos de cos2 (calidad de representación)
# =========================================================
fig_cos2_filas_dim12 <- factoextra::fviz_cos2(
  res_acs,
  choice = "row",
  axes = c(1, 2)
) +
  ggplot2::labs(
    title = "Calidad de representación (cos2) de los cultivos \nDimensiones 1 y 2"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "06_cos2_filas_dim12_acs.png"),
  plot = fig_cos2_filas_dim12,
  width = 9,
  height = 6,
  dpi = 300
)

fig_cos2_columnas_dim12 <- factoextra::fviz_cos2(
  res_acs,
  choice = "col",
  axes = c(1, 2)
) +
  ggplot2::labs(
    title = "Calidad de representación (cos2) de las topografías \nDimensiones 1 y 2"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(ruta_figuras, "06_cos2_columnas_dim12_acs.png"),
  plot = fig_cos2_columnas_dim12,
  width = 9,
  height = 6,
  dpi = 300
)

# =========================================================
# 5. Resumen en consola
# =========================================================
cat("\nGráficos generados en:\n")
cat("- output/figuras/06_screeplot_acs.png\n")
cat("- output/figuras/06_mapa_filas_acs.png\n")
cat("- output/figuras/06_mapa_columnas_acs.png\n")
cat("- output/figuras/06_biplot_acs.png\n")
cat("- output/figuras/06_contrib_filas_dim1_acs.png\n")
cat("- output/figuras/06_contrib_filas_dim2_acs.png\n")
cat("- output/figuras/06_contrib_columnas_dim1_acs.png\n")
cat("- output/figuras/06_contrib_columnas_dim2_acs.png\n")
cat("- output/figuras/06_cos2_filas_dim12_acs.png\n")
cat("- output/figuras/06_cos2_columnas_dim12_acs.png\n")
cat("\n06_graficos_acs.R ejecutado correctamente.\n")
# Función recursiva para seleccionar una variable numérica y graficar un histograma
plot_histogram_recursive <- function(data) {
  if (!is.data.frame(data)) {
    stop("El argumento 'data' debe ser un data frame.")
  }
  cat("Columnas disponibles:\n")
  num_cols <- which(sapply(data, is.numeric))
  for (i in seq_along(num_cols)) {
    cat(i, ": ", names(data)[num_cols[i]], "\n")
  }
  cat("\nSeleccione el número de la columna que desea graficar como histograma: ")
  choice <- as.integer(readline(prompt = ""))
  if (choice < 1 || choice > length(num_cols)) {
    cat("Selección inválida. Intente de nuevo.\n")
    plot_histogram_recursive(data)
  } else {
    col_name <- names(data)[num_cols[choice]]
    # Graficar el histograma
    hist(data[[col_name]], main = paste("Histograma de", col_name), xlab = col_name, col = "lightgreen")
  }
}

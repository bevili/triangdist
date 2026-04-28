#' @title Distribución Triangular
#' @description Funciones para la distribución triangular con parámetros min, max y mode.
#' @param x,q Vector de cuantiles.
#' @param p Vector de probabilidades.
#' @param n Número de observaciones.
#' @param min Límite inferior (a).
#' @param max Límite superior (b).
#' @param mode Moda (c).
#' @return Vector de valores correspondientes.
#' @export
dtriang <- function(x, min, max, mode) {
  # Validación de errores [cite: 50]
  if (any(min > max) || any(mode < min) || any(mode > max)) {
    stop("Parámetros inválidos: min <= mode <= max es requerido.")
  }

  # Lógica de densidad [cite: 9, 10, 12]
  res <- numeric(length(x))
  idx1 <- x >= min & x < mode
  idx2 <- x >= mode & x <= max

  res[idx1] <- 2 * (x[idx1] - min) / ((max - min) * (mode - min))
  res[idx2] <- 2 * (max - x[idx2]) / ((max - min) * (max - mode))
  return(res)
}

#' @rdname dtriang
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min > max) || any(mode < min) || any(mode > max)) {
    stop("Parámetros inválidos.")
  }

  res <- numeric(length(q))
  res[q < min] <- 0
  res[q > max] <- 1

  idx1 <- q >= min & q < mode
  idx2 <- q >= mode & q <= max

  res[idx1] <- (q[idx1] - min)^2 / ((max - min) * (mode - min))
  res[idx2] <- 1 - (max - q[idx2])^2 / ((max - min) * (max - mode))
  return(res)
}

#' @rdname dtriang
#' @export
qtriang <- function(p, min, max, mode) {
  if (any(min > max) || any(mode < min) || any(mode > max) || any(p < 0) || any(p > 1)) {
    stop("Parámetros o probabilidades (p entre 0 y 1) inválidos.") # [cite: 50]
  }

  p_mode <- (mode - min) / (max - min)
  res <- numeric(length(p))

  idx1 <- p < p_mode
  idx2 <- p >= p_mode

  res[idx1] <- min + sqrt(p[idx1] * (max - min) * (mode - min))
  res[idx2] <- max - sqrt((1 - p[idx2]) * (max - min) * (max - mode))
  return(res)
}

#' @rdname dtriang
#' @export
rtriang <- function(n, min, max, mode) {
  # Método de Inversión [cite: 42, 44, 46]
  p <- runif(n)
  return(qtriang(p, min, max, mode))
}

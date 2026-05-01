#' @title Triangular Distribution
#' @description Functions for the triangular distribution.
#' @param x,q Vector of quantiles
#' @param p Vector of probabilities
#' @param n Number of observations
#' @param min Lower bound
#' @param max Upper bound
#' @param mode Mode
#' @return Numeric vector
#' @export


# Density function

dtriang <- function(x, min, max, mode){

  if(any(min >= max))
    stop("min must be smaller than max")

  if(any(mode <= min) || any(mode >= max))
    stop("mode must satisfy min < mode < max")

  res <- numeric(length(x))

  # increasing part
  idx1 <- x >= min & x <= mode

  # decreasing part
  idx2 <- x > mode & x <= max

  res[idx1] <- 2*(x[idx1]-min) /
    ((max-min)*(mode-min))

  res[idx2] <- 2*(max-x[idx2]) /
    ((max-min)*(max-mode))

  return(res)

}



# Distribution function (CDF)

#' @rdname dtriang
#' @export

ptriang <- function(q, min, max, mode){

  if(any(min >= max))
    stop("min must be smaller than max")

  if(any(mode <= min) || any(mode >= max))
    stop("mode must satisfy min < mode < max")

  res <- numeric(length(q))

  res[q < min] <- 0
  res[q > max] <- 1

  idx1 <- q >= min & q <= mode
  idx2 <- q > mode & q <= max

  res[idx1] <- ((q[idx1]-min)^2) /
    ((max-min)*(mode-min))

  res[idx2] <- 1 -
    ((max-q[idx2])^2)/
    ((max-min)*(max-mode))

  return(res)

}



# Quantile function

#' @rdname dtriang
#' @export

qtriang <- function(p, min, max, mode){

  if(any(min >= max))
    stop("min must be smaller than max")

  if(any(mode <= min) || any(mode >= max))
    stop("mode must satisfy min < mode < max")

  if(any(p < 0 | p > 1))
    stop("p must be between 0 and 1")

  cut <- (mode-min)/(max-min)

  res <- numeric(length(p))

  idx1 <- p <= cut
  idx2 <- p > cut

  res[idx1] <- min +
    sqrt(p[idx1]*(max-min)*(mode-min))

  res[idx2] <- max -
    sqrt((1-p[idx2])*(max-min)*(max-mode))

  return(res)

}




# Random generation
# Inverse transform method

#' @rdname dtriang
#' @export

rtriang <- function(n, min, max, mode){

  u <- runif(n)

  qtriang(u,min,max,mode)

}


# install.packages("devtools")
# install.packages("testthat")
# devtools::document()
# devtools::check()

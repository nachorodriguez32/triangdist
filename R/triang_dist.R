#' Triangular Distribution Density
#'
#' @description Computes the probability density function of the triangular distribution.
#'
#' @param x vector of quantiles.
#' @param min lower limit of the distribution.
#' @param max upper limit of the distribution.
#' @param mode mode of the distribution.
#'
#' @details The height of the triangle is calculated as 2 / (max - min).
#'
#' @return A numeric vector of densities.
#' @export
dtriang <- function(x, min, max, mode) {

  if (any(min > max)) stop("Error: min cannot be greater than max.")
  if (any(mode < min | mode > max)) stop("Error: mode must be between min and max.")

  h <- 2 / (max - min)

  dens <- ifelse(x < min | x > max, 0,
                 ifelse(x < mode, 2 * (x - min) / ((max - min) * (mode - min)),
                        ifelse(x == mode, h,
                               2 * (max - x) / ((max - min) * (max - mode))
                        )
                 )
  )

  return(dens)
}


#' Triangular Distribution Cumulative Distribution Function
#'
#' @description Computes the cumulative distribution function (CDF) of the triangular distribution.
#'
#' @param q vector of quantiles.
#' @param min lower limit of the distribution.
#' @param max upper limit of the distribution.
#' @param mode mode of the distribution.
#'
#' @return A numeric vector of probabilities.
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min > max)) stop("Error: min cannot be greater than max.")
  if (any(mode < min | mode > max)) stop("Error: mode must be between min and max.")

  prob <- ifelse(q <= min, 0,
                 ifelse(q <= mode, (q - min)^2 / ((max - min) * (mode - min)),
                        ifelse(q < max, 1 - (max - q)^2 / ((max - min) * (max - mode)),
                               1
                        )
                 )
  )
  return(prob)
}

#' Triangular Distribution Quantile Function
#'
#' @description Computes the quantile function (inverse CDF) of the triangular distribution.
#'
#' @param p vector of probabilities.
#' @param min lower limit of the distribution.
#' @param max upper limit of the distribution.
#' @param mode mode of the distribution.
#'
#' @return A numeric vector of quantiles.
#' @export
qtriang <- function(p, min, max, mode) {
  if (any(min > max)) stop("Error: min cannot be greater than max.")
  if (any(mode < min | mode > max)) stop("Error: mode must be between min and max.")
  if (any(p < 0 | p > 1)) stop("Error: probabilities p must be between 0 and 1.")

  threshold <- (mode - min) / (max - min)

  quant <- ifelse(p <= threshold,
                  min + sqrt(p * (max - min) * (mode - min)),
                  max - sqrt((1 - p) * (max - min) * (max - mode))
  )
  return(quant)
}

#' Triangular Distribution Random Generation
#'
#' @description Generates random deviates from the triangular distribution.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param min lower limit of the distribution.
#' @param max upper limit of the distribution.
#' @param mode mode of the distribution.
#'
#' @return A numeric vector of random deviates.
#' @export
rtriang <- function(n, min, max, mode) {
  if (any(min > max)) stop("Error: min cannot be greater than max.")
  if (any(mode < min | mode > max)) stop("Error: mode must be between min and max.")

  if (length(n) > 1) n <- length(n)

  u <- runif(n)
  return(qtriang(p = u, min = min, max = max, mode = mode))
}

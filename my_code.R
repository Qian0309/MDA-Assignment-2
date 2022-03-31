#' Example function
#'
#' Compute an unnormalised negated log-likelihood for set of samples
#' from Poisson models with potentially different parameter values
#'
#' @param lambda A numeric vector
#' @param y A numeric vector of observations from Po(lambda)
#'
#' @return A negated log-likelihood value,
#'   not including the normalisation constant
#'
#' @details This documentation is written in the syntax of the
#'   `roxygen2` package, that can transform it into the syntax
#'   for standard R help files. It mostly operates on whole
#'   R packages, but the syntax is humanly readable also in
#'   stand-alone scripts like this one. Similar documentation
#'   syntax systems exist for Java and C/C++.
#'
my_function <- function(lambda, y) {
  sum(lambda - log(lambda) * y)
}

#' Quadrupole bias correction factors
#'
#' A dataset containing the experimentally determined peak area correction
#' factors to adjust for differential isotope transit through the quadrupole.
#'
#' @format A tibble with `r nrow(correction_factors)` rows and
#' `r ncol(correction_factors)` variables:
#'
#' \describe{
#'   \item{metabolite}{name of metabolite}
#'   \item{M}{specific isotope}
#'   \item{cf}{correction factor that is multiplied by peak area}
#'   }
"correction_factors"

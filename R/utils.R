#' Negate %in%
#'
#' Binary operator that returnrs a logical vector returning `TRUE` if there is
#' not a match for its left operand.
#'
#' @param x The values to be matched
#' @param y The values to be matched against
#'
#' @export
"%nin%" <- function(x, y) {!(x %in% y)}

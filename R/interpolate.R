#' Interpolate x values from a standard curve
#'
#' This function takes a model object and a data frame containing new y values
#' and will interpolate the associated x values. This is useful for determining
#' sample concentrations from a standard curve. The intention is that it will
#' operate similarly to `predict()`. Currently, it will support polynomial fits.
#'
#' There is nothing yet in the function that protects from multiple roots being
#' returned. In addition, it will return `NA` if the new x values lie outside of
#' the range of the x values in the model.
#'
#' @param new_df Data frame containing new y values
#' @param model Linear model
#'
#' @return List of interpolated x values
#' @export
#'
#' @examples
#' x <- seq(from = 2, to = 10, by = 2)
#' y <- 2 * x
#' m <- lm(y ~ x)
#' new_y <- data.frame(y = seq(from = 1, to = 11, by = 2))
#' interpolate(new_y, m)
 interpolate <- function(new_df, model) {
  x <- stats::model.frame(model)[[deparse(model$terms[[3]])]]
  p <- polynom::polynomial(stats::coefficients(model))
  new_y <- as.list(new_df[[deparse(model$terms[[2]])]])
  new_x <- unlist(lapply(new_y, function(y) solve(p, y)))
  new_x <- round(new_x, digits = 8)
  replace(new_x, new_x < min(x) | new_x > max(x), NA)
}

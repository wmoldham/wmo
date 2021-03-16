#' Generate standard curves
#'
#' This function takes a tibble, groups by all variables except for concentration
#' and value, fits a linear model, extracts summary statistics, and generates
#' a plot. These linear models can then be used to interpolate values.
#'
#' @param df A tibble containing columns `conc` and `value` for generating a
#'     standard curve.
#'
#' @return A tibble with the original `data`, linear models, summary statistics,
#'     and plots of the standard curves.
#' @export
#'
make_std_curves <- function(df) {
  df %>%
    dplyr::filter(!is.na(.data$conc)) %>%
    dplyr::select(tidyselect::where(~all(!is.na(.)))) %>%
    dplyr::group_by(dplyr::across(-c(.data$conc, .data$value))) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      title = stringr::str_c(!!!rlang::syms(dplyr::groups(.data)), sep = "_")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      model = furrr::future_map(.data$data, ~lm(value ~ conc, data = .x, na.action = modelr::na.warn)),
      summary = furrr::future_map(.data$model, broom::glance),
      plots = furrr::future_map2(.data$data, .data$title, make_std_plots)
    ) %>%
    dplyr::group_by(
      dplyr::across(
        -c(.data$data, .data$title, .data$model, .data$summary, .data$plots)
      )
    )
}


#' Generate generic standard curves
#'
#' @param df A tibble containing data for a standard curve
#' @param title Optional to label the standard curve
#'
#' @return A ggplot
#' @export
#'
make_std_plots <- function(df, title = NULL) {
  ggplot2::ggplot(df) +
    ggplot2::aes(x = .data$conc, y = .data$value) +
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ x,
      color = "gray20",
      se = FALSE
    ) +
    ggplot2::geom_point(
      size = 3,
      alpha = 0.3,
      color = "blue"
    ) +
    ggplot2::stat_summary(
      fun = "mean",
      size = 4,
      geom = "point",
      alpha = 0.8,
      color = "blue"
    ) +
    ggplot2::labs(
      x = "Concentration",
      y = "Value",
      title = title
    )
}

#' Interpolate x values from a standard curve
#'
#' This function takes a model object and a data frame containing new y values
#' and will interpolate the associated x values. This is useful for determining
#' sample concentrations from a standard curve. The intention is that it will
#' operate similarly to `predict()`. Currently, it will support polynomial fits.
#' The function will return `NA` if the new y value is outside the range of the
#' standard curve. For polynomial fits, there must be only a single solution in
#' the range of the standard curve!
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
  new_x <- unlist(lapply(new_y, function(y) {
    roots <- solve(p, y)
    roots <- round(roots, digits = 8)
    root <- roots[which(Im(roots) == 0 & Re(roots) >= 0 & Re(roots) <= 1.25 * max(x))]
    ifelse(identical(root, numeric(0)), NA, Re(root))
  }))
  new_x
}

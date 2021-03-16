#' Remove outliers using median absolute deviation
#'
#' This function takes a tibble and will remove outliers from a specific column
#' using a median absolute deviation > 2. It returns the original tibble with a
#' new column `filtered` that contains NA in place of filtered values.
#'
#' @param tbl_df Tibble containing data to be filtered
#' @param column Column name of tibble from which outliers are to be removed
#' @param remove If TRUE, removes outlier observations from returned tibble
#'
#' @return If `remove == FALSE`, a tibble containing a new column, `filtered`,
#'   where filtered values have been replaced by NA. If `remove == TRUE`,
#'   outlier values have been removed from the tibble.
#'
#' @importFrom stats mad median
#' @importFrom dplyr bind_cols filter select
#'
#' @export
#'
#' @examples
#' remove_outliers(ggplot2::diamonds, "price")
remove_outliers <- function(tbl_df, column, remove = FALSE) {
  # col <- ensym(column)
  x <- tbl_df[[{{column}}]]
  y <- replace(x, which(abs(x - median(x)) / mad(x) > 2), NA)
  z <- dplyr::bind_cols(tbl_df, filtered = y)

  if (remove) {
    # message(sum(is.na(y)), " outliers have been removed")
    dplyr::filter(z, !is.na(.data$filtered)) %>%
      dplyr::select(-.data$filtered)
  } else {
    z
  }
}


#' Remove outliers from grouped tibble
#'
#' This function takes a grouped tibble and will remove outliers from a specific
#' column using a median absolute deviation > 2. It returns the original tibble
#' with a new column `filtered` that contains NA in place of filtered values.
#'
#' @param tbl_df Grouped tibble containing data to be filtered
#' @param column Column name of tibble from which outliers are to be removed
#' @param remove If `TRUE`, removes outlier observations from returned tibble
#'
#' @return If `remove == FALSE`, a tibble containing a new column, `filtered`,
#'   where filtered values have been replaced by NA. If `remove == TRUE`,
#'   outlier values have been removed from the tibble.
#'
#' @importFrom tidyr nest unnest
#' @importFrom dplyr is_grouped_df mutate
#' @importFrom purrr map
#' @importFrom rlang abort
#'
#' @export
#'
#' @examples
#' a <- dplyr::group_by(ggplot2::diamonds, cut)
#' remove_nested_outliers(a, price, FALSE)
remove_nested_outliers <- function(tbl_df, column, remove = FALSE) {

  if (!dplyr::is_grouped_df(tbl_df)) {
    rlang::abort(message = "Tibble has no groups")
  }

  tbl_df %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(
        .data$data,
        wmo::remove_outliers,
        column = ensym(column),
        remove = remove
      )
    ) %>%
    tidyr::unnest(c(.data$data))
}


#' Pivot and remove outlying technical replicates from columns a:c
#'
#' This function will combine columns labeled a:c, group by the remaining
#' variables in the tibble, and remove outliers based on the median absolute
#' deviation.
#'
#' @param df A tibble containing technical replicates in columns labeled a:c.
#'
#' @return A tibble containing summarized data for the technical replicates.
#' @export
#'
clean_tech_reps <- function(df) {
  tidyr::pivot_longer(
    data = df,
    cols = .data$a:.data$c,
    names_to = "replicate",
    values_to = "value"
  ) %>%
    dplyr::group_by(dplyr::across(-c(.data$replicate, .data$value))) %>%
    dplyr::mutate(
      value = replace(.data$value, which(abs(.data$value - median(.data$value)) / mad(.data$value) > 2), NA)
    ) %>%
    dplyr::summarise(value = mean(.data$value, na.rm = TRUE)) %>%
    dplyr::ungroup()
}

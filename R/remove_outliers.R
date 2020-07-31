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
#' @return If remove = FALSE, a tibble containing a new column, `filtered`,
#'   where filtered values have been replaced by NA. If remove = TRUE, outlier
#'   values have been removed from the tibble.
#'
#' @importFrom stats mad median
#' @importFrom dplyr bind_cols filter select
#'
#' @export
#'
#' @examples
#' remove_outliers(ggplot2::diamonds, "price")
remove_outliers <- function(tbl_df, column, remove = FALSE) {
  x <- tbl_df[[column]]
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

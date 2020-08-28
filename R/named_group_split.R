#' Add names to tibble list produced from group_split
#'
#' @param .tbl A tibble
#' @param ... Variables to group by
#'
#' @return A named list of tibbles.
#' @export
named_group_split <- function(.tbl, ...) {
  grouped <- dplyr::group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!dplyr::group_keys(grouped), sep = " / ")))

  grouped %>%
    dplyr::group_split() %>%
    rlang::set_names(names)
}

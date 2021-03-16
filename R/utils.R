#' Negate %in%
#'
#' Binary operator that returns a logical vector returning `TRUE` if there is
#' not a match for its left operand.
#'
#' @param x The values to be matched
#' @param y The values to be matched against
#'
#' @export
"%nin%" <- function(x, y) {!(x %in% y)}


#' Reads and combines multi-sheet Excel files
#'
#' This function extracts and combines Excel sheets from multiple files.
#'
#' @param excel_file_list A preferably named list of Excel files.
#'
#' @return A list of tibbles for each unique Excel sheet in the submitted files.
#' @export
#'
read_multi_excel <- function(excel_file_list) {
  sheets <- unique(unlist(purrr::map(excel_file_list, readxl::excel_sheets)))
  purrr::map(
    sheets,
    ~purrr::map_dfr(
      excel_file_list,
      readxl::read_excel,
      sheet = .x,
      .id = "experiment",
      col_types = "text"
    )
  ) %>%
    rlang::set_names(sheets)
}


#' Save multiple ggplots
#'
#' @param plots List of plots to save
#' @param names Base filenames of plots
#' @param path Directory to save plots
#' @param width Plot width in cm
#' @param height Plot height in cm
#'
#' @export
#'
print_plots <- function(plots, names, path, width = 20, height = 15) {
  furrr::future_walk2(
    plots,
    names,
    ~ ggplot2::ggsave(
      filename = stringr::str_c(.y, ".pdf"),
      path = path,
      plot = .x,
      device = cairo_pdf,
      width = width,
      height = height,
      units = "cm"
    )
  )
}

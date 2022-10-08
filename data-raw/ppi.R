# ppi.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidygraph)
})

nodes <-
  read_delim(
    "data-raw/PPI201806_large_proteins.txt",
    delim = "\t",
    col_names = c("symbol", "entrez"),
    col_types = "cc"
  )

edges <-
  read_delim(
    "data-raw/PPI201806_large.txt",
    delim = "\t",
    col_names = c("entrez1", "from", "entrez2", "to"),
    col_types = "cccc"
  ) |>
  select(!contains("entrez"))

ppi <-
  tbl_graph(
    nodes = nodes,
    edges = edges,
    directed = FALSE,
    node_key = "symbol"
  )

usethis::use_data(ppi, overwrite = TRUE)

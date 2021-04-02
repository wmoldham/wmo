library(tidyverse)

hmdb_mappings <- readr::read_csv("data-raw/metabolite-library.csv")

usethis::use_data(hmdb_mappings, overwrite = TRUE)

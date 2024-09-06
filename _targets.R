library(targets)
tar_source("R")

list(
  # data import
  tar_target(m, read_mama_data())
)

#!/usr/bin/env Rscript

prop.capital

table.features <- function(id){
  table <- read.csv(paste(id, '.csv', sep = ''))
  c(
    id = id,
    nrow = nrow(table),
    ncol = ncol(table),

  )
}

#!/usr/bin/env Rscript

prop.cap

table.features <- function(id){
  table <- read.csv(paste(id, '.csv', sep = ''))
  header <- paste(colnames(table), collapse = '')
  c(
    id = id,
    nrow = nrow(table),
    ncol = ncol(table),
    header.nchar = length(header),
    header.nlowercase = length(gsub('[^a-z]', '', header)),
    header.nlowercase = length(gsub('[^A-Z]', '', header)),
  )
}

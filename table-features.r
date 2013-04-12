#!/usr/bin/env Rscript
library(psych)

table.features <- function(id){
  table <- read.csv(paste('rows/', id, '.csv', sep = ''), stringsAsFactors = F)
  header <- paste(colnames(table), collapse = '')

  # Year column
  if ('year' %in% colnames(table)) {
    year <- table$year
  } else if ('Year' %in% colnames(table)) {
    year <- table$Year
  } else {
    year <- NA
  }

  # Column types
  col.types <- table(sapply(table, typeof))

  features <- c(
    id = id,
    nrow = nrow(table),
    ncol = ncol(table),
    header.nchar = nchar(header),
    header.nlowercase = nchar(gsub('[^a-z]', '', header)),
    header.nlowercase = nchar(gsub('[^A-Z]', '', header)),
    year.mean = mean(year),
    year.median = median(year),
    year.max = max(year),
    year.min = min(year),
    year.var = var(year),
    year.skew = skew(year),
    year.kurtosis = kurtosi(year),
    ncol.character = col.types[['character']],
    ncol.integer = col.types[['integer']]
  )
}

t(t(table.features('zpd4-gad8')))

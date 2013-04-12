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
  date.cols <- grep('date', tolower(colnames(table)))
  if (0 == length(date.cols)) {
    .table <- table
  } else {
    .table <- table[-date.cols]
  }
  print(colnames(table))
  print(.table[1,])
  col.types <- table(sapply(.table, typeof))
  col.types[['date']] <- length(date.cols)

  features <- c(
    id = id,
    nrow = nrow(table),
    ncol = ncol(table),
    header.nchar = nchar(header),
    header.nlowercase = nchar(gsub('[^a-z]', '', header)),
    header.nuppercase = nchar(gsub('[^A-Z]', '', header)),
    year.mean = mean(year),
    year.median = median(year),
    year.max = max(year),
    year.min = min(year),
    year.var = var(year),
    year.skew = skew(year),
    year.kurtosis = kurtosi(year),
    ncol.character = col.types[['character']],
    ncol.integer = col.types[['integer']],
    ncol.date = col.types[['date']]
  )
}

t(t(table.features('zpd4-gad8')))

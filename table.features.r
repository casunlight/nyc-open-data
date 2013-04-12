#!/usr/bin/env Rscript
library(psych)

table.features <- function(id){
  table.df <- read.csv(paste('rows/', id, '.csv', sep = ''), stringsAsFactors = F)
  header <- paste(colnames(table.df), collapse = '')

  # Year column
  if ('year' %in% colnames(table.df)) {
    year <- table.df$year
  } else if ('Year' %in% colnames(table.df)) {
    year <- table.df$Year
  } else {
    year <- NA
  }

  # Column types
  date.cols <- grep('date', tolower(colnames(table.df)))
  if (0 == length(date.cols)) {
    .table.df <- table.df
  } else {
    .table.df <- table.df[-date.cols]
  }
  .t <<- .table.df
  
  if (ncol(.table.df) == 0) {
    col.types <- list()
  } else {
    col.types <- table(sapply(.table.df, typeof))
  }

  if (!'character' %in% col.types) {
    col.types[['character']] <- 0
  }
  if (!'integer' %in% col.types) {
    col.types[['integer']] <- 0
  }
  col.types[['date']] <- length(date.cols)

  features <- data.frame(
    id = id,
    nrow = nrow(table.df),
    ncol = ncol(table.df),
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

ids <- sub('^rows/', '', sub('.csv$', '', list.files('rows')))
write.table(table.features(ids[1]), file = "table.features.csv", append = F, row.names = F, sep = ',')
for (id in ids[-1]) {
  write.table(table.features(id), file = "table.features.csv", append = T, col.names = F, row.names = F, sep = ',')
}

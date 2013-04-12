#!/usr/bin/env Rscript
library(ggplot2)
library(scales)

# nyc <- read.csv('table.features.csv')
p.dimensions <- ggplot(nyc) + aes(x = ncol, y = nrow, label = id) +
  scale_x_log10('Number of columns', labels = comma) +
  scale_y_log10('Number of rows', labels = comma) +
  labs(title = 'Dimensions of NYC open data')
print(p.dimensions + geom_point())
#print(p.dimensions + geom_text())

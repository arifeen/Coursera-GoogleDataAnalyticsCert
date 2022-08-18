setwd("/home/samsil/Documents/Capstone-google/CaseStudy1-Cyclistic/Samsil Arifeen")


library(disk.frame)
setup_disk.frame(workers = 4)
options(future.globals.maxSize = Inf)
library(tidyverse)
library(data.table)

path_to_data <- "/home/samsil/Documents/Capstone-google/CaseStudy1-Cyclistic/Samsil Arifeen/df_final_1.csv"

some.df = csv_to_disk.frame(
  infile = "df_final_1.csv", 
  outdir = "df_final_1.df",
  in_chunk_size = NULL,
  inmapfn = base::I,
  shardby = NULL,
  compress = 50,
  overwrite = TRUE,
  header = TRUE,
  .progress = TRUE,
  backend = c("data.table", "readr", "LaF"),
  chunk_reader = c("bigreadr", "data.table", "readr", "readLines"),
)



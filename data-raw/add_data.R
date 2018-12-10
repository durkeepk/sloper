#### Adding Datasets to a Package

# 1. Load in the dataset from raw_data

sloper_exdat <- read.csv("data-raw/sloper_exdat.csv")

# 2. Create the RData object & Add to the data folder
usethis::use_data(sloper_exdat, overwrite = T)

library(sloper)

library(data.table)
library(tidyverse)


vars <- c("r_id", "t_id", "strength_rating", "t_measured_strength")

df <- sloper_exdat[ , vars]


rater_IDs <- "r_id"
target_IDs <- "t_id"

N_ratings <- length(unique(df[,target_IDs]))

#https://stats.stackexchange.com/questions/111686/choosing-one-pair-randomly-from-repeated-pairs
boot_data <- data.frame(data.table(df)
                        [,.SD[sample(1:.N, N_ratings, replace = T),],
                          by = rater_IDs, ])

int_slope <- sloper::get_slopes(boot_data,
                                response = "strength_rating",
                                contingency = "t_measured_strength",
                                compress = "rater", maximal = T)

merge()

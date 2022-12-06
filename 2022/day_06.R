# December 6, 2022

library(purrr)
source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(6, 2022)

in_vec <- strsplit(pzl_data, "") %>% unlist

for (n in 4:length(in_vec)) {
  if (identical(in_vec[(n-3):n], unique(in_vec[(n-3):n]))) {
    print(n)
    return(n)
  }
}

n_pack <- 14
for (n in n_pack:length(in_vec)) {
  if (identical(in_vec[(n-n_pack+1):n], unique(in_vec[(n-n_pack+1):n]))) {
    print(n)
    return(n)
  }
}

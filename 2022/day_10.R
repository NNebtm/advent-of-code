# December 10, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(10, 2022)

do_op <- function(start, op) {
  if (op == "noop") {
    return(start)
  } else {
    addop <- strsplit(op, " ") %>% unlist
    return(c(start, start + as.integer(addop[2])))
  }
}

# cycle_vec[n] is the value of the register at the beginning of the n'th cycle.
cycle_vec <- 1
for (op in pzl_data) {
  cycle_vec <- c(cycle_vec, do_op(tail(cycle_vec, 1), op))
}
cycle_vec

purrr::map_dbl(40*1:6-20, function(x) cycle_vec[x]*x) %>% sum

# part 2

crt_out <- purrr::map_chr(1:240, function(x) 
  if (abs(((x-1) %% 40) - cycle_vec[x])<=1) {
    return("#")
  } else {
    return(".")
  }
) 

for (n in 1:6) {
  print(crt_out[((n-1)*40+1):(n*40)])
}


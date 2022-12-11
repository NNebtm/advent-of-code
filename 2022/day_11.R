# December 11, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(11, 2022)

do_op <- function(op, num) {
  op_vec <- strsplit(op, " ") %>% unlist
  
  if (op_vec[1] == "old") {
    left <- num
  } else {
    left <- as.double(op_vec[1])
  }
  
  if (op_vec[3] == "old") {
    right <- num
  } else {
    right <- as.double(op_vec[3])
  }
  
  if (op_vec[2] == "+") {
    out <- left + right
  } else if (op_vec[2] == "*") {
    out <- left * right
  }
  
  return(out)
}

full_test <- function(num, op, test, ttarg, ftarg) {
  new <- do_op(op, num)
  new <- floor(new/3)
  if(new %% test == 0) {
    target <- ttarg
  } else {
    target <- ftarg
  }
  out <- c(target, new)
  return(out)
}

# Load the data and change from index 0 to index 1.
items <- pzl_data[grepl("Starting items:", pzl_data)] %>% 
  gsub("  Starting items: ", "", .) %>% 
  purrr::map(function(x) strsplit(x = x, split = ", ") %>% 
               unlist %>% 
               as.integer)

n_items <- items %>% unlist %>% length

operations <- pzl_data[grepl("Operation:", pzl_data)] %>% 
  gsub("  Operation: new = ", "", .)

tests <- pzl_data[grepl("Test:", pzl_data)] %>% 
  gsub("  Test: divisible by ", "", .) %>% 
  as.integer

true_targets <- pzl_data[grepl("If true:", pzl_data)] %>% 
  gsub("    If true: throw to monkey ", "", .) %>% 
  as.integer + 1

false_targets <- pzl_data[grepl("If false:", pzl_data)] %>% 
  gsub("    If false: throw to monkey ", "", .) %>% 
  as.integer + 1

n_monkey <- pzl_data[grepl("Monkey \\d*:", pzl_data)] %>% length

interactions_1 <- rep(0, n_monkey)
items_1 <- items

for (a in 1:20) {
  for (b in 1:n_monkey) {
    for (item in items_1[[b]]) {
      toss <- full_test(item, operations[b], tests[b], true_targets[b], false_targets[b])
      new_monk <- toss[1]
      new_item <- toss[2]
      items_1[[new_monk]] <- c(items_1[[new_monk]], new_item)
      items_1[[b]] <- tail(items_1[[b]], -1)
      interactions_1[b] <- interactions_1[b] + 1
    }
  }
}
items_1
interactions_1 %>% sort %>% tail(2) %>% prod

# part 2

# Since the that determine to which monkey the item is tossed is based on a 
# modulo operation, it is unchanged if we take the worry level modulo the product
# of all test values first. Had the tests not been the first 8 primes, a smaller
# number which had all tests as divisors could likely have been found.
test_prod <- tests %>% prod

full_test_2 <- function(num, op, test, ttarg, ftarg) {
  new <- do_op(op, num)
  new <- new %% test_prod
  if(new %% test == 0) {
    target <- ttarg
  } else {
    target <- ftarg
  }
  out <- c(target, new)
  return(out)
}

interactions_2 <- rep(0, n_monkey)
items_2 <- items


for (a in 1:10000) {
  for (b in 1:n_monkey) {
    for (item in items_2[[b]]) {
      toss <- full_test_2(item, operations[b], tests[b], true_targets[b], false_targets[b])
      new_monk <- toss[1]
      new_item <- toss[2]
      items_2[[new_monk]] <- c(items_2[[new_monk]], new_item)
      items_2[[b]] <- tail(items_2[[b]], -1)
      interactions_2[b] <- interactions_2[b] + 1
    }
  }
}
items_2
interactions_2

interactions_2 %>% sort %>% tail(2) %>% prod


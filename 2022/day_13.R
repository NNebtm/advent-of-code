# December 13, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(13, 2022)
pzl_test <- c("[1,1,3,1,1]",
              "[1,1,5,1,1]",
              "",
              "[[1],[2,3,4]]",
              "[[1],4]",
              "",
              "[9]",
              "[[8,7,6]]",
              "",
              "[[4,4],4,4]",
              "[[4,4],4,4,4]",
              "",
              "[7,7,7,7]",
              "[7,7,7]",
              "",
              "[]",
              "[3]",
              "",
              "[[[]]]",
              "[[]]",
              "",
              "[1,[2,[3,[4,[5,6,7]]]],8,9]",
              "[1,[2,[3,[4,[5,6,0]]]],8,9]")

# left_vec <- pzl_test[seq(1, length(pzl_test), 3)] %>% gsub("\\[", "list(", .) %>% gsub("\\]", ")", .)
# right_vec <- pzl_test[seq(2, length(pzl_test), 3)] %>% gsub("\\[", "list(", .) %>% gsub("\\]", ")", .)

left_vec <- pzl_data[seq(1, length(pzl_data), 3)] %>% gsub("\\[", "list(", .) %>% gsub("\\]", ")", .)
right_vec <- pzl_data[seq(2, length(pzl_data), 3)] %>% gsub("\\[", "list(", .) %>% gsub("\\]", ")", .)

left_list <- purrr::map(as.list(left_vec), function(x) eval(parse(text = x)))
right_list <- purrr::map(as.list(right_vec), function(x) eval(parse(text = x)))

comp_list <- function(left, right, test = FALSE) {
  if (test) browser()
  if (!identical(left, right)) {
    if (class(left) == "list" & class(right) == "numeric") {
      right <- list(right)
    } else if(class(left) == "numeric" & class(right) == "list") {
      left <- list(left)
    } 
    
    if (class(left) == "numeric" & class(right) == "numeric") {
      if (left < right) {
        return(TRUE)
      } else if (left > right) {
        return(FALSE)
      }
    }
    
    min_len <- min(length(left), length(right))
    # We will only try to look at the contents of the lists if they are both 
    # lists and both at least of length 1
    if (class(left) == "list" & class(left) == "list" & length(left) > 0 & length(right) > 0) {
      for (n in 1:min_len) {
        res <- comp_list(left[[n]], right[[n]], test = test)
        if (is.logical(res)) {
          return(res)
        }
      }
    }
    if (length(left) < length(right)) {
      return(TRUE)
    } else if (length(left) > length(right)) {
      return(FALSE)
    }
  }
}

# comp_list(left_list[[6]], right_list[[6]], test = TRUE)

res_vec <- purrr::map2_lgl(left_list, right_list, comp_list)
which(res_vec) %>% sum

# part 2

tot_list <- left_list %>% append(right_list) %>% append(list(list(2))) %>% append(list(list(6)))

for (n in seq_along(tot_list)) {
  class(tot_list[[n]]) <- "aoc_list"
}

names(tot_list) <- as.character(seq_along(tot_list))

#We will define a new class with custom sorting
class(tot_list) <- "aoc_list"

'[.aoc_list' <- function(x, i) {
  class(x) <- "list"
  x <- x[i]
  class(x) <- "aoc_list"
  x
}

'<.aoc_list' <- function(a,b) {
  a <- purrr::flatten(a)
  b <- purrr::flatten(b)
  comp_list(a,b)
}

'>.aoc_list' <- function(a,b) {
  b < a
}

'==.aoc_list' <- function(a,b) {
  identical(a,b)
}

sort_list <- sort(tot_list) %>% names

which(sort_list == "301") * which(sort_list == "302")

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

#Turn the pseudo-code lists into R lists.
left_vec <- pzl_data[seq(1, length(pzl_data), 3)] %>% gsub("\\[", "list(", .) %>% gsub("\\]", ")", .)
right_vec <- pzl_data[seq(2, length(pzl_data), 3)] %>% gsub("\\[", "list(", .) %>% gsub("\\]", ")", .)

#Turn the strings into actual lists.
left_list <- purrr::map(as.list(left_vec), function(x) eval(parse(text = x)))
right_list <- purrr::map(as.list(right_vec), function(x) eval(parse(text = x)))

comp_list <- function(left, right, test = FALSE) {
  if (test) browser()
  #If the two entries are indentical, we have a tie and move on to the next step.
  if (!identical(left, right)) {
    #If one entry is a list and the other numeric, make the numeric a list.
    if (class(left) == "list" & class(right) == "numeric") {
      right <- list(right)
    } else if(class(left) == "numeric" & class(right) == "list") {
      left <- list(left)
    } 
    
    #If both are numeric, find out if left is smaller than right. 
    if (class(left) == "numeric" & class(right) == "numeric") {
      if (left < right) {
        return(TRUE)
      } else if (left > right) {
        return(FALSE)
      }
    }
    
    #We will loop through both lists, so we need the minimum length to ensure
    #that we don't try to access a non-existing list-element.
    min_len <- min(length(left), length(right))
    # We will only try to look at the contents of the lists if they are both 
    # lists and both non-empty.
    if (class(left) == "list" & class(left) == "list" & length(left) > 0 & length(right) > 0) {
      for (n in 1:min_len) {
        res <- comp_list(left[[n]], right[[n]], test = test)
        if (is.logical(res)) {
          return(res)
        }
      }
    }
    
    #If we have gotten this far, we should evaluate by the length of the lists:
    #The first one to run out should be considered smaller.
    if (length(left) < length(right)) {
      return(TRUE)
    } else if (length(left) > length(right)) {
      return(FALSE)
    }
  }
}

# comp_list(left_list[[6]], right_list[[6]], test = TRUE)

#Find out which lists are ordered correctly and add up the indices.
res_vec <- purrr::map2_lgl(left_list, right_list, comp_list)
which(res_vec) %>% sum

# part 2

#Construct a new list of all actual entries as well as two new tracing signals.
tot_list <- left_list %>% append(right_list) %>% append(list(list(2))) %>% append(list(list(6)))

#We are naming the entries to be able to keep track of them and find the indices
#of the two tracing signals.
names(tot_list) <- as.character(seq_along(tot_list))

#We will define a new class with custom sorting to sort the lists.
class(tot_list) <- "aoc_list"

for (n in seq_along(tot_list)) {
  class(tot_list[[n]]) <- "aoc_list"
}

'[.aoc_list' <- function(x, i) {
  class(x) <- "list"
  x <- x[i]
  class(x) <- "aoc_list"
  x
}

'<.aoc_list' <- function(a,b) {
  #flatten ensures that the actual list is pulled out from the named list.
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

#Find the sorted list and record the names
sort_list <- sort(tot_list) %>% names

#Find the product of the indices of the two tracing signals.
which(sort_list == "301") * which(sort_list == "302")

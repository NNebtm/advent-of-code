# December 7, 2022

library(rlang)
library(purrr)
source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(7, 2022)

pzl_test <- c("$ cd /",
              "$ ls",
              "dir a",
              "14848514 b.txt",
              "8504156 c.dat",
              "dir d",
              "$ cd a",
              "$ ls",
              "dir e",
              "29116 f",
              "2557 g",
              "62596 h.lst",
              "$ cd e",
              "$ ls",
              "584 i",
              "$ cd ..",
              "$ cd ..",
              "$ cd d",
              "$ ls",
              "4060174 j",
              "8033020 d.log",
              "5626152 d.ext",
              "7214296 k")

# Create a list of each directory.
# The first element is the path to the directory, each other element is a file
# or contained directory.
pzl_list <- list()
new_vec <- c()
pointer  <- c()
for (line in pzl_data) {
  if (grepl("\\$ cd", line) & !grepl("\\.\\.", line)) {
    pointer <- c(pointer, gsub("\\$ cd |\\/", "", line))
    pzl_list <- append(pzl_list, list(new_vec))
    new_vec <- paste(pointer, collapse = "/")
  } else if (line == "$ cd ..") {
    pointer <- head(pointer, -1)
  } else if (!grepl("\\$", line)) {
    new_vec <- c(new_vec, line)
  }
}
pzl_list <- append(pzl_list, list(new_vec))
pzl_list <- tail(pzl_list, -1)

# Replace each sub directory name with the path to that directory
# Remove file names and keep only file sizes
for (n in seq_along(pzl_list)) {
  pzl_list[[n]] <- gsub("dir ", paste0(pzl_list[[n]][1], "/"), pzl_list[[n]])
  pzl_list[[n]] <- gsub("([0-9]*) .*", "\\1", pzl_list[[n]])
}

# Create a vector with all directory names
pzl_dirs <- map_chr(pzl_list, `[`, 1)

# Starting from the bottom, replace each subdirectory with the total size of files 
# contained in that subdirectory.
for (m in rev(seq_along(pzl_list))) {
  for (n in 2:length(pzl_list[[m]])) {
    if (pzl_list[[m]][n] %in% pzl_dirs) {
      pzl_list[[m]][n] <- sum(as.integer(tail(pzl_list[[which(pzl_list[[m]][n] == pzl_dirs)]], -1)))
    }
  }
}

# Find the total size of each directory and the sum of all "small size" dir's
pzl_size <- map_int(pzl_list, function(x) sum(as.integer(tail(x, -1))))
pzl_size[pzl_size <= 100000] %>% sum()

#Part 2

#Find the needed space
need <- 30000000 - (70000000 - pzl_size[1])

#Find the smalles dir size larger than the needed space.
pzl_size[pzl_size >= need] %>% min

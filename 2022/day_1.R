# December 1, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(1, 2022)

elf_splits <- which(pzl_data == "")

elf_vec <- c()
i_start <- 1
for (split in seq_along(elf_splits)) {
  i_end <- elf_splits[split] - 1
  elf_vec[split] <- sum(as.integer(pzl_data[i_start:i_end]))
  i_start <- elf_splits[split] + 1
}

which(elf_vec == max(elf_vec))
max(elf_vec)

sort(elf_vec, decreasing = TRUE)[1:3] %>% sum


#Solution using split

j <- 1
split_fac <- c()
for (i in seq_along(pzl_data)) {
  if(pzl_data[i] == "") {
    split_fac[i] <- 0
    j <- j + 1
  } else {
    split_fac[i] <- j
  }
}
split_fac <- factor(split_fac)

elf_list <- sapply(split(pzl_data, split_fac), function(x) sum(as.integer(x)))
max(elf_list, na.rm = TRUE)

sort(elf_list, decreasing = TRUE)[1:3] %>% sum(na.rm = TRUE)

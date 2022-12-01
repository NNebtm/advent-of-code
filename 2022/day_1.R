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

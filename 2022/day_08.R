# December 8, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(8, 2022)
pzl_test <- c("30373",
              "25512",
              "65332",
              "33549",
              "35390")

library(purrr)

pzl_mat <- matrix(unlist(map(strsplit(pzl_data, ""), as.integer)), ncol = 99, byrow = TRUE)
pzl_mat <- matrix(unlist(map(strsplit(pzl_test, ""), as.integer)), ncol = 5, byrow = TRUE)

m_max <- dim(pzl_mat)[1]
n_max <- dim(pzl_mat)[2]

vis_mat <- matrix(rep(0,m_max*n_max), ncol = m_max, byrow = TRUE)

for (m in 1:m_max) {
  for (n in 1:n_max) {
    if (m %in% c(1,m_max) |
        n %in% c(1,n_max)) {
      vis_mat[m,n] <- 1
      # print(paste(m,n,1))
    } else if (all(pzl_mat[m,n] > pzl_mat[1:(m-1),n]) |
               all(pzl_mat[m,n] > pzl_mat[(m+1):m_max,n]) |
               all(pzl_mat[m,n] > pzl_mat[m,1:(n-1)]) |
               all(pzl_mat[m,n] > pzl_mat[m, (n+1):n_max])) {
      vis_mat[m,n] <- 1
      # print(paste(m,n,1))
    }
    # print(paste(m,n))
  }
}

vis_mat %>% as.list %>% unlist %>% sum

#Part 2
sce_mat <- matrix(rep(0,m_max*n_max), ncol = m_max, byrow = TRUE)

for (m in 1:m_max) {
  for (n in 1:n_max) {
    if (m %in% c(1,m_max) |
        n %in% c(1,n_max)) {
      sce_mat[m,n] <- 0
    } else {
      left <- cumprod(pzl_mat[m,n] > rev(pzl_mat[1:(m-1),n]))
      if (any(left == 0)) {
        left <- sum(left) + 1
      } else {
        left <- sum(left)
      }
      right <- cumprod(pzl_mat[m,n] > pzl_mat[(m+1):m_max,n])
      if (any(right == 0)) {
        right <- sum(right) + 1
      } else {
        right <- sum(right)
      }
      up <- cumprod(pzl_mat[m,n] > rev(pzl_mat[m,1:(n-1)]))
      if (any(up == 0)) {
        up <- sum(up) + 1
      } else {
        up <- sum(up)
      }
      down <- cumprod(pzl_mat[m,n] > pzl_mat[m, (n+1):n_max])
      if (any(down == 0)) {
        down <- sum(down) + 1
      } else {
        down <- sum(down)
      }
      
      sce_mat[m,n] <- left*right*up*down
    }
  }
}

sce_mat %>% as.list %>% unlist %>% max


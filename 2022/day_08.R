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
# pzl_mat <- matrix(unlist(map(strsplit(pzl_test, ""), as.integer)), ncol = 5, byrow = TRUE)

m_max <- dim(pzl_mat)[1]
n_max <- dim(pzl_mat)[2]

vis_mat <- matrix(rep(0,m_max*n_max), ncol = m_max, byrow = TRUE)

# Loop through all trees. If we are on an edge or if the height is greater than 
# all trees in any of the four directions, it is visible.
for (m in 1:m_max) {
  for (n in 1:n_max) {
    if (m %in% c(1,m_max) |
        n %in% c(1,n_max)) {
      vis_mat[m,n] <- 1
    } else if (all(pzl_mat[m,n] > pzl_mat[1:(m-1),n]) |
               all(pzl_mat[m,n] > pzl_mat[(m+1):m_max,n]) |
               all(pzl_mat[m,n] > pzl_mat[m,1:(n-1)]) |
               all(pzl_mat[m,n] > pzl_mat[m, (n+1):n_max])) {
      vis_mat[m,n] <- 1
    }
  }
}

#Turn the visibility matrix into an integer vector and sum it.
vis_mat %>% as.list %>% unlist %>% sum

#Part 2
sce_mat <- matrix(rep(0,m_max*n_max), ncol = m_max, byrow = TRUE)

# Loop through all trees. If we are on an edge, the scenic score is 0.
# If not, look each each cardinal direction and compare the tree's height to theirs.
# cumprod gives us a vector with 1's until we hit the first tree that is taller,
# and then 0's from there. Add up the vector and if it ended because we found a 
# taller tree, add one.
# Multiply the scenic scores for each direction.
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

#Convert the matrix to an integer vector and find the max.
sce_mat %>% as.list %>% unlist %>% max


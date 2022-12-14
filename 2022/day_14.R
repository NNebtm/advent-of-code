# December 14, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(14, 2022)
pzl_test <- c("498,4 -> 498,6 -> 496,6",
              "503,4 -> 502,4 -> 502,9 -> 494,9")
library(purrr)

#Find all corners of the rock formations and fill out the spaces in between
form_rock <- function(line) {
  corners <- strsplit(line, " -> ")
  corners <- strsplit(corners %>% unlist, ",")
  corners <- map(corners, as.integer)
  rock_list <- list(corners[[1]])
  for (n in 2:length(corners)) {
    dif <- corners[[n]] - corners[[n-1]]
    m_max <- abs(sum(dif))
    for (m in 1:m_max) {
      rock_list <- append(rock_list, list(m*dif/m_max + corners[[n-1]]))
    }
    
  }
  
  return(rock_list)
}

#Create a list of rock coordinates.s
rock_list_tot <- map(pzl_data, form_rock) %>% flatten

#Find min and max values.
x_min <- min(map_dbl(rock_list_tot, head(1)))
x_max <- max(map_dbl(rock_list_tot, head(1)))
y_min <- min(map_dbl(rock_list_tot, head(2)))
y_max <- max(map_dbl(rock_list_tot, head(2)))

#Create an empty matrix to fill in with rocks. Extra spaces needed on the 
#right-hand edge to ensure that part 2 functions.
rock_matrix <- matrix(data=0, nrow = y_max + 3, ncol = x_max+200)

#Place all rocks in the matrix
for (rock in rock_list_tot) {
  rock_matrix[rock[2] + nrow(rock_matrix)*(rock[1]-1)] <- 1
}

#Look at the top and center of the rock formation around where the sand comes out
# rock_matrix[1:20, 491:510]

#Generate function for how sand falls. The void option determines if the sand
#can fall off the bottom of the map.
sand_fall <- function(x_start, y_start, mat, void = TRUE) {
  if (y_start + 1 > y_max + 1 & void) {
    return(c(Inf, Inf))
  } else if (mat[y_start + 1, x_start] == 0) {
    return(c(x_start, y_start + 1))
  } else if (mat[y_start + 1, x_start - 1] == 0) {
    return(c(x_start - 1, y_start + 1))
  } else if (mat[y_start + 1, x_start + 1] == 0) {
    return(c(x_start + 1, y_start + 1))
  } else {
    return(c(x_start, y_start))
  }
}

#Find out where a grain of sand finally ends up by applying the sand_fall 
#function recursively.
sand_fall_full <- function(x_start, y_start, mat, void = TRUE) {
  coord_old <- c(x_start, y_start)
  coord_new <- sand_fall(x_start, y_start, mat, void)
  
  while (!identical(coord_new, coord_old) & all(coord_new < Inf)) {
    coord_old <- coord_new
    coord_new <- sand_fall(coord_old[1], coord_old[2], mat, void)
  }
  return(coord_new)
}

#Test where the first grain lands
#sand_fall_full(500, 0, rock_matrix)

#Create a copy of the rock matrix where the sand will fit.
sand_matrix <- rock_matrix
n_max <- (y_max + 2*x_max + 2) 
#Repeatedly drop grains of sand. I'm using a for loop rather than a while loop
#to ensure that it will terminate.
for (n in 1:n_max) {
  new_sand <- sand_fall_full(500, 0, sand_matrix)
  if (all(new_sand == c(Inf, Inf))) {
    print("inf")
    break
  } else if (all(new_sand == c(500, 0))) {
    print("same")
    break
  } else {
    sand_matrix[new_sand[2] + nrow(sand_matrix)*(new_sand[1]-1)] <- 2
  }
  
  if (n == n_max) {
    print(new_sand)
  }
}

#Check out the area around the point of entry
# sand_matrix[1:20, 491:510]
# 
# sand_matrix[1:10, 491:505]

#count all the grains of sand.
which(sand_matrix == 2) %>% length #1061

#part 2
rock_matrix_2 <- rock_matrix

#Apply the rock floor
rock_matrix_2[y_max + 2, 1:ncol(rock_matrix_2)] <- 1

#Run the same loop. The inf-option is not needed here, but it also does not harm.
sand_matrix_2 <- rock_matrix_2
n_max <- nrow(sand_matrix_2)*ncol(sand_matrix_2)
for (n in 1:n_max) {
  # print(n)
  new_sand <- sand_fall_full(500, 0, sand_matrix_2, void = FALSE)
  if (all(new_sand == c(Inf, Inf))) {
    print("inf")
    break
  } else if (all(new_sand == c(500, 0))) {
    print("same")
    break
  } else {
    # print(new_sand)
    sand_matrix_2[new_sand[2] + nrow(sand_matrix_2)*(new_sand[1]-1)] <- 2
  }
  
  if (n == n_max) {
    print(new_sand)
  }
}

#Count all the grains of sand and add 1 for the grain at (500, 0) which is not 
#in the matrix
(which(sand_matrix_2 == 2) %>% length) + 1


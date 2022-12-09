# December 9, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(9, 2022)

# If the tail is touching the head, take no action.
# If not, we exploit the fact that since we are taking one step at a time,
# the max size of the difference between positions is 2.
t_move <- function(vec) {
  if (abs(vec[1]) <= 1 & abs(vec[2]) <= 1) {
    return(c(0,0))
  } else {
    return(janitor::round_half_up(vec/2))
  }
}

# Initialize with starting position at the origin and a list of tail-positions
hpos <- c(0,0)
tpos <- c(0,0)
tlist <- list(tpos)

for (move in pzl_data) {
  move_vec <- unlist(strsplit(move, " "))
  
  dir <- case_when(move_vec[1] == "R" ~ c(1,0),
                   move_vec[1] == "L" ~ c(-1,0),
                   move_vec[1] == "U" ~ c(0,1),
                   move_vec[1] == "D" ~ c(0,-1))
  
  for (n in 1:move_vec[2]) {
    hpos <- hpos + dir
    tpos <- tpos + t_move(hpos - tpos)
    tlist <- append(tlist, list(tpos))
  }
}
hpos
tpos
tlist %>% unique() %>% length()

# Part 2

# Now we treat the head as just another knot and keep all knot-positions in a list
knpos <- rep(list(c(0,0)),10)
t9list <- list(knpos[[10]])

for (move in pzl_data) {
  move_vec <- unlist(strsplit(move, " "))
  
  dir <- case_when(move_vec[1] == "R" ~ c(1,0),
                   move_vec[1] == "L" ~ c(-1,0),
                   move_vec[1] == "U" ~ c(0,1),
                   move_vec[1] == "D" ~ c(0,-1))
  
  # We need to treat the head slightly differently from the others since that is
  # the one driving the movement, but the rest just treat the knot before it as 
  # the head from before.
  for (n in 1:move_vec[2]) {
    knpos[[1]] <- knpos[[1]] + dir
    for (n in 2:length(knpos)) {
      knpos[[n]] <- knpos[[n]] + t_move(knpos[[n-1]] - knpos[[n]])
    }
    t9list <- append(t9list, list(knpos[[10]]))
  }
}
t9list %>% unique() %>% length()
findElbowPoint <- function(wss) {
  if (length(wss) <= 1) {
    # Not enough points to find an elbow; return NA or an appropriate value
    return(NA)
  }
  
  # Prepare the data for distance calculation
  n <- length(wss)
  k <- 1:n
  plot_points <- matrix(c(k, wss), ncol = 2)
  
  # The line from the first to the last point
  start_point <- plot_points[1, ]
  end_point <- plot_points[n, ]
  
  # Function to calculate the distance of each point from the line
  line_distance <- function(point, start_point, end_point) {
    A <- end_point[2] - start_point[2]
    B <- start_point[1] - end_point[1]
    C <- end_point[1] * start_point[2] - start_point[1] * end_point[2]
    
    abs(A * point[1] + B * point[2] + C) / sqrt(A^2 + B^2)
  }
  
  # Calculate distances from the line for all points
  distances <- apply(plot_points, 1, function(point) line_distance(point, start_point, end_point))
  
  # Find the index of the maximum distance
  elbow_index <- which.max(distances)
  
  # If elbow_index is NA, then return NA or handle it as needed
  if (is.na(elbow_index)) {
    return(NA)
  }
  
  return(elbow_index)
}

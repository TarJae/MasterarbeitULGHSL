findElbowPoint <- function(wss) {
  if (length(wss) <= 1) {
    # Nicht genügend Punkte, um einen Knick zu finden; NA oder einen entsprechenden Wert zurückgeben
    return(NA)
  }
  
  # Vorbereitung der Daten für die Distanzberechnung
  n <- length(wss)
  k <- 1:n
  plot_points <- matrix(c(k, wss), ncol = 2)
  
  # Die Linie vom ersten bis zum letzten Punkt
  start_point <- plot_points[1, ]
  end_point <- plot_points[n, ]
  
  # Funktion zur Berechnung der Distanz jedes Punktes von der Linie
  line_distance <- function(point, start_point, end_point) {
    A <- end_point[2] - start_point[2]
    B <- start_point[1] - end_point[1]
    C <- end_point[1] * start_point[2] - start_point[1] * end_point[2]
    
    abs(A * point[1] + B * point[2] + C) / sqrt(A^2 + B^2)
  }
  
  # Berechnung der Distanzen zur Linie für alle Punkte
  distances <- apply(plot_points, 1, function(point) line_distance(point, start_point, end_point))
  
  # Finden des Index der maximalen Distanz
  elbow_index <- which.max(distances)
  
  # Wenn elbow_index NA ist, dann NA zurückgeben oder entsprechend behandeln
  if (is.na(elbow_index)) {
    return(NA)
  }
  
  return(elbow_index)
}

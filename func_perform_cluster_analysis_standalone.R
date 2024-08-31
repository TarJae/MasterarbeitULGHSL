perform_cluster_analysis_standalone <- function(procedure_name = NULL, 
                                                op_zugang_filter = NULL, 
                                                seite_filter = NULL, 
                                                details_filter = NULL, 
                                                ein_mehr_filter = NULL, 
                                                data) {
  
  # Daten filtern basierend auf den bereitgestellten Filtern
  gefilterte_daten <- data %>%
    filter(if (!is.null(procedure_name)) eingrbez_2 == procedure_name else TRUE) %>%
    filter(if (length(op_zugang_filter) > 0) op_zugang %in% op_zugang_filter else TRUE) %>%
    filter(if (length(seite_filter) > 0) Seite %in% seite_filter else TRUE) %>%
    filter(if (length(details_filter) > 0) details %in% details_filter else TRUE) |>
    filter(if (length(ein_mehr_filter) > 0) ein_mehr %in% ein_mehr_filter else TRUE)
  
  # Initialisierung der Variablen für spätere Nutzung
  scree_plot <- NULL
  swarm_plot <- NULL
  cluster_stats <- tibble()
  optimal_k <- NA
  
  # Überprüfung, ob gefilterte Daten vorhanden sind
  if (nrow(gefilterte_daten) > 0) {
    set.seed(123)  # Setzen des Zufallszahlengenerators für Reproduzierbarkeit
    
    # Anzahl der einzigartigen Werte in der Variable "schnitt_naht_minuten"
    unique_data_points <- length(unique(gefilterte_daten$schnitt_naht_minuten))
    
    # Wenn es zu wenige einzigartige Datenpunkte gibt, setze alle Daten in einen Cluster
    if (unique_data_points <= 5) {
      gefilterte_daten$cluster <- rep(1, nrow(gefilterte_daten))
      optimal_k <- 1
    } else {
      # Bestimmen der maximalen Anzahl von Clustern basierend auf den Daten
      max_k <- min(unique_data_points - 1, 10)
      
      # Berechnen der Summe der quadratischen Abweichungen innerhalb der Cluster für verschiedene Werte von k
      wss <- sapply(1:max_k, function(k) {
        tryCatch(kmeans(gefilterte_daten$schnitt_naht_minuten, k, nstart = 10)$tot.withinss, error = function(e) NA)
      })
      
      # Entfernen ungültiger WSS-Werte
      valid_wss <- na.omit(wss)
      
      # Wenn mehr als ein gültiger WSS-Wert vorhanden ist, bestimme das optimale k mittels Elbow-Methode
      if (length(valid_wss) > 1) {
        optimal_k <- findElbowPoint(valid_wss)
        kmeans_result <- kmeans(gefilterte_daten$schnitt_naht_minuten, centers = optimal_k, nstart = 10)
        gefilterte_daten$cluster <- kmeans_result$cluster
      } else {
        # Wenn keine Elbow-Methode angewendet werden kann, setze alle Daten in einen Cluster
        gefilterte_daten$cluster <- rep(1, nrow(gefilterte_daten))
        optimal_k <- 1
      }
    }
    
    # Generierung von Cluster-Statistiken
    cluster_stats1 <- gefilterte_daten %>%
      group_by(cluster) %>%
      summarise(
        Anzahl = n(),
        Mean = mean(schnitt_naht_minuten),
        Median = median(schnitt_naht_minuten),
        `25th Percentile` = quantile(schnitt_naht_minuten, probs = 0.25),
        `75th Percentile` = quantile(schnitt_naht_minuten, probs = 0.75),
        .groups = 'drop'
      ) %>%
      mutate(cluster = as.character(cluster)) %>%
      mutate(
        xmin = as.numeric(cluster) - 0.3,
        xmax = as.numeric(cluster) + 0.3
      )
    
    # Berechnung des gewichteten Mittels für die "Alle"-Gruppe
    weighted_mean <- sum(cluster_stats1$Mean * cluster_stats1$Anzahl) / sum(cluster_stats1$Anzahl)
    
    # Zusammenführen der Cluster-Statistiken in einem einzigen Dataframe
    total_stats <- tibble(
      cluster = "Alle",
      Anzahl = sum(cluster_stats1$Anzahl),
      Mean = mean(cluster_stats1$Mean),
      `Weighted Mean` = weighted_mean,
      Median = median(cluster_stats1$Median),
      `25th Percentile` = quantile(cluster_stats1$`25th Percentile`, probs = 0.25),
      `75th Percentile` = quantile(cluster_stats1$`75th Percentile`, probs = 0.75),
      xmin = case_when(
        nrow(cluster_stats1) == 1 ~ 1.7,
        nrow(cluster_stats1) == 2 ~ 2.7,
        nrow(cluster_stats1) == 3 ~ 3.7,
        nrow(cluster_stats1) == 4 ~ 4.7,
        nrow(cluster_stats1) == 5 ~ 5.7,
        nrow(cluster_stats1) == 6 ~ 6.7,
        .default = NA),
      xmax = case_when(
        nrow(cluster_stats1) == 1 ~ 2.3,
        nrow(cluster_stats1) == 2 ~ 3.3,
        nrow(cluster_stats1) == 3 ~ 4.3,
        nrow(cluster_stats1) == 4 ~ 5.3,
        nrow(cluster_stats1) == 5 ~ 6.3,
        nrow(cluster_stats1) == 6 ~ 7.3,
        .default = NA)
    )
    
    # Zusammenführen der Cluster-Statistiken und Hinzufügen der "Alle"-Gruppe
    cluster_stats <- bind_rows(cluster_stats1, total_stats) %>% 
      mutate(across(-cluster, ~round(., 1))) |> 
      rename(`Cluster Mean` = Mean, `Cluster Median`= Median)
    
    # Erstellung eines Schwarmplots
    swarm_plot <- ggplot() +
      geom_quasirandom(data = gefilterte_daten, aes(x = factor(cluster), y = schnitt_naht_minuten, color = factor(cluster)), 
                       size = ifelse(nrow(gefilterte_daten) < 200, 3, 1.5), show.legend = FALSE) +
      geom_quasirandom(data = gefilterte_daten, aes(x = factor("Alle"), y = schnitt_naht_minuten), color = "purple", alpha = 0.5, 
                       size = ifelse(nrow(gefilterte_daten) < 200, 3, 1.5)) +
      geom_segment(data = cluster_stats1, aes(x = xmin, xend = xmax, y = Mean, yend = Mean, linetype = "Mean"), 
                   color = "grey40", linewidth = 1.5) +
      geom_segment(data = cluster_stats1, aes(x = xmin, xend = xmax, y = Median, yend = Median, linetype = "Median"), 
                   color = "black", linewidth = 1.5) +
      geom_segment(data = total_stats %>% filter(cluster == "Alle"),
                   aes(x = xmin, xend = xmax, y = `Weighted Mean`, yend = `Weighted Mean`, linetype = "Weighted Mean"),
                   color = "darkorange", linewidth = 2.5) +
      geom_segment(data = total_stats %>% filter(cluster == "Alle"),
                   aes(x = xmin, xend = xmax, y = Median, yend = Median, linetype = "Median"),
                   color = "black", linewidth = 2.5) +
      geom_segment(data = total_stats %>% filter(cluster == "Alle"),
                   aes(x = xmin, xend = xmax, y = Mean, yend = Mean, linetype = "Mean"),
                   color = "grey40", linewidth = 2.5) +
      scale_color_manual(values = c("1" = "steelblue", "2" = "gold", "3" = "firebrick", "Alle" = "purple")) +
      scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dotted", "Weighted Mean" = "dotdash"))+
      labs(title = paste(procedure_name, op_zugang_filter,  seite_filter, details_filter), 
           x = "Cluster", y = "Schnitt-Naht [Minuten]", linetype = "") +
      theme_minimal() +
      tar_ggplot_font_size(7) 
    
    # Erstellung eines Scree-Plots, um die optimale Clusteranzahl zu bestimmen
    scree_plot <- if (optimal_k > 1) {
      ggplot(data.frame(k = 1:length(valid_wss), wss = valid_wss), aes(x = k, y = wss)) +
        geom_line(linewidth = 2, color = "grey") + 
        geom_point(size = 3) +
        geom_point(aes(x = optimal_k, y = valid_wss[optimal_k]), color = "red", size = 10, shape = 4) +
        labs(title = paste(procedure_name, op_zugang_filter,  seite_filter, details_filter), x = "Cluster Anzahl", y = "Summe der quadratischen Abweichungen \n innerhalb der Cluster") +
        scale_x_continuous(breaks = 1:10, limits = c(1, 10))+
        theme_minimal()+
        tar_ggplot_font_size(5)+
        theme(legend.position = "none")
    } else {
      ggplot() +
        geom_text(aes(x = 1, y = 0, label = "Nicht genügend einzigartige Datenpunkte für die Clusterbildung"), hjust = 0.5, vjust = 0.5) +
        labs(title = paste(procedure_name, op_zugang_filter, seite_filter, details_filter, ein_mehr_filter), x = "", y = "") +
        theme_minimal()+
        tar_ggplot_font_size(7)
      theme(legend.position = "none")
    }
  }
  
  # Rückgabe der aktualisierten gefilterten Daten mit Cluster-Zuordnungen und anderen Ergebnissen
  return(list(
    data = cluster_stats,
    swarm_plot = swarm_plot,
    scree_plot = scree_plot, 
    optimal_k = optimal_k, 
    filtered_data_with_clusters = filtered_data
  ))
}

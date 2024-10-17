library(BoolNet)
# Funktion zur Erstellung einer Wahrheitstabelle für ein zufälliges Netzwerk
create_truth_tables_boolnet <- function (number_of_genes_in_network, number_of_inputs_for_transition, fixed_poison_zeta){
  if (fixed_poison_zeta == 0){
    topology <- "fixed"
  } else if(fixed_poison_zeta == 1){
    topology <- "homogeneous"
    
  } else{
    topology <- "scale_free"
  }
  
  # Debugging: Überprüfe die Werte
  print(paste("Gene:", number_of_genes_in_network))
  print(paste("Inputs:", number_of_inputs_for_transition))
  print(paste("Topology:", topology))
  
  # Erzeuge ein zufälliges Genregulatorisches Netzwerk
  network <- generateRandomNKNetwork(number_of_genes_in_network, number_of_inputs_for_transition, topology)
  
  # Extrahiere die Interaktionen des Netzwerks
  network_interactions <- network$interactions
  print(network_interactions)
  
  # Funktion, um für ein einzelnes Gen die Wahrheitstabelle zu erstellen
  create_table_for_gene <- function(gene_name){
    # Indizes der Eingabegene für das aktuelle Gen
    input_indices <- network_interactions[[gene_name]]$input
    
    # Funktionswerte für die Wahrheitstabelle
    func_values <- network_interactions[[gene_name]]$func
    
    # Erstelle alle möglichen Kombinationen der Eingangsvariablen
    possible_combinations <- expand.grid(replicate(length(input_indices), c(0, 1), simplify = FALSE))
    
    # Füge die Funktionswerte zur Tabelle hinzu
    truth_table <- cbind(possible_combinations, y = func_values)
    
    # Setze die Spaltennamen auf "x" gefolgt von den Indizes der Eingabegene
    colnames(truth_table) <- c(paste0("x", input_indices), "y")
    
    # Konvertiere das Data-Frame in eine Matrix
    truth_table_matrix <- as.matrix(truth_table)
    
    return(truth_table_matrix)
  }
  
  # Erzeuge die Wahrheitstabellen für alle Gene in `network_interactions` mit `lapply`
  truth_tables <- lapply(names(network_interactions), create_table_for_gene)
  names(truth_tables) <- names(network_interactions) # Namen zuweisen ------ nacharbeiten
  
  return(truth_tables)
}


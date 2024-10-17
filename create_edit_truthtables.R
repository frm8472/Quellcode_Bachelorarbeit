
#' Funktion zum Erstellend der Wahrheitstabelle
#' 
#' @param count_variables 
#' @param count_trues 
#' @return 

create_truthtables <- function(count_variables, count_trues, fixed_or_limit) {
  
  # Anzahl der Zeilen (2^anzahl_variablen Kombinationen)
  number_rows <- 2^count_variables
  
  # Erstellen Sie alle moeglichen Kombinationen von Wahrheitswerten
  table <- expand.grid(rep(list(c(0, 1)), count_variables))
  
  # Konvertieren Sie den data.frame in eine Matrix
  matrix_table1 <- as.matrix(table)
  
  # Namen für die Spalten setzen (x_1, ...)
  colnames(matrix_table1) <- paste0("x", 1:count_variables)
  
  # Initialisieren der Matrix mit 0en
  matrix_table2 <- matrix(0, nrow = number_rows, ncol = count_variables)
  
  # Generierung von Matrix 2: Für jede Zeile zufällige Anzahl von 1en setzen
  for (row in 1:number_rows) {
    
    # Zufaellige Anzahl der Einsen für die aktuelle Zeile
    if (fixed_or_limit) {
      # Wenn fixed_or_limit TRUE ist, soll count_trues als feste Anzahl interpretiert werden
      num_ones <- count_trues
    } else {
      # Wenn fixed_or_limit FALSE ist, soll count_trues als maximale Obergrenze interpretiert werden
      num_ones <- sample(0:count_trues, 1)
    }
    
    # Zufällige Positionen auswählen, an denen 1en gesetzt werden
    if (num_ones > 0) {
      ones_positionen <- sample(1:count_variables, num_ones)
      
      # An den zufälligen Positionen 1en setzen
      matrix_table2[row, ones_positionen] <- 1
    }
  }
  
  # Namen für die Spalten setzen (y_1, ...)
  colnames(matrix_table2) <- paste0("y", 1:count_variables)
  
  output <- cbind(matrix_table1, matrix_table2)
  
  return(output)
}



delete_rows <- function(matrix_table, delete_prob) {
  
  # Anzahl der Zeilen und Spalten in der Matrix
  number_rows <- nrow(matrix_table)
  count_variables <- (ncol(matrix_table)) / 2
  
  # Festlegen der Indizes für die y-Spalten
  y_columns <- (count_variables + 1):(2 * count_variables)
  
  # Zufällige Auswahl der Zeilen, in denen y-Werte gelöscht werden sollen
  delete_flags <- runif(number_rows) < (delete_prob / 100)
  
  # Erstellen einer Kopie der Matrix, die modifiziert wird
  modified_matrix <- matrix_table
  
  # Setze NA in den y-Spalten der ausgewählten Zeilen
  modified_matrix[delete_flags, y_columns] <- NA
  
  return(modified_matrix)
}










#' Funktion zum Erstellend der Wahrheitstabelle
#' 
#' @param count_variables Die erste Zahl
#' @param count_trues Die zweite Zahl
#' @return Die Summe von x und y

create_truthtables <- function(count_variables, count_trues) {
  
  # Anzahl der Zeilen (2^anzahl_variablen Kombinationen)
  number_rows <- 2^count_variables
  
  # Erstellen Sie alle moeglichen Kombinationen von Wahrheitswerten
  table <- expand.grid(rep(list(c(0, 1)), count_variables))
  
  # Konvertieren Sie den data.frame in eine Matrix
  matrix_table1 <- as.matrix(table)
  
  # Namen für die Spalten setzen (x_1, ...)
  colnames(matrix_table1) <- paste0("x_", 1:count_variables)
  
  # Initialisieren der Matrix mit 0en
  matrix_table2 <- matrix(0, nrow = number_rows, ncol = count_variables)
  
  # Generierung von Matrix 2: Für jede Zeile zufaellige Anzahl von 1en (max. count_trues) setzen
  for (row in 1:number_rows) {
    
    # Zufaellige Anzahl der Einsen für die aktuelle Zeile (0 bis count_trues)
    num_ones <- sample(0:count_trues, 1)
    
    # Zufaellige Positionen auswaehlen, an denen 1en gesetzt werden
    if (num_ones > 0) {
      ones_positionen <- sample(1:count_variables, num_ones)
      
      # An den zufaelligen Positionen 1en setzen
      matrix_table2[row, ones_positionen] <- 1
    }
  }
  
  # Namen für die Spalten setzen (y_1, ...)
  colnames(matrix_table2) <- paste0("y_", 1:count_variables)
  
  
  output = (list(truthtable1 = matrix_table1, truthtable2 = matrix_table2))
  
  return(output)
}


# Funktion zur Benutzerinteraktion
user_interaction <- function() {
  
  # Funktion zur Validierung der Eingaben
  validate_inputs <- function(count_variables, count_trues) {
    # Überpruefe nur den Wert, der tatsaechlich übergeben wird
    if (!is.null(count_variables) && (is.na(count_variables) || count_variables <= 0)) {
      return(FALSE)
    }
    if (!is.null(count_trues) && (is.na(count_trues) || count_trues < 0)) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Benutzer nach der Anzahl der Variablen fragen
  repeat {
    count_variables <- as.integer(readline(prompt = "Select the number of variables (positive integer): "))
    if (validate_inputs(count_variables, NULL)) {
      break
    } else {
      cat("Error: Invalid input for the number of variables. Please enter a positive integer.\n")
    }
  }
  
  # Schleife, um sicherzustellen, dass der Wert für count_trues korrekt ist
  repeat {
    count_trues <- as.integer(readline(prompt = "Select the number of true-values per state (non-negative integer): "))
    if (validate_inputs(NULL, count_trues) && count_trues <= count_variables) {
      break  # Verlasse die Schleife, wenn die Bedingung erfuellt ist
      
      
    } else {
      if (is.na(count_trues) || count_trues < 0) {
        cat("Error: The number of true-values must be a non-negative integer.\n")
      } else {
        cat("Error: The number of true-values must be less than or equal to the number of variables. Please try again.\n")
      }
    }
  }
  
  # Rueckgabe der validierten Werte
  return(list(count_variables = count_variables, count_trues = count_trues))
}

# Funktion zum zufaelligen Loeschen von Zeilen in matrix_table2
delete_rows <- function(matrix_table2, delete_prob) {
  
  # Anzahl der Zeilen in der Matrix
  number_rows <- nrow(matrix_table2)
  
  # Ueberpruefen, ob die Loeschwahrscheinlichkeit gueltig ist
  if (delete_prob < 0 || delete_prob > 100) {
    stop("The deletion probability must be between 0 and 100.")
  }
  
  # Zufaellige Auswahl der Zeilen, die geloescht werden sollen
  delete_flags <- runif(number_rows) < (delete_prob/100)
  
  # Erstellen einer Kopie der Matrix für die Ausgabe
  modified_matrix <- matrix_table2
  
  # Ersetzen der Werte in den zu loeschenden Zeilen durch X
  modified_matrix[delete_flags, ] <- NA
  
  return(modified_matrix)
}

# Benutzerinteraktion durchfuehren und die Werte abrufen
user_inputs <- user_interaction()

# Anzeigen der erhaltenen Werte
cat("Number of variables:", user_inputs$count_variables, "\n")
cat("Number of true-values per state:", user_inputs$count_trues, "\n")

# Wahrheitstabelle als Matrix erstellen und anzeigen
list_of_truthtables <- create_truthtables(user_inputs$count_variables, user_inputs$count_trues)
print(truthtable)

print("Matrix 1:")
print(list_of_truthtables$truthtable1)

print("Matrix 2:")
print(list_of_truthtables$truthtable2)


# Definiere eine Loeschwahrscheinlichkeit
delete_prob <- as.numeric(readline(prompt = "Enter the probability of deleting a row in percent (0 to 100): "))

# Loesche einige Zeilen basierend auf der Wahrscheinlichkeit
modified_matrix <- delete_rows(list_of_truthtables$truthtable2, delete_prob)

# Matrix 2 nach der Loeschung anzeigen
cat("Matrix 2 after deletion:\n")
print(modified_matrix)







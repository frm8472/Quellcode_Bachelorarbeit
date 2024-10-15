# Lösche alle Variablen in der Umgebung
rm(list = ls())


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
  colnames(matrix_table1) <- paste0("x_", 1:count_variables)
  
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
  colnames(matrix_table2) <- paste0("y_", 1:count_variables)
  
  output = list(truthtable1 = matrix_table1, truthtable2 = matrix_table2)
  
  return(output)
}


# Funktion zur Benutzerinteraktion
user_interaction <- function() {
  
  # Benutzer nach der Anzahl der Variablen fragen
  repeat {
    count_variables <- as.integer(readline(prompt = "Select the number of variables (positive integer): "))
    
   # if (!is.na(count_variables) && count_variables > 0) {
    if(!is.na(count_variables) && count_variables > 0){
      break
    } else {
      cat("Error: Invalid input for the number of variables. Please enter a positive integer.\n")
      cat("You entered:", count_variables, "\nPlease try again.\n")
    }
  }
  
  # Schleife, um sicherzustellen, dass der Wert für count_trues korrekt ist
  repeat {
    count_trues <- as.integer(readline(prompt = "Select the number of true-values per state (non-negative integer): "))
    if (!is.na(count_trues) && count_trues >= 0 && count_trues <= count_variables) {
      break  # Verlasse die Schleife, wenn die Bedingung erfüllt ist
    } else {
      if (is.na(count_trues) || count_trues < 0) {
        cat("Error: The number of true-values must be a non-negative integer.\n")
      } else {
        cat("Error: The number of true-values must be less than or equal to the number of variables.\n")
        cat("You entered:", count_variables, "\nPlease try again.\n")
      }
    }
  }
  repeat {
    fix_or_limit <- tolower(readline(prompt = "Should count_trues be a fixed value (f) or a upper limit (l)? (Enter 'f' or 'l'): "))
    
    # Überprüfung der Eingabe für fix_or_variable
    if (fix_or_limit %in% c("f")) {
      fix_or_limit <- TRUE
      break  # Schleife verlassen, wenn die Eingabe gültig ist
    } else if (fix_or_limit %in% c("l")) {
      fix_or_limit <- FALSE
      break  # Schleife verlassen, wenn die Eingabe gültig ist
    } else {
      cat("Error: Please enter 'f' for fixed or 'l' for variable.\n")
      cat("You entered:", fix_or_limit, "\nPlease try again.\n")
    }
  }
  
  
  
  # Rueckgabe der validierten Werte
  return(list(count_variables = count_variables, count_trues = count_trues, fix_or_limit = fix_or_limit))
}


# Funktion zum zufaelligen Loeschen von Zeilen in matrix_table2
delete_rows <- function(matrix_table2, delete_prob) {
  
  # Anzahl der Zeilen in der Matrix
  number_rows <- nrow(matrix_table2)
  
  # Ueberpruefen, ob die Loeschwahrscheinlichkeit gueltig ist
  repeat {
    # Definiere eine Loeschwahrscheinlichkeit
    delete_prob <- as.numeric(readline(prompt = "Enter the probability of deleting a row in percent (0 to 100): "))
    if (delete_prob > 0 && delete_prob < 100) {
      break
    } else {
      cat("Error: The deletion probability must be between 0 and 100.\n")
      cat("You entered:", delete_prob, "\nPlease try again.\n")
    }
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
cat("Number of true-values per state is fixed (f) or a limit (l): ", user_inputs$fix_or_limit)

# Wahrheitstabelle als Matrix erstellen und anzeigen
list_of_truthtables <- create_truthtables(user_inputs$count_variables, user_inputs$count_trues,user_inputs$fix_or_limit )

# Loesche einige Zeilen basierend auf der Wahrscheinlichkeit
modified_matrix <- delete_rows(list_of_truthtables$truthtable2, delete_prob)


setwd("D:\\Repositorys\\Truthtables")

# Save Truthtables in csv-files
write.csv(list_of_truthtables$truthtable1, file = "truthtable_x.csv", row.names = TRUE)
write.csv(list_of_truthtables$truthtable2, file = "truthtable_y.csv", row.names = TRUE)
write.csv(modified_matrix, file = "truthtable_modified.csv", row.names = TRUE)

write.table(list_of_truthtables$truthtable1, file = "truthtable_x.txt", sep = "\t", row.names = TRUE, col.names = TRUE)
write.table(list_of_truthtables$truthtable2, file = "truthtable_y.txt", sep = "\t", row.names = TRUE, col.names = TRUE)
write.table(modified_matrix, file = "truthtable_modified.txt", sep = "\t", row.names = TRUE, col.names = TRUE)

# Print-Ausgaben
print(list_of_truthtables)

print("Matrix 1:")
print(list_of_truthtables$truthtable1)

print("Matrix 2:")
print(list_of_truthtables$truthtable2)
# Matrix 2 nach der Loeschung anzeigen
cat("Matrix 2 after deletion:\n")
print(modified_matrix)




generate_logical_formula <- function(n, k, v) {
  # Definiere die verfügbaren Variablen
  variables <- LETTERS[1:v]  # Z. B. A, B, C, ..., je nach Wert von v
  formulas <- vector("list", n)
  
  for (i in 1:n) {
    # Anzahl der Variablen in der Formel
    num_vars <- sample(1:k, 1)  # 1 bis k Variablen
    
    # Wähle die Variablen ohne Wiederholung
    selected_vars <- sample(variables, num_vars, replace = FALSE)
    
    # Zufällige logische Operatoren
    operators <- c("&", "|", "!")
    formula <- ""
    
    for (var in selected_vars) {
      if (formula == "") {
        formula <- var
      } else {
        op <- sample(operators[1:2], 1)  # Nur AND und OR
        formula <- paste0("(", formula, " ", op, " ", var, ")")
      }
      
      # Füge NOT zufällig hinzu
      if (runif(1) < 0.5) {
        formula <- paste0("!", formula)
      }
    }
    
    formulas[[i]] <- formula
  }
  
  return(formulas)
}

# Beispiel: Generiere 5 zufällige logische Formeln mit max. 3 Variablen aus einem Pool von 4 Variablen

random_formulas <- generate_logical_formula(10, 3, 4)

# Ausgabe der generierten Formeln
print(random_formulas)


# Funktion zum Erstellen einer Wahrheitstabelle für boolesche Ausdrücke
create_truth_table <- function(v, n, formulas) {
  # Alle möglichen Kombinationen von Eingabewerten für v Variablen
  input_combinations <- expand.grid(rep(list(c(TRUE, FALSE)), v))
  colnames(input_combinations) <- LETTERS[1:v]
  
  # Datenrahmen für die Ausgangsvariablen vorbereiten
  output_table <- matrix(NA, nrow = nrow(input_combinations), ncol = n)
  
  for (i in 1:nrow(input_combinations)) {
    # Für jede Kombination der Eingangsvariablen
    current_values <- as.list(input_combinations[i, ])
    
    # Für jede Formel berechnen
    for (j in 1:n) {
      # Evaluierung der aktuellen Formel mit den aktuellen Eingangsvariablen
      formula_result <- eval(parse(text = formulas[[j]]), envir = current_values)
      output_table[i, j] <- formula_result
    }
  }
  
  # Kombination der Eingabe- und Ausgabe-Tabellen
  truth_table <- cbind(input_combinations, as.data.frame(output_table))
  
  # Benenne die Spalten für die Ausgabevariablen
  colnames(truth_table)[(v + 1):(v + n)] <- paste0("Output", 1:n)
  
  return(truth_table)
}

# Beispiel für die Verwendung der Funktionen
n <- 5
v <- 3
formulas <- generate_logical_formula(n, 3, v)
print(formulas)
truth_table <- create_truth_table(v, n, formulas)

# Ausgabe der generierten Wahrheitstabelle
print(truth_table)

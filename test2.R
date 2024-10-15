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

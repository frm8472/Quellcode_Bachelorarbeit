library(cli)

user_welcome <- function(){
  cli_h1("Creating random truth tables")
  
  cli_text("This is an application to generate, modify and save random truth tables.")
  
}

choose_method <- function() {
  
  cli_text("1. Select the method to generate random truth tables.")
  
  # Definiere die Methoden
  methods <- c("Method 1: Generates truth tables based on a fixed or maximum number of true-values in the states after the transition.",
               "Method 2: Generates truth tables using concept classes. In this case, each gene is influenced by a specific number of other genes.",
               "Method 3: Generates truth tables by using a random N-K network created with the Boolnet package.")
  
  # Ausgabe der Methoden mit sapply
  sapply(seq_along(methods), function(number_method) {
    cli_text("{cli::col_blue(number_method)}: {methods[number_method]}")
  })
  
  repeat {
    # Frage den Nutzer nach einer Zahl
    generating_method <- as.integer(readline(prompt = cli_text("Please select a {cli::col_blue('method')} by typing the {cli::col_blue('number (1 - 3)')}: ")))
    
   
    
    # Überprüfe, ob die Konvertierung erfolgreich war und ob die Eingabe gültig ist
    if (!is.na(generating_method) && generating_method > 0 && generating_method < 4) {
      cli_alert_success("You chose {cli::col_blue('method ', generating_method)}!")
      break  # Verlasse die Schleife nach einer erfolgreichen Auswahl
    } else {
      cli_alert_danger("You have not selected an available method. Please try again.")
    }
  }
  return (generating_method)
}


choose_parameters_method1 <- function(){
  # Benutzer nach der Anzahl der Variablen fragen
  repeat {
    nodes_in_network <- as.integer(readline(prompt = cli_text("Select the number of nodes in the network (positive integer): ")))
  
    if(!is.na(nodes_in_network) && nodes_in_network > 0){
      cli_alert_success("You chose {cli::col_blue(nodes_in_network)} nodes for the network!")
      break
    } else {
      cli_alert_danger("You entered {cli::col_blue(nodes_in_network)}. This is not a valid input.\nPlease try again.")
    }
  }
  repeat {
    fix_or_limit <- tolower(readline(prompt = cli_text("Should the number of true-values per state be a fixed value (f) or a maximum limit (l)? (Enter 'f' or 'l'): ")))
    
    # Überprüfung der Eingabe für fix_or_variable
    if (fix_or_limit %in% c("f")) {
      fix_or_limit <- TRUE
      cli_alert_success("You chose the number of true-values per state to be a fixed value!")
      break  # Schleife verlassen, wenn die Eingabe gültig ist
    } else if (fix_or_limit %in% c("l")) {
      fix_or_limit <- FALSE
      cli_alert_success("You chose the number of true-values per state to be a maximum limit!")
      break  # Schleife verlassen, wenn die Eingabe gültig ist
    } else {
      cli_alert_danger("You entered {cli::col_blue(fix_or_limit)}. This is not a valid input.\nPlease try again.")
    }
  }
  
  
  # Schleife, um sicherzustellen, dass der Wert für count_trues korrekt ist
  repeat {
    count_trues_after_transation <- as.integer(readline(prompt = cli_text("Select the number of true-values per state : ")))
    if (!is.na(count_trues_after_transation) && count_trues_after_transation >= 0 && count_trues_after_transation <= nodes_in_network) {
      if (fix_or_limit == TRUE){
        cli_alert_success("You chose exactly {cli::col_blue(count_trues_after_transation)} true-values per state!")
      } else{
        cli_alert_success("You chose a maximum of {cli::col_blue(count_trues_after_transation)} true-values per state!")
      }
      
      break 
      
    } else {
      if (is.na(count_trues_after_transation) || count_trues_after_transation < 0) {
        cli_alert_danger("You entered {cli::col_blue(count_trues_after_transation)}. This is not a valid input.\nThe number of true-values must be a positive integer.\nPlease try again.")
        
      } else {
        cli_alert_danger("You entered {cli::col_blue(count_trues_after_transation)}. This is not a valid input.\nThe number of true-values must be less than or equal to the number of variables.\nPlease try again.")
      }
    }
  }
  
  # Rueckgabe der validierten Werte
  return(list(nodes_in_network = nodes_in_network, fix_or_limit = fix_or_limit, count_trues_after_transation = count_trues_after_transation))
}

choose_parameters_method2 <- function(){
  # Benutzer nach der Anzahl der Variablen fragen
  repeat {
    nodes_in_network <- as.integer(readline(prompt = cli_text("Select the total number of genes in the network: ")))
    
    if(!is.na(nodes_in_network) && nodes_in_network > 0){
      cli_alert_success("You chose {cli::col_blue(nodes_in_network)} nodes for the network!")
      break
    } else {
      cli_alert_danger("You entered {cli::col_blue(nodes_in_network)}. This is not a valid input.\nPlease try again.")
    }
  }
  repeat {
    fix_or_limit <- tolower(readline(prompt = cli_text("Should the number of genes per transition be a fixed value (f) or a maximum limit (l)? (Enter 'f' or 'l'): ")))
    
    # Überprüfung der Eingabe für fix_or_variable
    if (fix_or_limit %in% c("f")) {
      fix_or_limit <- TRUE
      cli_alert_success("You chose the number of number of genes per transition to be a fixed value!")
      break  # Schleife verlassen, wenn die Eingabe gültig ist
    } else if (fix_or_limit %in% c("l")) {
      fix_or_limit <- FALSE
      cli_alert_success("You chose the number of number of genes per transition to be a maximum limit!")
      break  # Schleife verlassen, wenn die Eingabe gültig ist
    } else {
      cli_alert_danger("You entered {cli::col_blue(fix_or_limit)}. This is not a valid input.\nPlease try again.")
    }
  }
  
  # Schleife, um sicherzustellen, dass der Wert für count_trues korrekt ist
  repeat {
    number_genes_input_transition <- as.integer(readline(prompt = cli_text("Select the number of genes for the input of the transition function: ")))
    if (!is.na(number_genes_input_transition) && number_genes_input_transition >= 0 && number_genes_input_transition <= nodes_in_network) {
      if (fix_or_limit == TRUE){
        cli_alert_success("You chose exactly {cli::col_blue(number_genes_input_transition)} genes for the input per transition function!")
      } else{
        cli_alert_success("You chose a maximum of {cli::col_blue(number_genes_input_transition)} genes for the input per transition function!")
      }
      break 
      
    } else {
      if (is.na(number_genes_input_transition) || number_genes_input_transition < 0) {
        cli_alert_danger("You entered {cli::col_blue(number_genes_input_transition)}. This is not a valid input.\nThe number of genes must be a positive integer.\nPlease try again.")
        
      } else {
        cli_alert_danger("You entered {cli::col_blue(number_genes_input_transition)}. This is not a valid input.\nThe number of genes must be less than or equal to the total number of genes\nPlease try again.")
      }
    }
  }
  
  # Rueckgabe der validierten Werte
  return(list(nodes_in_network = nodes_in_network, fix_or_limit = fix_or_limit, number_genes_input_transition = number_genes_input_transition))
}


choose_parameters_method3 <- function(){
  # Benutzer nach der Anzahl der Variablen fragen
  repeat {
    nodes_in_network <- as.integer(readline(prompt = cli_text("Select the total number of genes in the network: ")))
    
    if(!is.na(nodes_in_network) && nodes_in_network > 0){
      cli_alert_success("You chose {cli::col_blue(nodes_in_network)} nodes for the network!")
      break
    } else {
      cli_alert_danger("You entered {cli::col_blue(nodes_in_network)}. This is not a valid input.\nPlease try again.")
    }
  }
  repeat {
    fixed_poison_zeta <- tolower(readline(prompt = cli_text("Should the number of genes per transition be a fixed value (f), drawn from a Piosson distribution (p) or a Zeta-distribution (z) a mean value (mean)? (Enter 'f', 'p' or 'z': ")))
    
    # Überprüfung der Eingabe für fix_or_variable
    if (fixed_poison_zeta %in% c("f")) {
      fixed_poison_zeta <- 0
      cli_alert_success("You chose the number of genes per transition to be a fixed value!")
      break  # Schleife verlassen, wenn die Eingabe gültig ist
    } else if (fixed_poison_zeta %in% c("p")) {
      fixed_poison_zeta <- 1
      cli_alert_success("You chose the  number of genes per transition to be drawn from a Poisson distribution!")
      break  # Schleife verlassen, wenn die Eingabe gültig ist
    } else if (fixed_poison_zeta %in% c("z")) {
      fixed_poison_zeta <- 2
      cli_alert_success("You chose the number of genes per transition to be drawn from a Zeta distribution!")
      break  # Schleife verlassen, wenn die Eingabe gültig ist
    } else {
      cli_alert_danger("You entered {cli::col_blue(fixed_poison_zeta)}. This is not a valid input.\nPlease try again.")
    }
  }
  
  # Schleife, um sicherzustellen, dass der Wert für count_trues korrekt ist
  repeat {
    number_genes_input_transition <- as.integer(readline(prompt = cli_text("Select the number of genes for the input of the transition function: ")))
    if (!is.na(number_genes_input_transition) && number_genes_input_transition >= 0 && number_genes_input_transition <= nodes_in_network) {
      if (fixed_poison_zeta == 0){
        cli_alert_success("You chose exactly {cli::col_blue(number_genes_input_transition)} genes for the input per transition function!")
      } else{
        cli_alert_success("You chose a mean of {cli::col_blue(number_genes_input_transition)} genes for the input per transition function!")
      }
      break 
      
    } else {
      if (is.na(number_genes_input_transition) || number_genes_input_transition < 0) {
        cli_alert_danger("You entered {cli::col_blue(number_genes_input_transition)}. This is not a valid input.\nThe number of genes must be a positive integer.\nPlease try again.")
        
      } else {
        cli_alert_danger("You entered {cli::col_blue(number_genes_input_transition)}. This is not a valid input.\nThe number of genes must be less than or equal to the total number of genes\nPlease try again.")
      }
    }
  }
  
  # Rueckgabe der validierten Werte
  return(list(nodes_in_network = nodes_in_network, fixed_poison_zeta = fixed_poison_zeta, number_genes_input_transition = number_genes_input_transition))
}



choose_deleting_probability <- function() {
  repeat {
    # Frage den Nutzer nach der Löschwahrscheinlichkeit
    delete_prob <- as.integer(readline(prompt = cli_text("Enter the probability of deleting values in output-columns in percent (0 to 100): ")))
    
    # Überprüfe die Eingabe
    if (delete_prob >= 0 && delete_prob <= 100) {
      cli_alert_success("You chose a deletion probability of {cli::col_blue(delete_prob)}%!")
      break
    } else {
      cli_alert_danger("You entered {cli::col_blue(delete_prob)}. This is not a valid input.\nThe probability must be between 0 to 100 percent.\nPlease try again.")
    }
  }
  return(delete_prob)
}


user_interaction <- function (){
  user_welcome()
  
  selected_method <- choose_method()
  
  if (selected_method == 1){
    selected_parameters <- choose_parameters_method1()
  }
  else if (selected_method == 2){
    selected_parameters <- choose_parameters_method2()
  } else{
    selected_parameters <- choose_parameters_method3()
  }
  
  deleting_probability <- choose_deleting_probability()
  
  output <- list(selected_method = selected_method,
                 deleting_probability = deleting_probability,
                 parameters = selected_parameters)
 
  return (output)
  
}
result <- user_interaction()
print(result)

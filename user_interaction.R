# Load the 'cli' package for enhanced command-line interface features.
library(cli)

#' This function gives the user a short introduction about the application.
#'
#' @return: No return value, called for printing to the console.

user_welcome <- function(){
  # Print a main header.
  cli_h1("Creating random truth tables")
  
  # Print a brief description of what the application does.
  cli_text("This is an application to generate, modify and save random truth tables.")
  
}

#' Prompt the user to select a method for generating random truth tables.
#'
#' This function displays a list of methods for generating truth tables and allows the user 
#' to select one by typing the corresponding number. It will continue to prompt the user until 
#' a valid selection is made.
#'
#'@return Integer, the selected method number (1, 2, or 3).


choose_method <- function() {
  
  # Display the main prompt message for selecting a method.
  cli_text("1. Select the method to generate random truth tables.")
  
  # Define the methods in a vector for easy iteration.
  methods <- c("Method 1: Generates truth tables based on a fixed or maximum number of true-values in the states after the transition.",
               "Method 2: Generates truth tables using concept classes. In this case, each gene is influenced by a specific number of other genes.",
               "Method 3: Generates truth tables by using a random N-K network created with the Boolnet package.")
  
  # Display each method option with a number prefix using sapply().
  sapply(seq_along(methods), function(number_method) {
    cli_text("{cli::col_blue(number_method)}: {methods[number_method]}")
  })
  
  # Start a loop until the user makes a valid selection for the method number.
  repeat {
    # Prompt the user to select a method by typing a number (1 - 3).
    generating_method <- as.integer(readline(prompt = cli_text("Please select a {cli::col_blue('method')} by typing the {cli::col_blue('number (1 - 3)')}: ")))
    
    # Check if the user's input is valid (a integer within 1 through 3).
    if (!is.na(generating_method) && generating_method > 0 && generating_method < 4) {
      
      # Display a success message and break the loop if the selection is valid.
      cli_alert_success("You chose {cli::col_blue('method ', generating_method)}!")
      break 
      
      # Alert the user that the input was invalid and prompt to try again.
    } else {
      cli_alert_danger("You have not selected an available method. Please try again.")
    }
  }
  # Return the user's selected method number.
  return (generating_method)
}

#' Prompt the user to select parameters for method 1 of truth table generation.
#'
#' This function interacts with the user to select the number of nodes in the network, 
# 'choose whether the number of true-values per state should be fixed or a maximum limit, 
#' and specify the number of true-values for each state.
#'
#' @return A list containing three elements:
#'         - nodes_in_network: Integer, the number of nodes in the network.
#'         - fix_or_limit: Logical, TRUE if the number of true-values is fixed, FALSE if it is a maximum limit.
#'         - count_trues_after_transaction: Integer, the selected number of true-values per state.

choose_parameters_method1 <- function(){
  # Prompt the user for the number of nodes in the network.
  repeat {
    nodes_in_network <- as.integer(readline(prompt = cli_text("Select the number of nodes in the network (positive integer): ")))
  
    # Validate that the input is a positive integer.
    if(!is.na(nodes_in_network) && nodes_in_network > 0){
      
      # Display a success message and break the loop if the selected number is valid.
      cli_alert_success("You chose {cli::col_blue(nodes_in_network)} nodes for the network!")
      break
      
      # Alert the user that the input was invalid and prompt to try again.
    } else {
      cli_alert_danger("You entered {cli::col_blue(nodes_in_network)}. This is not a valid input.\nPlease try again.")
    }
  }
  
  # Prompt the user to decide if the number of true-values per state is fixed or a maximum limit.
  repeat {
    fix_or_limit <- tolower(readline(prompt = cli_text("Should the number of true-values per state be a fixed value (f) or a maximum limit (l)? (Enter 'f' or 'l'): ")))
    
    # Validate that the input is either 'f' for fixed or 'l' for limit.
    # Case 1: The input is 'f' for fixed.
    if (fix_or_limit %in% c("f")) {
      
      # Assign the value TRUE to the variable.
      fix_or_limit <- TRUE
      
      # Display a success message and break the loop if the input is valid.
      cli_alert_success("You chose the number of true-values per state to be a fixed value!")
      break
      
      # Case 2: The input is 'l' for limit.
    } else if (fix_or_limit %in% c("l")) {
      
      # Assign the value FALSE to the variable.
      fix_or_limit <- FALSE
      
      # Display a success message and break the loop if the input is valid.
      cli_alert_success("You chose the number of true-values per state to be a maximum limit!")
      break
      
      # Alert the user that the input was invalid and prompt to try again.
    } else {
      cli_alert_danger("You entered {cli::col_blue(fix_or_limit)}. This is not a valid input.\nPlease try again.")
    }
  }
  
  
  # Prompt the user to select the number of true-values per state.
  repeat {
    count_trues_after_transaction <- as.integer(readline(prompt = cli_text("Select the number of true-values per state : ")))
    
    # Validate that the input is non-negative and within the valid range (less or equal than the total number of nodes in the network).
    # Case 1: The input is valid.
    if (!is.na(count_trues_after_transaction) && count_trues_after_transaction >= 0 && count_trues_after_transaction <= nodes_in_network) {
      
      # Case 1a: The number of true-values per state is fixed.
      if (fix_or_limit == TRUE){
        
        # Display a success message and break the loop if the input is valid.
        cli_alert_success("You chose exactly {cli::col_blue(count_trues_after_transaction)} true-values per state!")
      } 
      # Case 1b: The number of true-values per state is a limit.
      else{
        
        # Display a success message and break the loop if the input is valid.
        cli_alert_success("You chose a maximum of {cli::col_blue(count_trues_after_transaction)} true-values per state!")
      }
      break 
      
    } 
    # Case 2: The input is invalid.
    else {
      # Case 2a: The input is not a positive integer.
      if (is.na(count_trues_after_transaction) || count_trues_after_transaction < 0) {
        cli_alert_danger("You entered {cli::col_blue(count_trues_after_transaction)}. This is not a valid input.\nThe number of true-values must be a positive integer.\nPlease try again.")
        
      } 
      # Case 2b: The input is bigger than the total number of nodes in the network.
      else {
        cli_alert_danger("You entered {cli::col_blue(count_trues_after_transaction)}. This is not a valid input.\nThe number of true-values must be less than or equal to the number of variables.\nPlease try again.")
      }
    }
  }
  
  # Return a list containing the number of nodes in the network, a logical value that determined if the number of true-values is fixed or a limit and the number of true-values per state after transaction.
  return(list(nodes_in_network = nodes_in_network, fix_or_limit = fix_or_limit, count_trues_after_transaction = count_trues_after_transaction))
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


#' This function organizes the entire user interaction process by querying various parameters for the creation and modification of truth tables.
#' 
#' @return: A list consisting of the selected generating method, the deleting probability as well as a list of different parameters for generating the truth table.

user_interaction <- function (){
  
  # Give the user a short introduction about the application.
  user_welcome()
  
  # Obtain the method selected by the user to generate the truth table.
  selected_method <- choose_method()
  
  # Depending on the selected method, the corresponding function is executed to to ask the user about the choice of parameters.
  # Case 1:
  if (selected_method == 1){
    selected_parameters <- choose_parameters_method1()
  }
  # Case 2:
  else if (selected_method == 2){
    selected_parameters <- choose_parameters_method2()
  # Case 3:
  } else{
    selected_parameters <- choose_parameters_method3()
  }
  
  # Obtain the deletion probability selected by the user. This probability is used to delete rows in the truth table.
  deleting_probability <- choose_deleting_probability()
  
  # Store all of the user's selected parameters in a list.
  output <- list(selected_method = selected_method,
                 deleting_probability = deleting_probability,
                 parameters = selected_parameters)
 
  # Return a list consisting of the selected generating method, the deleting probability as well as a list of different parameters for generating the truth table.
  return (output)
  
}

################################################################################
#                                   TEST                                       #
################################################################################

result <- user_interaction()
print(result)

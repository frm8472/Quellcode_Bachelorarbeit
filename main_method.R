#  Clear the entire workspace by removing all objects. 
rm(list = ls())

# Load and execute the necessary scripts to access their functions.
source("D:\\Repositorys\\Quellcode_Bachelorarbeit\\user_interaction.R")
source("D:\\Repositorys\\Quellcode_Bachelorarbeit\\create_edit_truthtables.R")
source("D:\\Repositorys\\Quellcode_Bachelorarbeit\\combine_truthtables.R")
source("D:\\Repositorys\\Quellcode_Bachelorarbeit\\boolnet_truthtables.R")



#' This function organizes the entire process of creating and modifying truth tables.
#'
#' @return A list consisting of the entire truth table and the truth table with deleted rows.

main_function <- function (){
  
  # Get the parameters for generating a truth table that the user selected
  selected_user_parameters <- user_interaction()
  
  # Get the probability for the deletion of rows in the truth table. 
  deletion_probability <- selected_user_parameters$deleting_probability
  
  # Depending on the selected method, the corresponding function is executed to generate the truth tables.
  # Case 1: 
  if (selected_user_parameters$selected_method == 1){
    truth_table_complete <- create_truthtables(selected_user_parameters$parameters$nodes_in_network, selected_user_parameters$parameters$count_trues_after_transaction, selected_user_parameters$parameters$fix_or_limit)
  # Case 2:
  } else if (selected_user_parameters$selected_method == 2){
    truth_table_complete <- generate_all_truth_tables (selected_user_parameters$parameters$nodes_in_network, selected_user_parameters$parameters$fix_or_limit, selected_user_parameters$parameters$number_genes_input_transition)
 # Case3: 
  } else {
  }
  
  # Random rows in the table are deleted according to the selected probability.
  truth_table_modified <- delete_rows(truth_table_complete, deletion_probability)
  
  # Return the entire truth table and the truth table with the deleted rows in a list.
  return (list(truth_table_complete, truth_table_modified))
  
}

################################################################################
#                                   TEST                                       #
################################################################################
result <- main_function()
print(result)
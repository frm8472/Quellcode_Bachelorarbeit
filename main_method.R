rm(list = ls())
source("D:\\Repositorys\\Quellcode_Bachelorarbeit\\user_interaction.R")
source("D:\\Repositorys\\Quellcode_Bachelorarbeit\\create_edit_truthtables.R")
source("D:\\Repositorys\\Quellcode_Bachelorarbeit\\combine_truthtables.R")
source("D:\\Repositorys\\Quellcode_Bachelorarbeit\\boolnet_truthtables.R")

main_function <- function (){
  selected_user_parameters <- user_interaction()
  deletion_probability <- selected_user_parameters$deleting_probability
  
  if (selected_user_parameters$selected_method == 1){
    truth_table_complete <- create_truthtables(selected_user_parameters$parameters$nodes_in_network, selected_user_parameters$parameters$count_trues_after_transation, selected_user_parameters$parameters$fix_or_limit)
  } else if (selected_user_parameters$selected_method == 2){
    truth_table_complete <- generate_all_truth_tables (selected_user_parameters$parameters$nodes_in_network, selected_user_parameters$parameters$fix_or_limit, selected_user_parameters$parameters$number_genes_input_transition)
  } else {
  }
  
  truth_table_modified <- delete_rows(truth_table_complete, deletion_probability)
  
  
 return (truth_table_modified)
  
}
result <- main_function()
print(result)
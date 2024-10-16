rm(list = ls())

# Generates random truth-table with the n input variables and 1 output variable
# Returns a list of the random truth-table and the order of the input variables x_i needed to convert to z_i in the complete truth-table
generate_truth_table <- function(number_input_variables, maximum_number_input_variables) {
  
  
  # soll maximum_number_input_variables mal durchgeführt werden
  
  
  
  # Generate a data-frame that contains all possible combinations for n input variables 
  possible_combinations <- expand.grid(replicate(number_input_variables, c(0, 1), simplify = FALSE))
  
  # Create a vector y (output variable) that contains random truth-values for each row
  y_output_variable <- apply(possible_combinations, 1, function(row) {
    # Choose a random value out of 0 or 1 for y
    sample(c(0, 1), 1) 
  })
  
  # Append the vector y to the data-frame that contains all possible combinations and create a random truth_table
  random_truth_table <- cbind(possible_combinations, y_output_variable)
  
  # Assign to the n first columns the names x_i. The last column is named y.
  colnames(random_truth_table) <- c(paste0("x", 1:number_input_variables), "y")
  
  # Convert the data-frame to a matrix
  random_truth_table_matrix <- as.matrix(random_truth_table)
  
  # Generate a random relationship between x_i and z_i of the complete truth-table in form of a vector
  #z_indices <- sample(1:number_input_variables)
  z_indices <- sample(1:maximum_number_input_variables, number_input_variables)
  
  # Returns a list consisting of the random truth-table in form of a matrix as well as the vector that contains the relationship between x_i and z_i
  return(list(random_truth_table_matrix = random_truth_table_matrix,z_indices = z_indices))
}


# Sorts a truth-table according to a vector establish a relationship between x_i and z_i
sort_columns_of_truth_table <- function (truth_table, z_indices){
  
  # Get the number of input-variables of the random truth-table
  number_input_variables <- ncol(truth_table)
  
  # Sort the z_indices in ascending order
  z_indices_sorted <- sort(z_indices)
  
  # Order the initial indices
  sorting_indices <- order(z_indices)
  
  # Sort the columns of the truth-table according to the sorting_indices.
  sorted_truth_table <- truth_table[, sorting_indices]
  
  # Add the new column names z_i to the columns of the truth-table matrix
  colnames(sorted_truth_table) <- paste0("z", z_indices_sorted)
  
  # Append the last column containing the output variable to the truth-table
  sorted_truth_table <- cbind(sorted_truth_table, y =  truth_table[, (number_input_variables)])
  
  # Returns the sorted truth-table
  return (sorted_truth_table)
}

combine_truth_tables <- function(list_truth_tables, maximum_number_input_variables) {
  
  # Generate a data-frame that contains all possible combinations for the input variables 
  combined_truth_table_dataframe <- expand.grid(replicate(maximum_number_input_variables, c(0, 1), simplify = FALSE))
  
  # Name the columns of the dataframe z_i
  colnames(combined_truth_table_dataframe) <- paste0("z", 1:maximum_number_input_variables)
  
  # Convert the data-frame to a matrix
  combined_truth_table_matrix <- as.matrix(combined_truth_table_dataframe)
  
  # Add the y-columns for each truth-table in the list
  y_columns <- lapply(list_truth_tables, function(truth_table_matrix) {
    
    # Find common columns between the combined truth-table and the current truth-table
    common_columns <- intersect(colnames(combined_truth_table_matrix), colnames(truth_table_matrix))
    
    # Add an empty column (NA) and fill it with the correct values according to the intersection
    y_column <- apply(combined_truth_table_matrix[, common_columns, drop = FALSE], 1, function(row) {
      
      # Find matching indices where all values match
      match_idx <- which(apply(truth_table_matrix[, common_columns, drop = FALSE], 1, function(x) all(x == row)))
      
      # If a match is found, return the y-value; otherwise return NA
      if (length(match_idx) > 0) {
        return(truth_table_matrix[match_idx[1], "y"])
      } else {
        return(NA)
      }
    })
    
    # Return the y-vector
    return(y_column)
  })
  
  # Bind the y-columns to the combined truth table
  combined_truth_table_matrix <- cbind(combined_truth_table_matrix, do.call(cbind, y_columns))
  
  # Set the names for the y-columns
  y_column_names <- paste0("y", seq_along(list_truth_tables))
  colnames(combined_truth_table_matrix)[(maximum_number_input_variables + 1):ncol(combined_truth_table_matrix)] <- y_column_names
  
  # Return the combined truth table with the added y-columns
  return(combined_truth_table_matrix)
}




###########################################
#               EXAMPLE                   #  
###########################################

result_truth_table_1 <- generate_truth_table(3,5)

result_truth_table_2 <- generate_truth_table(2,5)

print(result_truth_table_1$random_truth_table_matrix)
print(result_truth_table_1$z_indices)

result_sorted_1 <- sort_columns_of_truth_table(result_truth_table_1$random_truth_table_matrix, result_truth_table_1$z_indices)
result_sorted_2 <- sort_columns_of_truth_table(result_truth_table_2$random_truth_table_matrix, result_truth_table_2$z_indices)

result <-  combine_truth_tables(list(result_sorted_1, result_sorted_2), 5)
print(result)


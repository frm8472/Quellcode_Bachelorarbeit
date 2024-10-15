funct <- function(n, k) {
  ind <- sample(c(1:n), k)
  return(function() {
    return(ind[1] - ind[2])  
  })
}

# Create the inner function
inner_function <- funct()

# Call the inner function and print the result
print(inner_function())
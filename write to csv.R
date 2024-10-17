
# Wahrheitstabelle als Matrix erstellen und anzeigen
output <- create_truthtables(5,3,TRUE )

# Loesche einige Zeilen basierend auf der Wahrscheinlichkeit
modified_matrix <- delete_rows(output,50)
print(output)
print(modified_matrix)


setwd("D:\\Repositorys\\Truthtables")

quoted_colnames <- colnames(output)

# Save Truthtables in csv-files
write.csv(output, file = "truthtable_all.csv", row.names = FALSE, col.names = quoted_colnames, na = "NA", quote = FALSE)
write.csv(modified_matrix, file = "truthtable_modified.csv", row.names = FALSE, col.names = quoted_colnames, na =  "NA", quote = FALSE)

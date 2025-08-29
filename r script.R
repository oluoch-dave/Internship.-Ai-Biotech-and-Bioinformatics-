#The function Classify_gene()

classify_gene <- function(logFC, padj) {
  if (logFC > 1 & padj < 0.05) {
    return("Upregulated")
  } else if (logFC < -1 & padj < 0.05) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}

#Defining the Input and Output folder
input_dir <- "Raw Data"
output_dir <- "Results"

#Creating Output folder if it doesn't exist

if(!dir.exists(output_dir)){
  dir.create(output_dir)
}

#The list of files to process

files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")

#Preparing empty list to processed results

result_list <- list()

#Processing each file in a loop

for (file_names in files_to_process) {
  cat("\nProcessing:", file_names, "\n")
  
  input_file_path <- file.path(input_dir, file_names)
}

#Importing the dataset

data <- read.csv(input_file_path, header = TRUE)
cat("File imported. Checking for missing values...\n")

#Handling the missing values in the padj column (replacing with 1)

if("padj" %in% names(data)){
  missing_count <- sum(is.na(data$padj))
  
  cat("Missing values in 'padj':", missing_count, "\n")
  data$padj[is.na(data$padj)] <- 1
}

#Adding new column 'status', applying the Classify_gene function to each row

data$status <- mapply(classify_gene, data$logFC, data$padj)
cat("Gene status classification completed.\n")

#Saving the results in the R list

result_list[[file_names]] <- data

#Saving the processed files in the Results folder

output_file_path <- file.path(output_dir, paste0("classified_", file_names))
write.csv(data, output_file_path, row.names = FALSE)
cat("Results saved to:", output_dir, "\n")


#Printing the summary counts of significant, upregulated and downregulated

cat("\nSummary for", file_names, ":\n")
cat("Total genes:", nrow(data), "\n")
cat("Upregulated genes:", sum(data$status == "Upregulated"), "\n")
cat("Downregulated genes:", sum(data$status == "Downregulated"), "\n")
cat("Not significant genes:", sum(data$status == "Not_Significant"), "\n")

#Using table() for summary

cat("\nTable summary:\n")
print(table(data$status))


#Assessing the results

results_1 <- result_list[["DEGs_Data_1.csv"]] 
results_2 <- result_list[["DEGs_Data_2.csv"]]

#Saving the Entire R workspace

save.image(file = "DavidOluoch_Class_2_Assignment.RData")




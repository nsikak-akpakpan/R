# reading from clipboard into dataframe, use this after copying the data to the clipboard
my_data <- read.table(file = "clipboard", sep = "\t", header = TRUE)
print(my_data) # print the data for preview
colnames(my_data) # list the column names

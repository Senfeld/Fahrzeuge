##### ADD LINES FOR ALL PACKAGES SO THAT ALL GET INSTALLED: I:E: 
##### NEEDS TO WORK ON A FRESH START
##### CREATE VISUALIZATION FOR EACH OEM WITH A data.tree https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
install.packages("tidyr")
install.packages("readr")
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

file_path <- 'Fahrzeug/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=';', header=TRUE)

# data_karosserie <- data %>%
#   group_by(Grouped_Column = substr(ID_Karosserie, 1, 2)) %>%
#   summarize(Count = n(),
#             FirstValue = first(ID_Karosserie))
# data_karosserie

summarize_and_print <- function(data, columns) {
  for (col in columns) {
    result <- data %>%
      group_by(Grouped_Column = str_extract(!!sym(col), "^[^-]+")) %>%
      summarise(Count = n(),
                FirstValue = first(!!sym(col)))
    
    cat("\nSummary for column:", col, "\n")
    print(result, row.names = FALSE)
  }
}

columns_to_summarize <- c("ID_Karosserie", "ID_Schaltung", "ID_Sitze", "ID_Motor")

# Call the function
summarize_and_print(data, columns_to_summarize)



############################ Cleaner_einzelteile working version below for .csv!

file_path <- 'Einzelteile/Einzelteil_T32.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=";", header=TRUE)


cleaner_einzelteile <- function(data) {
  data$X <- NULL
  data$X1 <- NULL
  
  if (ncol(data) == 7) {
    column_vector <- colnames(data)
    column_vector <- na.omit(column_vector)
    return(data)
  }
  else if (ncol(data) == 14) {
    original_columns <- colnames(data)[1:7]
    # Remove the ".x" suffix from each column name
    original_columns <- sub("\\.x$", "", original_columns)
    concatenated_data <- data.frame(matrix(nrow = 2 * nrow(data), ncol = length(original_columns)))
    colnames(concatenated_data) <- original_columns
    for (col in original_columns) {
      concatenated_data[[col]] <- c(data[[paste0(col, ".x")]], data[[paste0(col, ".y")]])
    }
    return(concatenated_data)
  }
  else if (ncol(data) == 21){
    original_columns <- colnames(data)[!grepl("\\.x$|\\.y$", colnames(data))]
    concatenated_data <- data.frame(matrix(nrow = 3 * nrow(data), ncol = length(original_columns)))
    colnames(concatenated_data) <- original_columns
    for (col in original_columns) {
      concatenated_data[[col]] <- c(data[[col]], data[[paste0(col, ".x")]], data[[paste0(col, ".y")]])
    }
    return(concatenated_data)
  }
}

cleandata <- cleaner_einzelteile(data)

cleandata <- cleandata[!is.na(cleandata$ID_T32), ]

#Delete columns only containing NAs
#cleandata <- cleandata[, colSums(is.na(cleandata)) != nrow(cleandata)]



######################## Code for solving .txt's!           ##########################
Sys.setenv("VROOM_CONNECTION_SIZE" = "3GB")
separator <- "  "  # adjust this according to your file's format
line_break <- " "
setwd("C:/Users/vbjan/Documents/Fahrzeuge/Einzelteile")

file_path <- 'Einzelteil_T36.txt'
data <- read_file(paste0(file_path))
data <- str_replace_all(data, pattern = separator, replacement = ";")
data <- str_replace_all(data, pattern = line_break, replacement = "\n")
#write.csv(data, file='T_36.csv')
cat(data, file = "T36.csv")

file_path <- 'T36.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=";", header=TRUE)


#### T31
Sys.setenv("VROOM_CONNECTION_SIZE" = "3GB")
separator <- "  "  # adjust this according to your file's format
line_break <- ""
setwd("C:/Users/vbjan/Documents/Fahrzeuge/Einzelteile")
file_path <- 'Einzelteil_T31.txt'
data <- read_file(paste0(file_path))
data <- str_replace_all(data, pattern = separator, replacement = ";")
data <- str_replace_all(data, pattern = line_break, replacement = "\n")
#write.csv(data, file='T_36.csv')
cat(data, file = "T31.csv")

file_path <- 'T31.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=";", header=TRUE)

#### T34
Sys.setenv("VROOM_CONNECTION_SIZE" = "3GB")
separator <- " | | "  # adjust this according to your file's format
line_break <- ""
setwd("C:/Users/vbjan/Documents/Fahrzeuge/Einzelteile")
file_path <- 'Einzelteil_T34.txt'
data <- read_file(paste0(file_path))
data <- str_replace_all(data, pattern = fixed(separator), replacement = ";")
data <- str_replace_all(data, pattern = fixed(line_break), replacement = "\n")
#write.csv(data, file='T_36.csv')
cat(data, file = "T34.csv")

file_path <- 'T34.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=";", header=TRUE)


#### T35
separator <- "\\"  # adjust this according to your file's format
line_break <- "" ############################ So machen das die Zahlen die Anführungsstrichen sind die Zeilenumbrüche sind "123".
setwd("C:/Users/vbjan/Documents/Fahrzeuge/Einzelteile")
file_path <- 'Einzelteil_T35.txt'
data <- read_file(paste0(file_path))
data <- str_replace_all(data, pattern = fixed(separator), replacement = ";")
data <- str_replace_all(data, pattern = paste0("(?<=;", separator, ");(?=;)", sep = ""), replacement = "\n")
#write.csv(data, file='T_36.csv')
cat(data, file = "T35.csv")

file_path <- 'T35.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=";", header=TRUE)


#### T39 WORKS
Sys.setenv("VROOM_CONNECTION_SIZE" = "3GB")
separator <- "\\"  # adjust this according to your file's format
line_break <- "  "
setwd("C:/Users/vbjan/Documents/Fahrzeuge/Einzelteile")
file_path <- 'Einzelteil_T39.txt'
data <- read_file(paste0(file_path))
data <- str_replace_all(data, pattern = fixed(separator), replacement = ";")
data <- str_replace_all(data, pattern = line_break, replacement = "\n")
#write.csv(data, file='T_36.csv')
cat(data, file = "T39.csv")

file_path <- 'T35.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=";", header=TRUE)







################## Version currently not working
cleaner_einzelteile <- function(data) {
  data$X <- NULL
  data$X1 <- NULL
  original_columns <- colnames(data)[!grepl("\\.x$|\\.y$", colnames(data))]
  concatenated_data <- data.frame(matrix(nrow = 3 * nrow(data), ncol = length(original_columns)))
  if (ncol(data) == 7) {
    column_vector <- colnames(data)
    column_vector <- na.omit(column_vector)
    return(data)
  }
  else if (ncol(data) == 14) {
    for (col in original_columns) {
      concatenated_data[[col]] <- c(data[[paste0(col, ".x")]], data[[paste0(col, ".y")]])
    }
    return(concatenated_data)
  }
  else if (ncol(data) == 21){
    for (col in original_columns) {
      concatenated_data[[col]] <- c(data[[col]], data[[paste0(col, ".x")]], data[[paste0(col, ".y")]])
    }
    return(concatenated_data)
  }
}

cleandata <- cleaner_einzelteile(data)

cleandata <- cleandata[!is.na(cleandata$ID_T32), ]

#Delete columns only containing NAs
cleandata <- cleandata[, colSums(is.na(cleandata)) != nrow(cleandata)]


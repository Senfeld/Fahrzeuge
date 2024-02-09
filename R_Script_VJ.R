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



############################ Cleaner_einzelteile working version below

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







file_path <- 'Einzelteile/Einzelteil_T31.txt'
# Read the txt fil
data <- read_delim(file_path, delim = " ")


file_path <- 'Einzelteile/Einzelteil_T39.txt'
# Read the txt fil
data <- read_delim(file_path)


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


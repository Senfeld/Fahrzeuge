##### ADD LINES FOR ALL PACKAGES SO THAT ALL GET INSTALLED: I:E: 
##### NEEDS TO WORK ON A FRESH START
##### CREATE VISUALIZATION FOR EACH OEM WITH A data.tree https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
install.packages("tidyr")
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)

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
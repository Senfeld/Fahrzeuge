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


###### T35 (working version)
library(tidyverse)

text <- readLines("Einzelteile/Einzelteil_T35.txt", warn = FALSE)
gesamter_text <- paste(text, collapse = " ")


# Ersetzen der spezifischen Sequenz durch einen Zeilenumbruch (\n)
# Die Sequenz "\\\"\\d+\"\\\\\\d+\\\\" steht für "ganzeZahl"\ganzeZahl\
# und wird durch einen Zeilenumbruch ersetzt, wobei wir die Sequenz selbst entfernen.
# Fügen \n vor der Sequenz ein, um den neuen Datensatz in einer neuen Zeile zu beginnen.
gesamter_text <- gsub("(?<!^)(\\\"\\d+-\\d+-\\d+-\\d+\"\\\\)", "\n\\1", gesamter_text, perl = TRUE)


# Aufteilen des Textes in Zeilen basierend auf den eingefügten Zeilenumbrüchen
gesplitteter_text <- unlist(strsplit(gesamter_text, "\n"))

#Entfernen Spalte "X1"
gesplitteter_text <- lapply(gesplitteter_text, function(x) {
  sub("^\\\"X1\"\\\\", "", x)
})


# Ersetze | | und \ durch Komma in jedem Datensatz
gesplitteter_text <- lapply(gesplitteter_text, function(x) {
  x <- gsub("\\| \\|", ",", x) # Ersetze | | durch Komma
  gsub("\\\\", ",", x)         # Ersetze \ durch Komma
})

#Macht es möglich .txt als .csv einzulesen 
txt_to_csv <- paste(gesplitteter_text, collapse = "\n")

#Ab Zeile 491308 wird es eklig, aber lässt sich trotzdem mit arbeiten 
T35 <- read_csv(txt_to_csv, col_names = TRUE)

#Entfernt alle Zeilen, die nicht Fehlerhafte Bauteile enthalten 
T35_clean <- T35[!is.na(T35$Fehlerhaft_Datum.x),]

#entfernt in der Spalte den "Zahl" character an einigen Dezimalzahlen 
T35_clean$Fehlerhaft_Fahrleistung.x <- str_replace_all(T35_clean$Fehlerhaft_Fahrleistung.x, '\"[0-9]+\"', '')

#Entfernt alle Spalten, die NA enthalten 
T35_clean <- T35_clean %>%
  select(where(~ all(!is.na(.))))

#Umbennenung der Spalten 
T35_clean <- T35_clean %>% 
  rename(ID_T35 = ID_T35.x,
         Produktionsdatum = Produktionsdatum.x,
         Herstellernummer = Herstellernummer.x,
         Werksnummer = Werksnummer.x,
         Fehlerhaft = Fehlerhaft.x,
         Fehlerhaft_Datum = Fehlerhaft_Datum.x,
         Fehlerhaft_Fahrleistung = Fehlerhaft_Fahrleistung.x)

#Spalte in numeric-Klasse umwandeln 
T35_clean$Fehlerhaft_Fahrleistung <- as.numeric(T35_clean$Fehlerhaft_Fahrleistung)





##### T34 
text <- readLines("Einzelteile/Einzelteil_T34.txt", warn = FALSE)
gesamter_text <- paste(text, collapse = " ")


# Ersetzen der spezifischen Sequenz durch einen Zeilenumbruch (\n)
# Die Sequenz "\\\"\\d+\"\\\\\\d+\\\\" steht für "ganzeZahl"\ganzeZahl\
# und wird durch einen Zeilenumbruch ersetzt, wobei wir die Sequenz selbst entfernen.
# Fügen \n vor der Sequenz ein, um den neuen Datensatz in einer neuen Zeile zu beginnen.
gesamter_text <- gsub("(?<!^)(\"\\d+-\\d+-\\d+-\\d+\" \\| \\| )", "\n\\1", gesamter_text, perl = TRUE)




# Aufteilen des Textes in Zeilen basierend auf den eingefügten Zeilenumbrüchen
gesplitteter_text <- unlist(strsplit(gesamter_text, "\n"))

#Entfernen Spalte "X1"
gesplitteter_text <- lapply(gesplitteter_text, function(x) {
  sub("^\\\"X1\" \\| \\| ", "", x)
})



# Ersetze | | und \ durch Komma in jedem Datensatz
gesplitteter_text <- lapply(gesplitteter_text, function(x) {
  x <- gsub("\\| \\|", ",", x) # Ersetze | | durch Komma
  gsub("\\\\", ",", x)         # Ersetze \ durch Komma
})

#Macht es möglich .txt als .csv einzulesen 
txt_to_csv <- paste(gesplitteter_text, collapse = "\n")

#Ab Zeile 491308 wird es eklig, aber lässt sich trotzdem mit arbeiten 
T34 <- read_csv(txt_to_csv, col_names = TRUE)

#Spaltennamen umbennen, hier wird Spalte 9, die 1 heißt umbenannt zu "Unsinn", da man diese sonst nicht einfach löschen    könnte 
#Verschieben der Spalte "Produktionsdatum" auf zweite Position
T34_clean <- T34 %>% 
  rename(Produktionsdatum = Produktionsdatum_Origin_01011970,
         Unsinn = 9) %>%
  select(ID_T34, Produktionsdatum, everything())

#Spalte in Datumformat bringen - besprechen, ob origin-Spalte genutzt werden muss
T34_clean$Produktionsdatum <- as.Date(T34_clean$Produktionsdatum, origin = "1970-01-01")

#Restliche Spalten löschen
T34_clean$...10 <- NULL
T34_clean$Unsinn <- NULL 
T34_clean$`origin"1`<- NULL 

#Entfernt alle Zeilen, die nicht Fehlerhafte Bauteile enthalten 
T34_clean <- T34_clean[!is.na(T34$Fehlerhaft_Datum),]

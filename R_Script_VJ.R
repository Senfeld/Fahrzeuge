install.packages("tidyr")
library(dplyr)
library(tidyr)
                 
file_path <- 'Zulassungen/Zulassungen_alle_Fahrzeuge.csv'

# Read the CSV file
data <- read.csv(file_path,  sep=';', header=TRUE)

# Display the first few rows of the data

#### Gemeinden checken haben eine 1 am Ende.
head(data)
tail(data)

############# Einzelteile
#####Ideen zum säubern der Datie:
###### Check ID_T21 mit Herstellernummer und Werksnummer. 
file_path <- 'Einzelteil_T21.csv'

# Read the CSV file
data <- read.csv(file_path, sep=';', header=TRUE)

# Display the first few rows of the data

#### Gemeinden checken haben eine 1 am Ende.
head(data)
tail(data)

install.packages("tidyr")
library(tidyr)
file_path <- 'Fahrzeug/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=';', header=TRUE)

data_karosserie <- data %>%
  group_by(Grouped_Column = substr(ID_Karosserie, 1, 2)) %>%
  summarize(Count = n(),
            FirstValue = first(ID_Karosserie))

data_schaltung <- data %>%
  group_by(Grouped_Column = substr(ID_Schaltung, 1, 2)) %>%
  summarize(Count = n(),
            FirstValue = first(ID_Schaltung))
data_schaltung
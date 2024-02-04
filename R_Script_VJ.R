##### ADD LINES FOR ALL PACKAGES SO THAT ALL GET INSTALLED: I:E: 
##### NEEDS TO WORK ON A FRESH START
install.packages("tidyr")
library(tidyr)
library(dplyr)
library(stringr)

file_path <- 'Fahrzeug/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=';', header=TRUE)

data_karosserie <- data %>%
  group_by(Grouped_Column = substr(ID_Karosserie, 1, 2)) %>%
  summarize(Count = n(),
            FirstValue = first(ID_Karosserie))
data_karosserie

data_schaltung <- data %>%
  group_by(Grouped_Column = str_extract(ID_Schaltung, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Schaltung))
data_schaltung

data_sitze <- data %>%
  group_by(Grouped_Column = str_extract(ID_Sitze, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Sitze))
data_sitze

data_motor <- data %>%
  group_by(Grouped_Column = str_extract(ID_Motor, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Motor))
data_motor

file_path <- 'Fahrzeug/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=';', header=TRUE)

data_karosserie <- data %>%
  group_by(Grouped_Column = substr(ID_Karosserie, 1, 2)) %>%
  summarize(Count = n(),
            FirstValue = first(ID_Karosserie))
data_karosserie

data_schaltung <- data %>%
  group_by(Grouped_Column = str_extract(ID_Schaltung, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Schaltung))
data_schaltung

data_sitze <- data %>%
  group_by(Grouped_Column = str_extract(ID_Sitze, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Sitze))
data_sitze

data_motor <- data %>%
  group_by(Grouped_Column = str_extract(ID_Motor, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Motor))
data_motor

file_path <- 'Fahrzeug/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=';', header=TRUE)

data_karosserie <- data %>%
  group_by(Grouped_Column = substr(ID_Karosserie, 1, 2)) %>%
  summarize(Count = n(),
            FirstValue = first(ID_Karosserie))
data_karosserie

data_schaltung <- data %>%
  group_by(Grouped_Column = str_extract(ID_Schaltung, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Schaltung))
data_schaltung

data_sitze <- data %>%
  group_by(Grouped_Column = str_extract(ID_Sitze, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Sitze))
data_sitze

data_motor <- data %>%
  group_by(Grouped_Column = str_extract(ID_Motor, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Motor))
data_motor

file_path <- 'Fahrzeug/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv'
# Read the CSV fil
data <- read.csv(file_path, sep=';', header=TRUE)

data_karosserie <- data %>%
  group_by(Grouped_Column = substr(ID_Karosserie, 1, 2)) %>%
  summarize(Count = n(),
            FirstValue = first(ID_Karosserie))
data_karosserie

data_schaltung <- data %>%
  group_by(Grouped_Column = str_extract(ID_Schaltung, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Schaltung))
data_schaltung

data_sitze <- data %>%
  group_by(Grouped_Column = str_extract(ID_Sitze, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Sitze))
data_sitze

data_motor <- data %>%
  group_by(Grouped_Column = str_extract(ID_Motor, "^[^-]+")) %>%
  summarise(Count = n(),
            FirstValue = first(ID_Motor))
data_motor


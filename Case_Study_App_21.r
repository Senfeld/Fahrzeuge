##
## This is a Shiny web application. The application is run when clicking "Run app".
##
## Find out more about building applications with Shiny here:
##
##    http://shiny.rstudio.com/
##

if(!require(install.load)){
  install.packages("install.load")
}

library(install.load)
install_load("DT","leaflet","leaflet.extras","plotly","tidyverse","shiny","shinythemes")

load('Finaler_Datensatz_Gruppe_21.RDa')

ui <- fluidPage(
  titlePanel(title = div(img(src = "https://seeklogo.com/images/T/tu-berlin-logo-7A5917B47A-seeklogo.com.png", alt = "Mein Logo", height = "50px"),"Datenanalyse")),
  tags$head(
    tags$style(HTML("
      body {
        background-color: lightsteelblue;
      }

      .navbar {
        background-color: lightsteelblue !important;
      }

      .sidebar {
        background-color: lightsteelblue !important;
      }
    "))
  ),
  tabsetPanel(
    tabPanel("Suche",
             fluidPage(
               titlePanel("Autokarte"),
               tags$style(HTML("
            #search_panel {
              position: absolute;
              top: 200px; /* Anpassen der Position nach Bedarf */
              left: 85px; /* Anpassen der Position nach Bedarf */
              z-index: 1000;
              background-color: lightsteelblue;
              padding: 10px;
              border-radius: 5px;
              border: 3px solid white; /* Rahmen um das Suchfeld */
            }
      
            #map-container {
              border: 3px solid white; /* Rahmen um die Karte */
              border-radius: 5px;
              padding: 0px;
            }
          ")),
               div(
                 id = "search_panel",
                 textInput("vehicle_id", "Fahrzeug-ID eingeben:"),
                 actionButton("search_button", "Suche starten"),
                 verbatimTextOutput("vehicle_status")
               ),
               div(
                 id = "map-container",
                 leafletOutput("map", width = "100%", height = "calc(100vh - 150px)")
               )
             )
    ),
    
    tabPanel("Produktionsmenge",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("Filter"),
                   dateInput("from_date", "Produktionszeitraum von"),
                   dateInput("to_date", "Produktionszeitraum bis"),
                   selectInput("ebene_dropdown", "Ebene auswählen:", choices = c("Komponente", "Einzelteile")),
                   actionButton("Balkendiagramm_generieren", "Erstellen"),
                   style = "margin-top: 50px; border: 5px solid white; border-radius: 5px; padding: 10px;" 
                 ),
                 mainPanel(
                   plotlyOutput("histogram"),
                   style = "margin-top: 20px; border: 5px solid white; border-radius: 5px; padding: 10px;" 
                 )
               )
             )
    ),
    
    tabPanel("Daten",
             fluidPage(
               titlePanel("Große Datenbank"),
               DTOutput("daten_tabelle")
             )
    ),
    tabPanel("Fahrleistung - Vergleich",
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Filter"),
                 textInput("zip_code1", "Erste Postleitzahl:"),
                 textInput("zip_code2", "Zweite Postleitzahl:"),
                 actionButton("boxplot_generieren", "Generate Box Plot"),
                 style = "margin-top: 20px; border: 5px solid white; border-radius: 5px; padding: 10px;"
               ),
               mainPanel(
                 fluidRow(
                   column(width = 6, 
                          div(class = "boxplot-container", 
                              plotlyOutput("boxplot_left", height = "400px")
                          )
                   ),
                   column(width = 6, 
                          div(class = "boxplot-container", 
                              plotlyOutput("boxplot_right", height = "400px")
                          )
                   )
                 ),
                 style = "margin-top: 20px; border: 5px solid white; border-radius: 5px; padding: 10px;"
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  Große_Datenbank2 <- Große_Datenbank[!(is.na(Große_Datenbank$Fahrzeug_Fehlerhaft_Datum) & 
                                          is.na(Große_Datenbank$Komponente_Fehlerhaft_Datum) & 
                                          rowSums(is.na(Große_Datenbank[,30:ncol(Große_Datenbank)])) == ncol(Große_Datenbank[,30:ncol(Große_Datenbank)])),]
  
  Große_Datenbank2 <- left_join(Große_Datenbank2, Zulassungen_Gemeinde, by="ID_Fahrzeug")
  
  # Ersetze "," durch "." und konvertiere dann in Numeric
  Große_Datenbank2$Laengengrad <- as.numeric(gsub(",", ".", Große_Datenbank2$Laengengrad))
  Große_Datenbank2$Breitengrad <- as.numeric(gsub(",", ".", Große_Datenbank2$Breitengrad))
  
  Große_Datenbank2$Laengengrad <- as.numeric(Große_Datenbank2$Laengengrad)
  Große_Datenbank2$Breitengrad <- as.numeric(Große_Datenbank2$Breitengrad)
  
  
  # Karte rendern
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addHeatmap(
        data = Gemeinde_Fehler_Häufigkeit,
        lat = ~Breitengrad,
        lng = ~Laengengrad,
        intensity = ~Anzahl_Fehler,
        radius = 10,
        max = 10,
        blur = 20) %>% 
      setView(lng = 10.4515, lat = 51.1657, zoom = 6)
  })
  
  
  
  
  observeEvent(input$Balkendiagramm_generieren, {
    
    
    # Produktionsmenge Histogramm
    # Generate histogram based on filtered data
    filtered_data <- reactive({
      from_date <- isolate(input$from_date)
      to_date <- isolate(input$to_date)
      
      # Filter data based on selected date range
      filtered <- Große_Datenbank %>%
        filter(Fahrzeug_Produktionsdatum >= from_date & Fahrzeug_Produktionsdatum <= to_date) %>%
        mutate(Komponente = substr(ID_Komponente, 1, 2))
      
      return(filtered)
    })
    
    
    output$histogram <- renderPlotly({
      if (isolate(input$ebene_dropdown) == "Komponente") {
        # Plot Balkendiagramm für Komponente
        ggplotly(
          ggplot(filtered_data(), aes(x = OEM_Ort, fill = Komponente)) +
            geom_bar(position = "stack") +
            labs(title = "Produktionsmengen", x = "OEM Werk", y = "Komponentenmengen")
        )
      } else if (isolate(input$ebene_dropdown) == "Einzelteile") {
        
        oem_parts_df <- filtered_data() %>%
          select(OEM_Ort, starts_with("ID_T"))
        
        oem_parts_df$Anzahl_Einzelteile <- rowSums(!is.na(oem_parts_df[,paste0("ID_T",30:40)]))
        
        oem_parts_sum <- aggregate(Anzahl_Einzelteile ~ OEM_Ort, data = oem_parts_df, FUN = sum)
        
        # Plot Balekndiagramm für Einzelteile
        ggplotly(
          ggplot(oem_parts_sum, aes(x = OEM_Ort, y = Anzahl_Einzelteile)) +
            geom_bar(stat = "identity", fill = "lightsteelblue") +
            labs(title = "Gesamtanzahl der verbauten Einzelteile nach OEM-Ort",
                 x = "OEM-Ort",
                 y = "Gesamtanzahl der Einzelteile")
        )
        
      }
    })
  })
  
  
  # Tabelle mit den Daten
  ## Filtern und formatieren der Daten
  daten <- reactive({
    Große_Datenbank %>%
      select(ID_Fahrzeug, 
             Fahrzeug_Fehlerhaft_Datum, 
             Fahrzeug_Fehlerhaft_Fahrleistung, 
             ID_Komponente, 
             Komponente_Fehlerhaft_Datum, 
             Komponente_Fehlerhaft_Fahrleistung,
             starts_with("ID_T"),
             starts_with("Fehlerhaft_Datum_T"),
             starts_with("Fehlerhaft_Fahrleistung_T")) %>%
      mutate(across(contains("Datum"), as.Date),
             across(contains("Fahrleistung"), ~ round(., digits = 0)))  # Runden der Fahrleistung auf ganze Zahlen
  })
  
  # Anzeigen der Daten in einer interaktiven Tabelle
  output$daten_tabelle <- renderDT({
    datatable(daten(), 
              options = list(pageLength = 10),  # Anzahl der Zeilen pro Seite
              rownames = FALSE) %>%  # Keine Zeilennummern anzeigen
      formatStyle(names(daten()), background = "white", fontWeight = "bold") # Hintergrundfarbe und Schriftstärke für alle Spalten festlegen
    
  })
  
  # Fahrzeug-Suche
  observeEvent(input$search_button, {
    req(input$vehicle_id)  # Stelle sicher, dass eine Fahrzeug-ID eingegeben wurde
    
    if(input$vehicle_id %in% Große_Datenbank2$ID_Fahrzeug) {
      # Extrahieren Sie die Informationen zum Fahrzeug
      vehicle_info <- Große_Datenbank2[Große_Datenbank2$ID_Fahrzeug == input$vehicle_id, ]
      fehlerhaft <- paste("Ihr Fahrzeug mit der ID ", input$vehicle_id," ist fehlerhaft!")
      # Bestimme, ob das Fahrzeug, die Komponente oder eines der Einzelteile fehlerhaft ist
      if (!is.na(vehicle_info$Fahrzeug_Fehlerhaft_Datum)) {
        fehler_text <- "Das Fahrzeug ist fehlerhaft."
        fehler_datum <- vehicle_info$Fahrzeug_Fehlerhaft_Datum
        fehler_fahrleistung <- vehicle_info$Fahrzeug_Fehlerhaft_Fahrleistung
      } else if (!is.na(vehicle_info$Komponente_Fehlerhaft_Datum)) {
        fehler_text <- "Eine Komponente des Fahrzeugs ist fehlerhaft."
        fehler_datum <- vehicle_info$Komponente_Fehlerhaft_Datum
        fehler_fahrleistung <- vehicle_info$Komponente_Fehlerhaft_Fahrleistung
      } else {
        # Überprüfe jedes Einzelteil
        fehler_text <- NULL
        for (i in 30:40) {
          column_name <- paste("Fehlerhaft_Datum_T", i, sep = "")
          column_name_leistung <- paste("Fehlerhaft_Fahrleistung_T", i, sep = "")
          if (!is.na(vehicle_info[[column_name]])) {
            fehler_text <- paste("Ein Einzelteil (T", i, ") des Fahrzeugs ist fehlerhaft.", sep = "")
            fehler_datum <- vehicle_info[[column_name]]  
            fehler_fahrleistung <- round(vehicle_info[[column_name_leistung]], digits = 2)
            break
          }
        }
      }
      
      # Erstelle Popup-Nachricht mit den Fahrzeuginformationen und Fehlerstatus
      popup_text <- paste(
        "Herstellernummer - Fahrzeug:", vehicle_info$Fahrzeug_Herstellernummer, "<br>",
        "Produktionsdatum - Fahrzeug:", vehicle_info$Fahrzeug_Produktionsdatum, "<br>",
        "Fehlerhaftes Teil:",fehler_text,"<br>",
        "Datum - Fehler:",fehler_datum,"<br>",
        "Fahrlesitung - Fehler (km):",fehler_fahrleistung
        
      )
      
      # Füge den CircleMarker zur Karte hinzu
      leafletProxy("map", session) %>%
        clearMarkers() %>%
        addCircleMarkers(lng = vehicle_info$Laengengrad, lat = vehicle_info$Breitengrad, radius = 10, 
                         popup = popup_text)
      
      # Gebe die den Fehlerstatus aus
      output$vehicle_status <- renderText({
        paste("Fehlerstatus: ", fehlerhaft)
      })
    } else {
      output$vehicle_status <- renderText({
        "Ihr Fahrzeug ist nicht fehlerhaft."
      })
    }
  })
  
  
  observeEvent(input$boxplot_generieren, {
    zip_code1 <- isolate(input$zip_code1)
    zip_code2 <- isolate(input$zip_code2)
    
    # Filter data for the entered zip codes
    Große_Datenbank2$Postleitzahl <- as.character(Große_Datenbank2$Postleitzahl)
    
    PLZ1 <- Große_Datenbank2 %>% 
      filter(Postleitzahl == zip_code1 & Komponente_Fehlerhaft_Fahrleistung != 0)
    
    PLZ2 <- Große_Datenbank2 %>% 
      filter(Postleitzahl == zip_code2 & Komponente_Fehlerhaft_Fahrleistung != 0)
    
    # Create box plot with plotly
    output$boxplot_left <- renderPlotly({
      p1 <- plot_ly(PLZ1, x = ~Postleitzahl, y = ~Komponente_Fehlerhaft_Fahrleistung, 
                    type = 'box', 
                    name = paste("Boxplot für", zip_code1), 
                    marker = list(color = 'lightsteelblue')) %>%
        layout(title = paste("Boxplot für", zip_code1),
               xaxis = list(title = "Postleitzahl"),
               yaxis = list(title = "Fahrleistung (km)"))
      
      p1
    })
    
    output$boxplot_right <- renderPlotly({
      p2 <- plot_ly(PLZ2, x = ~Postleitzahl, y = ~Komponente_Fehlerhaft_Fahrleistung, 
                    type = 'box', 
                    name = paste("Boxplot für", zip_code2), 
                    marker = list(color = 'lightsteelblue')) %>%
        layout(title = paste("Boxplot für", zip_code2),
               xaxis = list(title = "Postleitzahl"),
               yaxis = list(title = "Fahrleistung (km)"))
      
      p2
    })
  })
}

shinyApp(ui, server)
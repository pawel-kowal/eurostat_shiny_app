library(shiny)
library(data.table)
library(googleVis)
library(plotly)

shinyUI(fluidPage(
  
  titlePanel("Liczba zgonow"),
  
  sidebarLayout(
    
    sidebarPanel(
      actionButton("getDataFromServer", "Pobierz dane"),
      selectInput("selectYear",
                  label = "Rok danych",
                  choices = as.vector(as.character(2023:2000),mode="list")
      ),
      selectInput("selectGender",
                  label = "Plec",
                  choices = as.vector(c('Kobieta','Mezczyzna'),mode="list")
      ),
      selectInput("selectCountry",
                  label = "Kraj",
                  choices = as.vector(c('AM',
                                        'AT',
                                        'BE',
                                        'BG',
                                        'CH',
                                        'CY',
                                        'CZ',
                                        'DE',
                                        'DK',
                                        'EE',
                                        'EL',
                                        'ES',
                                        'FI',
                                        'FR',
                                        'HR',
                                        'HU',
                                        'IE',
                                        'IS',
                                        'IT',
                                        'LI',
                                        'LT',
                                        'LU',
                                        'LV',
                                        'MT',
                                        'NL',
                                        'NO',
                                        'PL',
                                        'PT',
                                        'RO',
                                        'RS',
                                        'SE',
                                        'SI',
                                        'SK'),mode="list")
      )
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Tabela", DT::dataTableOutput("tabelka")),
                  tabPanel("Mapka",htmlOutput("view")),
                  tabPanel("Szereg czasowy", plotlyOutput("timeSeriesPlot"))
      )
    )
  )
))
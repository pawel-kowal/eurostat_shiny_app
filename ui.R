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
            )
        ),

        mainPanel(
            tabsetPanel(type = "tabs",
                tabPanel("Moja tabela", DT::dataTableOutput("dataSample")),
                tabPanel("Mapka",htmlOutput("view"))
            )
        )
    )
))

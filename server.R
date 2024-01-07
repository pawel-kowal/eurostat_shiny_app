library(shiny)
library(data.table)
library(googleVis)
library(ggplot2)
library(data.table)

shinyServer(function(input, output) {
  
  # lista z parametrami  
  outVar <- reactiveValues(
    selectYearVar = "2023",
    selectGenderVar = "Kobieta",
    selectCountryVar = "PL"
  )
  
  # pobranie wieku
  observeEvent(input$selectYear,{
    outVar$selectYearVar <- input$selectYear
  })
  
  # pobranie plci
  observeEvent(input$selectGender,{
    outVar$selectGenderVar <- input$selectGender
  })
  
  # pobieranie kraju do szeregu czasowego
  observeEvent(input$selectCountry,{
    outVar$selectCountryVar <- input$selectCountry
    
  })
  
  # pobranie danych (przygotowanie zmiennej na podstawie przycisku)
  v <- reactiveValues(dataLoadDownload = FALSE)
  
  observeEvent(input$getDataFromServer,{
    v$dataLoadDownload <- !v$dataLoadDownload
  })
  
  # pobranie danych ze stronki
  dataIn <- reactive({
    if(v$dataLoadDownload==TRUE){
      try({
        dataDir<- getwd()#file.path(getwd(),"data")
        download.file(url="https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/demo_r_mwk_ts.tsv.gz",
                      destfile=file.path(dataDir,"demo_r_mwk_ts.tsv.gz"),method="libcurl")
        d <- read.table(file=file.path(dataDir,"demo_r_mwk_ts.tsv.gz"),sep="\t",dec=".",header=T)
        countries <- c('AM','AT','BE','BG','CH','CY','CZ','DE','DK','EE','EL','ES','FI','FR','HR','HU','IE','IS','IT','LI','LT','LU','LV','MT','NL','NO','PL','PT','RO','RS','SE','SI','SK')
        x <- as.data.frame(rbindlist(lapply(countries,function(country){
          x <- t(d[grep(country,d[,1]),])
          x <- x[-1,]
          options(warn=-1)
          x <- data.frame(
            week = gsub("X","",rownames(x)), 
            female = as.integer(gsub(" p","",x[,1])),
            male = as.integer(gsub(" p","",x[,2])),
            total = as.integer(gsub(" p","",x[,3])),
            country = country
          )
          options(warn=0)
          rownames(x) <- NULL
          x <- x[order(x$week),]
        })))
        rownames(x) <- NULL
        x[is.na(x)] <- 0
        x['year'] <- as.numeric(substr(x$week,1,4))
        return(x)
      })
      return(data.frame())  
      
    }else if (v$dataLoadDownload==FALSE){
      
      try({
        return(data.frame())
      },silent=T)
      
    }
    return(data.frame())
  })
  
  
  # wyswietlenie danych w tabelce (dla pewnosci, ze sie pobralo dobrze)
  output$dataSample <- DT::renderDataTable({
    DT::datatable(  
      dataIn(), 
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 16,
        lengthMenu = seq(from=2,by=2,to=16) 
      )
    )
  })
  
  
  
  
  # wyswietlenie danych w tabelce
  output$tabelka <- DT::renderDataTable({
    
    # filtrowanie po roku i kraju
      if(nrow(dataIn())){
      data <- dataIn()
      data_subset <- subset(data, year == outVar$selectYearVar & country == outVar$selectCountryVar)
      
      # dodawanie kolumn
      updatedData <- reactive({
        if(outVar$selectGenderVar == "Kobieta"){
          data_subset$Plec <- "Kobieta"
          data_subset$liczba = data_subset$female
        }
        else{
          data_subset$Plec <- "Mezczyzna"
          data_subset$liczba = data_subset$male
        }
        return(data_subset)
      })
      
      # zostawianie wybranych kolumn
      kolumny <- c("country", "Plec", "week", "liczba")
      data_subset <- updatedData()
      data_subset <- data_subset[, kolumny]
      
      # zmiana nazw kolumn
      colnames(data_subset) <- c("Kraj", "Plec", "Tydzien", "Liczba")
      
      DT::datatable(
        data <- data_subset,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 16,
          lengthMenu = seq(from=2,by=2,to=16) 
        )
      )}
    else{return(NULL)}
  })
  
  # stworzenie i wyswietlenie mapy
  output$view <- renderGvis({
    if(nrow(dataIn())){
      geoData <- dataIn()
      geoDataAggFem <- aggregate(female ~ year+country, data = geoData, sum)
      colnames(geoDataAggFem) <-c('year','country','sum')
      geoDataAggMal <- aggregate(male ~ year+country, data = geoData, sum)
      colnames(geoDataAggMal) <-c('year','country','sum')
      gvisGeoChart(if(outVar$selectGenderVar == "Kobieta") subset(geoDataAggFem, year==outVar$selectYearVar) else subset(geoDataAggMal, year==outVar$selectYearVar), "country", 'sum',
                   options=list(region="150",
                                width=800, height=800))
    }else{
      return(NULL)
    }
  })
  
  # szereg czasowy
  output$timeSeriesPlot <- renderPlotly({
    
    # filtrowanie po roku
    if(nrow(dataIn())){
      data <- dataIn()
      data_subset <- subset(data, year == outVar$selectYearVar)
      
      # dodawanie kolumn
      updatedData <- reactive({
        if(outVar$selectGenderVar == "Kobieta"){
          data_subset$Plec <- "Kobieta"
          data_subset$liczba = data_subset$female
        }
        else{
          data_subset$Plec <- "Mezczyzna"
          data_subset$liczba = data_subset$male
        }
        return(data_subset)
      })
      
      dane <- updatedData()
      
      # filtrowanie po kraju
      
      dane <- subset(dane, country == outVar$selectCountryVar)
      
      wykresy <- lapply(unique(dane$country), function(country){
        dane_kraju <- dane[dane$country == country, ]
        plot_ly(data = dane_kraju, x = ~week, y = ~total, type = 'scatter', mode = 'lines', name = country)
        
      })
      
      subplot(wykresy, nrows = length(wykresy))}
    #plot <- plot_ly(data = dane, x = ~week, y = ~total, type = 'scatter', mode = 'lines')
    #plot %>% layout(xaxis = list(title = "Data"), yaxis = list(title = "Wartość"))
    else {return(NULL)}
    
  })
  
  
})

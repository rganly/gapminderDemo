library(gapminder)
library(dplyr)
library(ggplot2)
library(scales)
library(ggmap)
library(RColorBrewer)
library(DT)

#load gapminder data with lat and long co-ordinates
gapminder<-readRDS(file="./data/gapminderDemo.RDS")

#set consistenc colour palletes
continent<-unique(gapminder$continent)
pal <- colorFactor(brewer.pal(n=length(continent),name="Set2"), domain = continent)


server <- function(input, output,session) {
  
  # ---- MAKING A REACTIVE DATA SET ----
  #another reactive example here: https://shiny.rstudio.com/gallery/reactivity.html
  gapminder_filt<-reactive({
    df<-gapminder
    #filter by continent
    if(length(input$continent)>0){
      df<-df %>%
        filter(continent %in% input$continent)
    }
    #filter by country
    if(length(input$countries)>0){
      df<-df %>%
        filter(country %in% input$countries)
    }
    
    #filter by year
    if(!is.null(input$yearRange)){
      df <- df %>%
      filter(year > input$yearRange[1] & year < input$yearRange[2])
    }
    df
  })
  
  #reactive event to make the gganimate work better
  values<-reactiveValues(
    clickedMarker=NULL #store the gapminder plots 
  )
  
  
  #reactive events to clear clicked data
  # observe the marker click info and print to console when it is changed.
  observeEvent(input$leafletWorldMap_marker_click,{
    values$clickedMarker <- input$leafletWorldMap_marker_click
  })
  
  observeEvent(input$leafletWorldMap_click,{
    values$clickedMarker <- NULL
  })
  
  #---- GAP MINDER PLOT ---
  #renderPlot
  output$gapminderPlot<-renderPlot({
    df<-gapminder_filt() #I don't want to handle this reactively

    if(!is.null(values$clickedMarker)){
      tmp<-values$clickedMarker
      tmp2<-df %>% filter(lat == tmp$lat & lon == tmp$lng)
      if(nrow(tmp2)!=0)
        df<-tmp2
    }
    
    p<-df %>% 
      ggplot(aes(x = gdpPercap, y = lifeExp, group=country,col = continent, size = pop)) + 
      geom_point(alpha=0.7) +
      scale_color_brewer(palette="Set2",drop=FALSE)+
      theme_bw()
 
    if(input$axis=="Linear"){
      p<- p +
        scale_x_continuous(labels = scales::comma) + 
        labs(x = "GDP Per Capita", 
            y = "Life Expectancy", 
            col = "Continent", 
            size = "Population", 
            title = "Health and Wealth") 
    }else{
      p<- p +
        scale_x_log10(labels = scales::comma) + 
        labs(x = "log10(GDP Per Capita)", 
             y = "Life Expectancy", 
             col = "Continent", 
             size = "Population", 
             title = "Health and Wealth") 
    }
    
    if(input$showPath){
      p<-p+geom_path(alpha=0.5)
    }

    p
  })
  
  #leaflet world map
  output$leafletWorldMap<-renderLeaflet({
    
    df<-gapminder_filt()%>%
      select(country,continent,lon,lat,gdpPercap)%>%
      group_by(country,continent,lon,lat) %>%
      summarise(medianGDP = median(gdpPercap)) %>%
      ungroup() %>%
      mutate(popUpContent = paste(sep = "<br/>",
                                  "<b>",country,"</b>",
                                  "<b> Median GDP per Capita:</b>", dollar_format()(round(medianGDP,2))) )
  
    
    leaflet(df) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, weight=1,
                       radius=~log10(medianGDP)*1.25,stroke=FALSE,fillOpacity=0.7,
                       fillColor = ~pal(continent),popup = ~popUpContent)
    
  })
  
  #render a datatable
  output$table<-DT::renderDataTable({
    DT::datatable(gapminder_filt(),
      extensions="Buttons",
      options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      fixedHeader=TRUE
      )
    )
    })
  #---- AUTOMATICALLY RENDER WIDGET CONTENT ---
  #Automatically fill checkboxs of continents
  output$contientChecks<-renderUI({
    contients<-gapminder %>%
      select(continent) %>%
      unique()
    
    #create a checkbox
    checkboxGroupInput(inputId = "continent",
                       label = "Continent",
                       choices = contients$continent,
                       selected = contients$continent)
  })
  
  # Automatically fill content of a dropbox list
  # here a nice example of all the ways to implement
  # a dropdown box : https://shiny.rstudio.com/gallery/selectize-vs-select.html
  output$filterCountry<-renderUI({
    countries<-gapminder %>%
      filter(continent %in% input$continent) %>%
      select(country)%>%
      unique()
    
    #variable for the number of items to show in dropdown
    nShow = 10
    if(length(countries$country)<10){
      nShow <- length(countries$country)
    }
    
    #dropbox list, allow multiple options to be selected
    selectInput(inputId = "countries",
                label = "Select countries",
                choices = countries$country,
                selected = NULL,
                multiple = TRUE,
                selectize = FALSE,
                size = nShow)
  })
  
  #-----
  output$dateRange<-renderUI({
    minYear<-min(gapminder$year)
    maxYear<-max(gapminder$year)
    
    sliderInput(inputId = "yearRange",
                label = "Year Range",
                min = minYear,
                max = maxYear,
                step =1,
                sep="",
                value = c(minYear,maxYear),
                animate = FALSE)
  })
}
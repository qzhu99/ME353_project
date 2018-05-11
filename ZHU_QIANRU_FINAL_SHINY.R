library(shiny)
library(leaflet)
library(RColorBrewer)
datafinal<-read.csv("ZHU_QIANRU_FINAL_SHINY_091615.csv")
datafinal<-datafinal[1:2000,]
#datafinal<-datafinal[abs(datafinal$tippercentage-mean(datafinal$tippercentage)) <= sd(datafinal$tippercentage),]
ui <- bootstrapPage(
  title = "NYC Green Taxi Trips 09/16/15",
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "70%", height = "70%"),
  absolutePanel(top = 10, right = 10,
               # sliderInput("range", "Tip Percentage", min(datafinal$tippercentage), max(datafinal$tippercentage),
                      #      value = range(datafinal$tippercentage), step = 0.1
                #),
                sliderInput("range_tripdistance","Trip Distance", min(datafinal$tripdistance), max(datafinal$tripdistance),
                            value = range(datafinal$tripdistance),step =0.1
                ),
                sliderInput("range", "Total Amount", min(datafinal$totalamount), max(datafinal$totalamount),
                            value = range(datafinal$totalamount), step = 0.5
                ),
               sliderInput("range_passenger",label = "Passengers",
                           value = range(datafinal$passengercount), 
                           min = min(datafinal$passengercount), 
                           max = max(datafinal$passengercount), 
                           step = 1),
                checkboxGroupInput(inputId = "paymenttype", label = "Payment Type", 
                                   choices = list("Credit Card" = 1, "Cash" = 2, "No Charge" = 3, "Dispute" = 4,"Unknown" = 5)),
                
                
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # use the reactive to enable the interactivity of the map based on the user's choice of the data 
  filteredData <- reactive({
   # datafinal[datafinal$tippercentage >= input$range[1] & datafinal$tippercentage <= input$range[2],]
    subset(datafinal,totalamount >=input$range[1] & totalamount <= input$range[2] &
             tripdistance >= input$range_tripdistance[1] &tripdistance <= input$range_tripdistance[2] &
             passengercount>=input$range_passenger[1] & passengercount<=input$range_passenger[2] &
             payment %in% input$paymenttype)
    
    })
         
            
 #enable the choice of color of the user's preference
  colorpal <- reactive({
    colorNumeric(input$colors, datafinal$tippercentage)
  })
  
  output$map <- renderLeaflet({
   #set up bounds for the default view of the map based on the location data read from the data
    leaflet(datafinal) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  

  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~tippercentage*30, weight = 1, color = "#666666",
                 fillColor = ~pal(tippercentage), fillOpacity = 0.7, popup = ~paste(tippercentage)
      )
  })
  
  # choice for the legend
  observe({
    proxy <- leafletProxy("map", data = datafinal)
    
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~tippercentage, title ="Tip Percentage %"
      )
    }
  })
  
}

shinyApp(ui, server)

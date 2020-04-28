server <- function(input, output, session) {
  # reactive values
  values <- reactiveValues(updateCalculations = 1)

  # reactive data
  openZHdata <- reactive({
    openZH <- getSwissDataFromOpenZH()
    # do we have older data
    oldData <- file.exists("data/openZHdata.Rd")
    # compare data to previous data if it exists
    if(oldData == TRUE){
      oldOpenZH <- load(file = "data/openZHdata.Rd")
      updateData <- isFALSE(compare(openZH, oldOpenZH))
    } else {
      updateData <- TRUE
    }
    
    if(updateData){
      # update file
      save(openZH, file = "data/openZHdata.Rd")
      # redo calculations
      if(values$updateCalculations == 1){
        values$updateCalculations <- 2
      } else {
        values$updateCalculations <- 1
      }
      print("Updating Data & redoing calculations")
    } else {
      print("now new data found")
    }
    return(openZH)
  })

  # calculate rEff
  calcOutput <- reactive({
    values$updateCalculations
  })

  # observers
  observeEvent(input$goButton, {
    if(input$goButton > 0){
      if(values$updateCalculations == 1){
        values$updateCalculations <- 2
      } else {
        values$updateCalculations <- 1
      }
    }
  })

  observeEvent(input$methodsLink, {
    updateTabItems(session, "tabs", selected = "methods")
  })
  

  # outputs
  output$lastUpdateBox <- renderInfoBox({
    infoBox(
      "Last Data Update", max(openZHdata()$date), icon = icon("exclamation-circle"),
      color = "purple"
    )
  })

  output$swissPlot <- renderPlot({
    plotData <- filter(openZHdata(), region == input$canton, data_type == "confirmed", variable == "cumul")
    plot <- ggplot(
      data = plotData,
      mapping = aes(date, value)) +
      geom_line()

    return(plot)
  })

  output$test <- renderText({
    values$updateCalculations
  })
}

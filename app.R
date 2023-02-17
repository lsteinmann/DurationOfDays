#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  titlePanel("Duration of Days: Calculate the amount of days in a given time span"),
  
  p("This app is meant to help you calculate how many days have passed during 
  two different dates (including the dates themselfes) for multiple date ranges. 
  This is helpful for getting an idea about your visa allowance."),
  
  
  sidebarLayout(
    sidebarPanel(
      p("Enter the dates of entry and exit of your relevant stays here and submit:"),
      dateRangeInput(inputId = "stayDatesInput",
                     label = "Date of Entry and Exit"),
      actionButton(inputId = "storeStayDates", label = "Add stay to list")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("staysTable_out")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      numericInput(inputId = "lastNdays", value = 180, label = "Relevant time span (in days)"),
      numericInput(inputId = "permDays", value = 90, label = "Number of days allowed to stay in time span"),
      dateInput(inputId = "plannedEntryInput",
                     label = "Planned Date of Entry")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Result"),
      textOutput("controls"),
      h3(textOutput("allowedStay"))
    )
  ),
  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = "clearAll", label = "Reset")
    ),
    mainPanel(
    )
  ),
  h2("Warning"),
  p("This app is meant as guidance. I do not guarentee that this is correct 
        or accepted by the relevant authorities. When staying in a foreign 
        country, always be sure to have a few 'days to spare' in case something
        unexpected happens."),
  h3("For a proper assessment of your situation, always contact the relevant 
    immigration authorities!")
  
)
server <- function(input, output, session) {
  
  currentInput <- reactiveVal()
  
  staysList <- reactiveVal()
  totalDays <- reactiveVal()
  staysTable <- reactiveVal()
  
  observeEvent(input$clearAll, { 
    staysList(NULL)
    totalDays(NULL)
    staysTable(NULL)
    updateNumericInput(session, "lastNdays", value = 180)
    updateTextInput(session, "permDays", value = 90)
  })
  
  observeEvent(input$stayDatesInput, { 
    currentInput(input$stayDatesInput)
  })
  
  observeEvent(input$storeStayDates, {
    currentStays <- staysList()
    newStay <- list(currentInput())
    
    new_staysList <- append(currentStays, newStay, after = length(currentStays))
    
    #new_staysList <- list(c(as.Date("2022-11-01"), as.Date("2022-11-10")), c(as.Date("2023-01-01"), as.Date("2023-01-10")))
    
    new_staysTable <- as.data.frame(matrix(ncol = 3, 
                                           nrow = length(new_staysList)))
    colnames(new_staysTable) <- c("Entry Date", "Exit Date", "Number of Days")
    
    new_staysTable$`Entry Date` <- lapply(new_staysList, function(x) format(x[[1]]))
    new_staysTable$`Exit Date` <- lapply(new_staysList, function(x) format(x[[2]]))
    
    days <- lapply(new_staysList, FUN = function(x) seq.Date(x[1], x[2], by = 1))
    new_totalDays <- as.Date(unlist(days), origin = "1970-01-01")
    
    days <- lapply(days, length)
    
    new_staysTable$`Number of Days` <- unlist(days)
    
    #seq.Date(x[1], x[2], by = 1)
    totalDays(new_totalDays)
    staysTable(new_staysTable)
    staysList(new_staysList)
  })
  
  output$staysTable_out <- renderTable({
    staysTable()
  })
  
  output$days <- renderTable({
    staysTable()
  })
  
  output$controls <- renderText({
    x_days_ago <- input$plannedEntryInput-input$lastNdays
    
    paste(sep = "", input$lastNdays, " days before your planned entry date was ", 
          format(x_days_ago, format = "%A, %d.%m.%Y"), ". ", 
          "Assuming you are allowed to stay ", input$permDays, 
          " within the last ", input$lastNdays, " days. ")
  })
  
  output$allowedStay <- renderText({
    
    days <- totalDays()
    date_of_entry <- input$plannedEntryInput
    spent_days <- length(days[which(days > date_of_entry-input$lastNdays)])
    allowed_days <- input$permDays-spent_days
    end_day <- date_of_entry + allowed_days
    paste(sep = "", 
          "When entering on ", format(date_of_entry, format = "%A, %d.%m.%Y"),
          " you are allowed to stay ", allowed_days, " days ",
          "until ", format(end_day, format = "%A, %d.%m.%Y"), ". ")
  })
  
  
}
shinyApp(ui = ui, server = server)

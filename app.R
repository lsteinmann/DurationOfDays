#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

page <- fluidPage(
  fluidRow(
    column(9,
           h1("Duration of Days: Calculate the Permissible Amount of Days 
              in a Time Span"),
           p("In many countries, you can stay without a visa or residence 
           permit for a duration of 90 days. In a lot of cases, this duration 
           is reset after 180 days have passed. It is often hard to calculate 
           this using a calendar alone. You can get a rough estimate by counting 
           the days you have spent in the country in question in the last 6 month. 
           Usually, the number of days you are allowed to stay is calculated on 
           the date of entry and does not reset thereafter."),
           p("This app helps you calculate the number of days during 
             multiple stays and get a sum of days you have spent. 
             The maximum number of days as well as the total duration in 
             which days should be counted can be set (with the defaults 
             being 90 within the last 180 days)."),
           hr())
  ),
  fluidRow(column(9, h2("Settings"))),
  fluidRow(
    column(3, style = "margin-top: 15pt;", 
           dateInput(inputId = "plannedEntryInput",
                     label = "Planned Date of Entry",
                     width = "100%")
    ),
    column(3, 
           numericInput(inputId = "lastNdays", 
                        value = 180, 
                        label = "Relevant time span (in days)",
                        width = "100%")
    ),
    column(3, 
           numericInput(inputId = "permDays", 
                        value = 90, 
                        label = "Number of permissible days in time span",
                        width = "100%")
    )
  ),
  fluidRow(
    column(9,
           htmlOutput("controls")
    )
  ),
  fluidRow(
    column(9,
           hr(),
           h2("Previous Durations / Stays")
    )
  ),
  fluidRow(
    column(6,
           #p("Enter the dates of entry and exit of your relevant stays here and submit:"),
           dateRangeInput(inputId = "stayDatesInput",
                          label = "Date of Entry and Exit",
                          width = "100%"),
           style = "margin-top:5px;"
           ),
    column(3,
           actionButton(inputId = "storeStayDates", 
                        label = "Add to list", 
                        class="btn btn-success",
                        width = "100%"),
           style = "margin-top:25px;"
           )
  ),
  fluidRow(
    column(9, 
           tableOutput("staysTable_out"), 
           hr()
    )
  ),
  fluidRow(
    column(9,
           h2("Result"),
           h4(htmlOutput("allowedStay", class = "alert alert-success"))
    )
  ),
  fluidRow(
    column(2),
    column(5, 
           actionButton(inputId = "clearAll", 
                        label = "Reset all values to default", 
                        icon = icon("rotate"), 
                        class = "btn-warning",
                        width = "100%")
    ),
    column(2)
  ),
  fluidRow(
    column(9,
           hr(),
           h2("Warning"),
           p("This app is meant as guidance. I do not guarantee that this is 
           correct or accepted by the relevant authorities. When staying in 
           a foreign country, always be sure to know the laws applicable in 
           your situation and to have a few 'days to spare' in case something 
           unexpected happens."),
           h3("For a proper assessment of your situation, always contact 
           the relevant immigration authorities!", 
              class="alert alert-danger"))
  ),
  fluidRow(
    column(5),
    column(4,
           class = "alert alert-dark",
           tags$div(
             "Contact me and/or get the code for this app ",
             tags$a(href="https://github.com/lsteinmann/DurationOfDays", 
                    "on GitHub!"),
             icon("github"),
             align = "right"
           )
    )
  )
)


# copied from https://community.rstudio.com/t/shiny-how-to-center-and-fix-width-of-dashboard/3575/5
ui <- tagList(
  tags$style("html,body{background-color: white;}
                .container{
                    width: 100%;
                    margin: 0 auto;
                    padding: 0;
                }
               @media screen and (min-width: 900px){
                .container{
                    width: 800px;
                }
               }"),
  tags$div(class="container",
           page)
  )


server <- function(input, output, session) {
  
  currentInput <- reactiveVal()
  
  staysList <- reactiveVal()
  totalDays <- reactiveVal()
  
  observeEvent(input$clearAll, { 
    staysList(NULL)
    totalDays(NULL)
    updateNumericInput(session, "lastNdays", value = 180)
    updateTextInput(session, "permDays", value = 90)
  })
  
  observeEvent(input$stayDatesInput, { 
    currentInput(input$stayDatesInput)
  })
  
  observeEvent(input$storeStayDates, {
    currentStays <- staysList()
    newStay <- list(currentInput())
    
    new_staysList <- append(currentStays, 
                            newStay, 
                            after = length(currentStays))
    
    days <- lapply(new_staysList, function(x) seq.Date(x[1], x[2], by = 1))
    new_totalDays <- as.Date(unlist(days), origin = "1970-01-01")
    
    totalDays(new_totalDays)
    staysList(new_staysList)
  })
  
  staysTable <- reactive({
    
    validate(need(staysList(), "Enter the dates of your previous stays."))
    
    new_staysTable <- as.data.frame(matrix(ncol = 4, 
                                           nrow = length(staysList())))
    
    colnames(new_staysTable) <- c("Entry Date", "Exit Date", 
                                  "Number of Days", "Counted Days")
    
    new_staysTable$`Entry Date` <- lapply(staysList(), 
                                          function(x) 
                                            format(x[[1]], 
                                                   format = "%A, %d.%m.%Y"))
    
    new_staysTable$`Exit Date` <- lapply(staysList(), 
                                         function(x) 
                                           format(x[[2]], 
                                                  format = "%A, %d.%m.%Y"))
    
    days <- lapply(staysList(), function(x) seq.Date(x[1], x[2], by = 1))
    relevant_days <- lapply(days, function(x) sum(x > x_days_ago()))
    days <- lapply(days, length)
    
    new_staysTable$`Number of Days` <- unlist(days)
    new_staysTable$`Counted Days` <- as.integer(relevant_days)
    
    return(new_staysTable)
  })
  
  output$staysTable_out <- renderTable(staysTable(), 
                                       width = "100%",
                                       striped = TRUE,
                                       hover = TRUE)
  
  x_days_ago <- reactiveVal()
  
  output$controls <- renderText({
    x_days_ago(input$plannedEntryInput-input$lastNdays)
    
    paste(sep = "", 
          "Assuming you are allowed to stay ", input$permDays, 
          " within the last ", input$lastNdays, " days. ",
          input$lastNdays, " days before your planned entry date was <b>", 
          format(x_days_ago(), format = "%A, %d.%m.%Y"), "</b>. ",
          "Make sure to enter all the relevant stays from that date onwards 
          in the following 'List of Durations / Stays':")
  })
  
  output$allowedStay <- renderText({
    
    days <- totalDays()
    date_of_entry <- input$plannedEntryInput
    spent_days <- length(days[which(days > date_of_entry-input$lastNdays)])
    allowed_days <- input$permDays-spent_days
    end_day <- date_of_entry + allowed_days
    paste(sep = "", 
          "When entering on ", format(date_of_entry, format = "%A, %d.%m.%Y"),
          " you are allowed to stay <b>", allowed_days, " days</b> ",
          "until <b>", format(end_day, format = "%A, %d.%m.%Y"), "</b>. ")
  })
  
  
}
shinyApp(ui = ui, server = server)

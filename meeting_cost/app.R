library(shiny)
library(readr)

# Define UI
ui <- fluidPage(
  titlePanel("Meeting Time & Cost Tracker"),
  sidebarLayout(
    sidebarPanel(
      textInput("meeting_title", "Enter Meeting Title:", ""),
      dateInput("meeting_date", "Select Meeting Date:", value = Sys.Date()),
      fileInput("csv_file", "Upload CSV File with 'name' and 'rates':", accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
      uiOutput("person_selection"),
      p("Click the Start/Stop button below to ",strong("Start and Stop")," the meeting timer"),
      actionButton("toggle_timer", "Start Timer"),
      actionButton("reset", "Reset")
      ),
    mainPanel(
      p("Upload a CSV file containing names and day rates, then use the controls to track time and calculate costs."),
      h3("Meeting Details:"),
      verbatimTextOutput("meeting_info"),
      h3("Start Time:"),
      verbatimTextOutput("start_time"),
      h3("Elapsed Time (Stopwatch):"),
      verbatimTextOutput("stopwatch"),
      h3("Stop Time:"),
      verbatimTextOutput("stop_time"),
      h3("'Opportunity cost' of the meeting:"),
      verbatimTextOutput("total_cost"),
      uiOutput("conditional_download")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive values for data and calculations
  values <- reactiveValues(
    data = NULL,
    start_time = NULL,
    stop_time = NULL,
    costs = NULL,
    elapsed = NULL,
    timer_running = FALSE
  )
  
  # Load CSV data
  observeEvent(input$csv_file, {
    req(input$csv_file)
    values$data <- read_csv(input$csv_file$datapath)
  })
  
  # Generate person selection UI dynamically
  output$person_selection <- renderUI({
    req(values$data)
    selectInput(
      "selected_people",
      "Select People:",
      choices = values$data$name,
      multiple = TRUE
    )
  })
  
  # Toggle timer functionality
  observeEvent(input$toggle_timer, {
    req(values$data)
    if (!values$timer_running) {
      # Start timer
      values$start_time <- if (is.null(values$start_time)) Sys.time() else values$start_time
      values$timer_running <- TRUE
      updateActionButton(session, "toggle_timer", label = "Stop Timer")
    } else {
      # Stop timer
      values$stop_time <- Sys.time()
      elapsed_time <- as.numeric(difftime(values$stop_time, values$start_time, units = "secs"))
      values$elapsed <- elapsed_time
      values$timer_running <- FALSE
      updateActionButton(session, "toggle_timer", label = "Start Timer")
    }
  })
  
  # Reset functionality
  observeEvent(input$reset, {
    values$start_time <- NULL
    values$stop_time <- NULL
    values$costs <- NULL
    values$elapsed <- NULL
    values$timer_running <- FALSE
    updateActionButton(session, "toggle_timer", label = "Start Timer")
  })
  
  
  # Reactive timer to update costs and stopwatch in real-time
  auto_update <- reactiveTimer(1000)  # Updates every 1 second
  observe({
    auto_update()
    if (values$timer_running) {
      req(values$start_time, input$selected_people)
      elapsed_time <- Sys.time() - values$start_time
      values$elapsed <- round(as.numeric(elapsed_time, units = "secs"), 0) # Elapsed time in seconds
      
      elapsed_hours <- as.numeric(difftime(Sys.time(), values$start_time, units = "hours"))
      selected <- input$selected_people
      if (!is.null(selected) && length(selected) > 0) {
        rates <- values$data[values$data$name %in% selected, ]$rate
        values$costs <- setNames(elapsed_hours * rates / 7.5, selected) # Assuming 7.5-hour workday
      }
    }
  })
  
  # Output meeting details
  output$meeting_info <- renderText({
    paste("Meeting Title:", input$meeting_title, "\nMeeting Date:", input$meeting_date)
  })
  
  output$start_time <- renderText({
    if (is.null(values$start_time)) {
      "Start Time has not been recorded yet."
    } else {
      paste("Start Time:", format(values$start_time, "%Y-%m-%d %H:%M:%S"))
    }
  })
  
  # Output elapsed time (stopwatch)
  output$stopwatch <- renderText({
    if (is.null(values$start_time) || !values$timer_running) {
      "00:00:00"
    } else {
      # Format elapsed time as HH:MM:SS
      secs <- values$elapsed %% 60
      mins <- (values$elapsed %/% 60) %% 60
      hours <- values$elapsed %/% 3600
      sprintf("%02d:%02d:%02d", hours, mins, secs)
    }
  })
  
  # Output stop time
  output$stop_time <- renderText({
    if (is.null(values$stop_time)) {
      "Stop Time has not been recorded yet."
    } else {
      paste("Stop Time:", format(values$stop_time, "%Y-%m-%d %H:%M:%S"))
    }
  })
  
  # Output total costs for all staff
  output$total_cost <- renderText({
    if (is.null(values$costs)) {
      "Total cost has not been calculated yet."
    } else {
      total_cost <- sum(values$costs)
      paste("Total Cost for All Staff: £", round(total_cost, 2))
    }
  })
  
  # Conditionally display the download button
  output$conditional_download <- renderUI({
    req(values$stop_time) # Only render the button if the stop time is set
    tagList(
      downloadButton("export_csv", "Export CSV")
    )
  })
  
  # Export CSV functionality
  output$export_csv <- downloadHandler(
    filename = function() {
      paste0("Meeting_Report_", Sys.Date(), ".csv")  # Ensure proper CSV extension
    },
    content = function(file) {
      req(values$data, values$costs, values$elapsed)
      
      # Construct the export data
      selected <- input$selected_people
      rates <- values$data[values$data$name %in% selected, ]$rate
      
      data <- data.frame(
        name = selected,
        rate = rates,
        MeetingTitle = input$meeting_title,
        MeetingDate = input$meeting_date,
        ElapsedTime = sprintf("%02d:%02d:%02d", values$elapsed %/% 3600, (values$elapsed %/% 60) %% 60, values$elapsed %% 60),
        Cost = round(values$costs, 2)
      )
      
      # Write the data to the file path passed by Shiny
      write.csv(data, file, append = FALSE, row.names = FALSE)
    },
    contentType = "text/comma-separated-values"
  )
}

# Run the application
shinyApp(ui = ui, server = server)

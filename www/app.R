library(tidyverse)
library(ggplot2)
library(DT)
library(shiny)
library(glue)


ui <- fluidPage(
  # Link to CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      tags$h3("R Shiny Task Scheduling"),
      tags$hr(),
      textInput(inputId = "inTaskName", label = "Task:", placeholder = "e.g., Marketing"),
      dateInput(inputId = "inStartDate", value = Sys.Date(), min = Sys.Date(), label = "Start Date:"),
      dateInput(inputId = "inEndDate", value = Sys.Date() + 10, min = Sys.Date() + 1, label = "End Date:"),
      actionButton(inputId = "btn", label = "Add Task")
    ),
    mainPanel(
      # Surround the elements with a DIV element that has a class name of "card"
      tags$div(
        class = "card",
        tags$h3("Task Table View"),
        tags$hr(),
        DTOutput(outputId = "tableTasks")
      ),
      # Surround the elements with a DIV element that has a class name of "card"
      tags$div(
        class = "card",
        tags$h3("Task Chart View"),
        tags$hr(),
        plotOutput(outputId = "plotTasks")
      )
    )
  )
)


server <- function(input, output) {
  df <- reactiveValues(
    data = data.frame(
      Task = c("Research", "Clinical Trials", "Regulatory Approval"),
      StartDate = as.Date(c("2023-05-01", "2023-07-01", "2024-01-01")),
      EndDate = as.Date(c("2023-06-30", "2023-12-31", "2024-06-30"))
    ) %>%
      mutate(ID = row_number(), .before = Task) %>%
      mutate(
        Remove = glue('<button id="custom_btn" onclick="Shiny.onInputChange(\'button_id\', \'{ID}\')">Remove</button>')
      )
  )
  
  observeEvent(input$btn, {
    task_name <- input$inTaskName
    task_start_date <- input$inStartDate
    task_end_date <- input$inEndDate
    
    if (!is.null(task_name) && !is.null(task_start_date) && !is.null(task_end_date)) {
      new_id <- max(df$data$ID) + 1
      new_row <- data.frame(
        ID = new_id,
        Task = task_name,
        StartDate = task_start_date,
        EndDate = task_end_date,
        Remove = glue('<button id="custom_btn" onclick="Shiny.onInputChange(\'button_id\', \'{new_id}\')">Remove</button>'),
        stringsAsFactors = FALSE
      )
      df$data <- rbind(df$data, new_row)
      df$data <- df$data[order(df$data$ID), ]
    }
  })
  
  observeEvent(input$button_id, {
    output$text <- renderText(glue("Row number {input$button_id} is selected recently"))
    df$data <- df$data[-c(as.integer(input$button_id)), ]
    df$data <- df$data[order(df$data$StartDate), ]
  })
  
  output$tableTasks <- renderDT({
    datatable(data = df$data, escape = FALSE)
  })
  
  output$plotTasks <- renderPlot({
    ggplot(df$data, aes(x = StartDate, xend = EndDate, y = fct_rev(fct_inorder(Task)), yend = Task)) +
      geom_segment(linewidth = 10, color = "#0198f9") +
      labs(
        title = "Pharma Company Gantt Chart",
        x = "Duration",
        y = "Task"
      ) +
      theme_bw() +
      theme(legend.position = "none") +
      theme(
        plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 45)
      )
  })
}


shinyApp(ui = ui, server = server)

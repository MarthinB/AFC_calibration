
library(shiny)
library(plotly)
library(changepoint)
library(readr)
library(dplyr)

# Author: Manon Dijoux 
# The aim of this app is to extract peaks of LGR measurements from LGR data


# UI
ui <- fluidPage(
  titlePanel("LGR- peak detection"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose Text File', accept = c(".txt")),
      actionButton("process", "Process Data"),
      hr(),
      h3("Adjust Selected Point"),
      actionButton("prev_point", "Previous Point"),
      actionButton("next_point", "Next Point"),
      actionButton("confirm_point", "Confirm Point"),
      hr(),
      h3("Selected Peak Information"),
      textOutput("selected_peak_info_ui"),
      hr(),
      h3("Selected Peaks"),
      verbatimTextOutput("selected_peaks"),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      plotlyOutput("methanePlot", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Function to read and process the file
  data <- reactive({
    req(input$file1)
    df <- read_delim(input$file1$datapath, delim = ",", skip = 5, col_names = FALSE) # Read file and skip first 5 lines
    
    # Select and rename the relevant columns
    df <- df %>%
      select(DateTime = X2, Concentration = X3)
    
    # Check and convert DateTime column to POSIXct
    df$DateTime <- as.POSIXct(df$DateTime, format = "%m/%d/%Y %H:%M:%OS", tz = "GMT")
    if (any(is.na(df$DateTime))) {
      stop("DateTime column contains invalid data.")
    }
    
    # Convert Concentration from scientific notation to numeric
    df$Concentration <- as.numeric(df$Concentration)
    
    df
  })
  
  # Function to process the data and detect change points
  processed_data <- eventReactive(input$process, {
    req(data())
    df <- data()
    
    # Detect change points using changepoint package
    chgpt <- cpt.mean(df$Concentration, method = "PELT", penalty = "SIC")
    chgpts <- cpts(chgpt)
    
    df$ChangePoint <- 0
    df$ChangePoint[chgpts] <- 1
    
    df
  })
  
  selected_peaks <- reactiveVal(data.frame(DateTime = character(), Concentration = numeric(), stringsAsFactors = FALSE))
  selected_point <- reactiveVal(NULL)
  zoom_ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Render the methane concentration plot
  output$methanePlot <- renderPlotly({
    req(processed_data())
    df <- processed_data()
    
    p <- plot_ly(df, x = ~DateTime, y = ~Concentration, type = 'scatter', mode = 'lines+markers',
                 text = ~paste("Date:", DateTime, "<br>Concentration:", Concentration),
                 hoverinfo = 'text', source = "A") %>%
      layout(title = 'Methane Concentration Over Time',
             xaxis = list(title = 'Time', rangeslider = list(visible = TRUE)),
             yaxis = list(title = 'Concentration'))
    
    if (!is.null(selected_point())) {
      selected <- selected_point()
      p <- p %>%
        add_trace(x = selected$DateTime, y = selected$Concentration, type = 'scatter', mode = 'markers',
                  marker = list(color = 'red', size = 10), showlegend = FALSE, hoverinfo = 'text',
                  text = ~paste("Date:", selected$DateTime, "<br>Concentration:", selected$Concentration))
    }
    
    if (!is.null(zoom_ranges$x) && !is.null(zoom_ranges$y)) {
      p <- p %>% layout(xaxis = list(range = zoom_ranges$x),
                        yaxis = list(range = zoom_ranges$y))
    }
    
    event_register(p, 'plotly_click')
    
    p
  })
  
  # Display selected peak information
  output$selected_peak_info_ui <- renderText({
    req(selected_point())
    selected_row <- selected_point()
    paste("Date:", format(selected_row$DateTime, "%m/%d/%Y %H:%M:%OS"),
          "Concentration:", selected_row$Concentration)
  })
  
  # Handle click event on the plot
  observeEvent(event_data("plotly_click", source = "A"), {
    click_data <- event_data("plotly_click", source = "A")
    req(click_data)
    
    df <- processed_data()
    click_time <- as.POSIXct(click_data$x, origin = "1970-01-01", tz = "GMT")
    idx <- which(df$DateTime == click_time)
    
    if (length(idx) > 0) {
      selected_row <- df[idx, ]
      selected_point(data.frame(DateTime = selected_row$DateTime, Concentration = selected_row$Concentration))
    }
  })
  
  # Navigate to the previous point
  observeEvent(input$prev_point, {
    if (!is.null(selected_point())) {
      df <- processed_data()
      current_time <- selected_point()$DateTime
      current_idx <- which(df$DateTime == current_time)
      
      if (length(current_idx) > 0 && current_idx > 1) {
        selected_row <- df[current_idx - 1, ]
        selected_point(data.frame(DateTime = selected_row$DateTime, Concentration = selected_row$Concentration))
      }
    }
  })
  
  # Navigate to the next point
  observeEvent(input$next_point, {
    if (!is.null(selected_point())) {
      df <- processed_data()
      current_time <- selected_point()$DateTime
      current_idx <- which(df$DateTime == current_time)
      
      if (length(current_idx) > 0 && current_idx < nrow(df)) {
        selected_row <- df[current_idx + 1, ]
        selected_point(data.frame(DateTime = selected_row$DateTime, Concentration = selected_row$Concentration))
      }
    }
  })
  
  # Confirm the selected point as a peak
  observeEvent(input$confirm_point, {
    if (!is.null(selected_point())) {
      peaks <- selected_peaks()
      new_peak <- selected_point()
      
      if (!any(peaks$DateTime == new_peak$DateTime)) {
        selected_peaks(rbind(peaks, new_peak))
      }
      
      cat("Confirmed Point Information:\n")
      cat("DateTime: ", format(new_peak$DateTime, "%d/%m/%Y %H:%M:%OS"), "\n")
      cat("Concentration: ", new_peak$Concentration, "\n\n")
    }
  })
  
  # Handle zoom events on the plot
  observeEvent(event_data("plotly_relayout", source = "A"), {
    zoom_data <- event_data("plotly_relayout", source = "A")
    if (!is.null(zoom_data$`xaxis.range[0]`) && !is.null(zoom_data$`xaxis.range[1]`)) {
      zoom_ranges$x <- c(zoom_data$`xaxis.range[0]`, zoom_data$`xaxis.range[1]`)
    }
    if (!is.null(zoom_data$`yaxis.range[0]`) && !is.null(zoom_data$`yaxis.range[1]`)) {
      zoom_ranges$y <- c(zoom_data$`yaxis.range[0]`, zoom_data$`yaxis.range[1]`)
    }
  })
  
  # Display the selected peaks
  output$selected_peaks <- renderPrint({
    req(selected_peaks())
    print(selected_peaks())
  })
  
  # Download handler for selected peaks
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("selected_peaks", ".csv", sep = "")
    },
    content = function(file) {
      peaks <- selected_peaks()
      
      # Sort peaks by DateTime and add Stage numbers
      peaks <- peaks %>% arrange(DateTime)
      peaks$Stage <- seq_len(nrow(peaks))
      
      # Write the data to CSV
      write.table(peaks, file, sep = ",", row.names = FALSE, col.names = c("DateTime", "CH4_ppm", "Stage"), quote = FALSE)
    }
  )
}

# App
shinyApp(ui = ui, server = server)




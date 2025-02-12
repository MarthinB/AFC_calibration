library(shiny)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(tidyr)

# GHG Low Cost Sensors - Data Visualization 
# by Tuba Bucak - modified from KennetTM FluxBandit app


version <- "GHG Low-Cost Sensor Graphs"
options(shiny.maxRequestSize=50*1024^2)

ui <- fluidPage(
  
  titlePanel(version),
  
  em("'This app is for visualizing GHG sensor data'"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$b(" Upload SD card data (Multiple files can be uploaded)"),
      
      fileInput("file", "",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      
      sliderInput("range", "",
                  ymd_hms("2022-09-01 00:00:00"),
                  ymd_hms("2022-09-22 12:00:00"),
                  c(ymd_hms("2022-09-01 00:00:00"), 
                    ymd_hms("2022-09-22 12:00:00")),
                  60*60*24, 
                  timezone="+0000"),
      
      tags$hr(),
      
      
      tags$br(),
      
    ),
    
    mainPanel(
      
      tags$b("CH4 (mv) /CO2 (ppm)"),
      p("Use slider to adjust the x-axis"),
      
      plotOutput("plot",
                 brush = brushOpts(
                   id = "plot_brush",
                   resetOnNew = TRUE
                 )),
      
      tags$b("Temperature/Humidity"),
      p("Use slider to adjust the x-axis"),
      
      plotOutput("plot2",
                 brush = brushOpts(
                   id = "plot_brush",
                   resetOnNew = TRUE
                 )),
      
      
    )
    
  )
)

server <- function(input, output, session){
  data <- reactive({
    req(input$file)
    temp = lapply(input$file$datapath, function(i){
      read.csv(i, header=FALSE)
    })
    
    df_temp = rbindlist(temp)
    colnames(df_temp) <- c("Cycle", "CH4_1", "CH4_2", "CO2", "Temperature", "Humidity", "Atm_Pres", "CH4_ppm", "Time")
    df <- df_temp %>%
      mutate(datetime = dmy_hms(Time)) %>%
      select(datetime, co2 = CO2, ch4 = CH4_1, airt = Temperature, water = Humidity)
    
    time_start <- min(df$datetime, na.rm = TRUE)
    time_end <- max(df$datetime, na.rm = TRUE)
    
    updateSliderInput(session, "range", value = c(time_start, time_end),
                      min = time_start, max = time_end, step = 60)
    
    return(df)
  })
  
  output$plot <- renderPlot({
    req(input$file)
    
    data_subset <- data() %>%
      filter(between(datetime, input$range[1], input$range[2]))
    
    # Ensure data_subset is not empty
    if (nrow(data_subset) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for the selected range", cex = 1.2)
      return()
    }
    
    # Ensure no NA or infinite values
    data_subset <- data_subset %>%
      filter(is.finite(co2), is.finite(ch4), !is.na(co2), !is.na(ch4))
    
    # Base plot for CO2
    plot(x = data_subset$datetime,
         y = data_subset$co2,
         ylab = expression("CO"[2]*" (ppm)"), xlab = "Datetime",
         main = "Overview Plot: CO2 and CH4",
         pch = 16, col = "blue",
         panel.first = grid())
    
    co2_min <- min(data_subset$co2, na.rm = TRUE)
    co2_max <- max(data_subset$co2, na.rm = TRUE)
    ch4_min <- min(data_subset$ch4, na.rm = TRUE)
    ch4_max <- max(data_subset$ch4, na.rm = TRUE)
    ch4_scaled <- (co2_max - co2_min) * ((data_subset$ch4 - ch4_min) / (ch4_max - ch4_min)) + co2_min
    
    ch4_labels <- pretty(data_subset$ch4)
    ch4_at <- (co2_max - co2_min) * ((ch4_labels - ch4_min) / (ch4_max - ch4_min)) + co2_min
    
    # Adding CH4 points
    points(x = data_subset$datetime, y = ch4_scaled, col = "coral", pch = 17)
    axis(4, at = ch4_at, labels = ch4_labels, col = "coral", col.ticks = "coral", col.axis = "coral")
    
    # Adding a legend
    legend("topright", legend = c(expression("CO"[2]), expression("CH"[4])), 
           col = c("blue", "coral"), pch = c(16, 17), box.lty = 0)
  })
  
  output$plot2 <- renderPlot({
    req(input$file)
    
    data_subset <- data() %>%
      filter(between(datetime, input$range[1], input$range[2]))
    
    # Ensure data_subset is not empty
    if (nrow(data_subset) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for the selected range", cex = 1.2)
      return()
    }
    
    # Ensure no NA or infinite values
    data_subset <- data_subset %>%
      filter(is.finite(airt), is.finite(water), !is.na(airt), !is.na(water))
    
    # Base plot for Temperature
    plot(x = data_subset$datetime,
         y = data_subset$airt,
         ylab = "Temperature (°C)", xlab = "Datetime",
         main = "Overview Plot: Temperature and RH",
         type = "o", pch = 16, col = "darkgreen", lwd = 2,
         panel.first = grid())
    
    Temp_min <- min(data_subset$airt, na.rm = TRUE)
    Temp_max <- max(data_subset$airt, na.rm = TRUE)
    RH_min <- min(data_subset$water, na.rm = TRUE)
    RH_max <- max(data_subset$water, na.rm = TRUE)
    RH_scaled <- (Temp_max - Temp_min) * ((data_subset$water - RH_min) / (RH_max - RH_min)) + Temp_min
    
    RH_labels <- pretty(data_subset$water)
    RH_at <- (Temp_max - Temp_min) * ((RH_labels - RH_min) / (RH_max - RH_min)) + Temp_min
    
    # Adding RH points
    points(x = data_subset$datetime, y = RH_scaled, col = "coral", pch = 17)
    axis(4, at = RH_at, labels = RH_labels, col = "coral", col.ticks = "coral", col.axis = "coral")
    
    # Adding a legend
    legend("topright", legend = c("Temperature", "RH"), 
           col = c("darkgreen", "coral"), pch = c(16, 17), box.lty = 0)
  })
}

shinyApp(ui, server)


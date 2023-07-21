# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Data Visualization App"),
  sidebarLayout(
    sidebarPanel(
      # Add the fileInput widget to upload data
      fileInput("data_upload", "Upload your CSV file", accept = ".csv"),
      
      # Dropdown menu for histogram variable selection
      selectInput("hist_var", "Select a Variable for Visualization:", ""),
      
      # Slider input for controlling the number of bins
      sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 10),
      
      # Dropdown menus for scatter plot variable selection
      selectInput("scatter_x", "Select X-axis Variable for Scatter Plot (Continuous):", ""),
      selectInput("scatter_y", "Select Y-axis Variable for Scatter Plot (Continuous):", ""),
      
      # Dropdown menu for categorical variable (color) in scatter plot
      selectInput("scatter_color", "Select Categorical Variable for Color in Scatter Plot:", ""),
      
      # Dropdown menus for 3-variable scatter plot
      selectInput("scatter3_var1", "Select X-axis Variable for 3-Variable Scatter Plot (Continuous):", ""),
      selectInput("scatter3_var2", "Select Y-axis Variable for 3-Variable Scatter Plot (Continuous):", ""),
      selectInput("scatter3_color", "Select Categorical Variable for Color in 3-Variable Scatter Plot:", ""),
      
      # Dropdown menus for scatter plot with two variables
      selectInput("scatter2_x", "Select X-axis Variable for Scatter Plot with Two Variables (Continuous):", ""),
      selectInput("scatter2_y", "Select Y-axis Variable for Scatter Plot with Two Variables (Continuous):", "")
    ),
    mainPanel(
      # Output for displaying the histogram or bar chart
      plotOutput("visualization"),
      verbatimTextOutput("summary_stat"), # Output for displaying summary statistic
      tableOutput("freq_table"), # Output for displaying frequency distribution table
      
      # Output for displaying the scatter plot with two variables
      plotlyOutput("scatterplot"),
      
      # Output for displaying the scatter plot with three variables
      plotlyOutput("scatter3dplot"),
      
      # Output for displaying the scatter plot with two variables
      plotlyOutput("scatter2dplot")
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output, session) {  # Include 'session' parameter
  
  # Read the uploaded data and create reactive data frame
  data <- reactive({
    req(input$data_upload)  # Ensure data is uploaded
    read.csv(input$data_upload$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  # Update dropdown menus with column names from the uploaded data
  observe({
    req(data())  # Ensure data is available
    updateSelectInput(session, "hist_var", choices = colnames(data()))
    updateSelectInput(session, "scatter_x", choices = colnames(data()))
    updateSelectInput(session, "scatter_y", choices = colnames(data()))
    updateSelectInput(session, "scatter_color", choices = colnames(data()))
    updateSelectInput(session, "scatter3_var1", choices = colnames(data()))
    updateSelectInput(session, "scatter3_var2", choices = colnames(data()))
    updateSelectInput(session, "scatter3_color", choices = colnames(data()))
    updateSelectInput(session, "scatter2_x", choices = colnames(data()))
    updateSelectInput(session, "scatter2_y", choices = colnames(data()))
  })
  
  # Create the histogram or bar chart based on the variable type
  output$visualization <- renderPlot({
    req(data(), input$hist_var)  # Ensure data and variable selection are available
    
    selected_var <- data()[[input$hist_var]]
    
    # Check if the selected variable is categorical
    if (is.factor(selected_var) || is.character(selected_var)) {
      # Create a bar chart for categorical variables
      data() %>%
        group_by_at(input$hist_var) %>%
        summarize(count = n()) %>%
        ggplot(aes(x = !!sym(input$hist_var), y = count, fill = !!sym(input$hist_var))) +
        geom_bar(stat = "identity") +
        labs(title = paste("Bar Chart of", input$hist_var),
             x = input$hist_var, y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      # Create a histogram for numeric variables
      ggplot(data(), aes(x = !!sym(input$hist_var))) +
        geom_histogram(binwidth = diff(range(selected_var))/input$bins, fill = "steelblue", color = "white") +
        labs(title = paste("Histogram of", input$hist_var),
             x = input$hist_var, y = "Frequency")
    }
  })
  
  # Display summary statistic for continuous variables
  output$summary_stat <- renderPrint({
    req(data(), input$hist_var)
    
    selected_var <- data()[[input$hist_var]]
    
    # Check if the selected variable is continuous
    if (!is.factor(selected_var) && !is.character(selected_var)) {
      summary(selected_var)
    }
  })
  
  # Display frequency distribution table for categorical variables
  output$freq_table <- renderTable({
    req(data(), input$hist_var)
    
    selected_var <- data()[[input$hist_var]]
    
    # Check if the selected variable is categorical
    if (is.factor(selected_var) || is.character(selected_var)) {
      freq_table <- data() %>%
        group_by_at(input$hist_var) %>%
        summarize(count = n())
      
      colnames(freq_table) <- c(input$hist_var, "Frequency")
      freq_table
    }
  })
  
  # Create the scatter plot with color mapping for categorical variable
  output$scatterplot <- renderPlotly({
    req(data(), input$scatter_x, input$scatter_y, input$scatter_color)  # Ensure data and variable selection are available
    plot_ly(data(), x = data()[[input$scatter_x]], y = data()[[input$scatter_y]], color = data()[[input$scatter_color]],
            type = "scatter", mode = "markers") %>%
      layout(title = paste("Scatter Plot of", input$scatter_x, "vs.", input$scatter_y),
             xaxis = list(title = input$scatter_x),
             yaxis = list(title = input$scatter_y))
  })
  
  # Create the scatter plot with three variables and color mapping for categorical variable
  output$scatter3dplot <- renderPlotly({
    req(data(), input$scatter3_var1, input$scatter3_var2, input$scatter3_color)  # Ensure data and variable selection are available
    plot_ly(data(), x = data()[[input$scatter3_var1]], y = data()[[input$scatter3_var2]], color = data()[[input$scatter3_color]],
            type = "scatter", mode = "markers") %>%
      layout(title = paste("Scatter Plot of", input$scatter3_var1, "vs.", input$scatter3_var2, "Color by", input$scatter3_color),
             xaxis = list(title = input$scatter3_var1),
             yaxis = list(title = input$scatter3_var2))
  })
  
  # Create the scatter plot with two variables
  output$scatter2dplot <- renderPlotly({
    req(data(), input$scatter2_x, input$scatter2_y)  # Ensure data and variable selection are available
    plot_ly(data(), x = data()[[input$scatter2_x]], y = data()[[input$scatter2_y]],
            type = "scatter", mode = "markers") %>%
      layout(title = paste("Scatter Plot of", input$scatter2_x, "vs.", input$scatter2_y),
             xaxis = list(title = input$scatter2_x),
             yaxis = list(title = input$scatter2_y))
  })
}

# Run the Shiny app
shinyApp(ui, server)

options(repos = c(CRAN = "https://cran.rstudio.com/"))

library(shiny)
library(shinythemes)
library(survival)
library(riskRegression)
library(dplyr)
library(ggplot2)
library(readxl)
library(prodlim)
library(plotly)   # For interactive plots

# -----------------------
# 1. Load Final Model
# -----------------------
saved_model_path <- "final_fg_model.rds"
mean_cif_path <- "mean_cif_data.rds"

# Load the RDS files using the variables
final_model <- readRDS(saved_model_path)
mean_cif_data <- readRDS(mean_cif_path)  

# -----------------------
# UI
# -----------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Fine-Gray Model Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter Predictor Values"),
      selectInput("Insurance_Type", "Insurance Type",
                  choices = list("No Insurance" = "0", 
                                 "Private"      = "1", 
                                 "Public"       = "2"),
                  selected = "0"),
      selectInput("Node", "Node Status",
                  choices = list("N0" = "0", "N1" = "1", "N2" = "2", "N3" = "3"),
                  selected = "2"),
      selectInput("Periodontal_Grading", "Periodontal Grading",
                  choices = list("0" = "0", "I" = "1", "II" = "2", "III" = "3", "IV" = "4"),
                  selected = "3"),
      selectInput("Disease_Site_Merged_2", "Tumor Site",
                  choices = list("Others" = "0", "Oropharynx" = "1", "Oral Cavity" = "2"),
                  selected = "2"),
      numericInput("Age", "Age", value = 60, min = 0, max = 120),
      numericInput("Smoking_Pack_per_Year", "Smoking Pack-Year", value = 50, min = 0, max = 200),
      numericInput("Income_1000", "Income (in $1000)", value = 50, min = 0, max = 500),
      numericInput("Number_Teeth_after_Extraction", "Number of Teeth After Extraction", 
                   value = 20, min = 0, max = 32),
      numericInput("RT_Dose", "RT Dose (Gy)", value = 66, min = 0, max = 80),
      numericInput("D20", "D20 (Gy)", value = 55, min = 0, max = 100),
      textInput("time_points_interest", 
                "Time Points (comma-separated)", 
                value = "60"),
      
      # --- Toggle for reference curve
      checkboxInput("showReference", "Show Reference (Average CIF)?", value = FALSE),
      
      actionButton("predictBtn", "Predict"),
      br(),
      helpText("Click the button to generate the CIF curve and predictions.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 h4("CIF Curve"),
                 # Use plotlyOutput for interactive CIF
                 plotlyOutput("plotCIF"),
                 br(),
                 h4("CIF Values at Requested Time Points"),
                 tableOutput("cifValues")
        )
      )
    )
  )
)

# -----------------------
# SERVER
# -----------------------
server <- function(input, output, session) {
  
  # Create a 1-row data frame from user inputs
  newdata_reactive <- reactive({
    data.frame(
      Insurance_Type = factor(input$Insurance_Type, levels = c("0", "1", "2")),
      Node           = factor(input$Node, levels = c("0", "1", "2", "3")),
      Periodontal_Grading = factor(input$Periodontal_Grading, 
                                   levels = c("0", "1", "2", "3", "4")),
      Disease_Site_Merged_2 = factor(input$Disease_Site_Merged_2, 
                                     levels = c("0", "1", "2")),
      Age = as.numeric(input$Age),
      Smoking_Pack_per_Year = as.numeric(input$Smoking_Pack_per_Year),
      Income_1000 = as.numeric(input$Income_1000),
      Number_Teeth_after_Extraction = as.numeric(input$Number_Teeth_after_Extraction),
      RT_Dose = as.numeric(input$RT_Dose),
      D20 = as.numeric(input$D20)
    )
  })
  
  observeEvent(input$predictBtn, {
    # 1. Predict & Plot CIF
    one_indiv <- newdata_reactive()
    time_grid <- seq(0, 114, by = 1)
    indiv_cif <- predictRisk(final_model, newdata = one_indiv, times = time_grid, cause = 1)
    cif_values <- as.numeric(indiv_cif[1, ])
    
    output$plotCIF <- renderPlotly({
      # Build the individual's data frame for plotting
      df_plot <- data.frame(
        Time = time_grid,
        CIF  = round(cif_values, 3)
      )
      
      # Base ggplot with individual's CIF curve
      p <- ggplot(df_plot, aes(x = Time, y = CIF)) +
        geom_line(color = "blue") +
        geom_point(aes(text = paste0("Time: ", Time, "\nCIF: ", sprintf('%.3f', CIF))),
                   color = "blue", size = 1) +
        theme_minimal() +
        labs(title = "", x = "Time (months)", y = "CIF")
      
      # If user has checked "Show Reference", overlay the mean CIF curve
      if (input$showReference) {
        p <- p +
          geom_line(
            data = mean_cif_data,
            aes(x = Time, y = MeanCIF),
            color = "red", linetype = "dashed"
          ) +
          geom_point(
            data = mean_cif_data,
            aes(x = Time, y = MeanCIF,
                text = paste0("Time: ", Time, "\nAvg CIF: ", sprintf('%.3f', MeanCIF))),
            color = "red", size = 1
          )
      }
      
      ggplotly(p, tooltip = "text")
    })
    
    # 2. Show CIF at user-requested time points
    user_times_vec <- as.numeric(trimws(strsplit(input$time_points_interest, ",")[[1]]))
    user_times_vec <- user_times_vec[!is.na(user_times_vec)]
    if (length(user_times_vec) == 0) user_times_vec <- c(60, 114)
    indiv_cif_interest <- predictRisk(final_model, newdata = one_indiv, 
                                      times = user_times_vec, cause = 1)
    
    output$cifValues <- renderTable({
      data.frame(
        Time = user_times_vec,
        CIF  = sprintf("%.3f", as.numeric(indiv_cif_interest))
      )
    }, digits = 0, align = 'c')
    
  })
}

if (interactive()) {
  shinyApp(ui = ui, server = server)
} else {
  # In a production (non-interactive) environment, use the PORT provided by Render.
  shiny::runApp(list(ui = ui, server = server), 
                host = "0.0.0.0", 
                port = as.numeric(Sys.getenv("PORT", 3838)))
}

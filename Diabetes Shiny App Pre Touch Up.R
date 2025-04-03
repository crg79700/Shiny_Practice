# Install packages if needed
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(shinythemes)) install.packages("shinythemes")
if (!require(data.table)) install.packages("caret")
if (!require(RCurl)) install.packages("dplyr")

# Load libraries
library(shiny)
library(ggplot2)
library(shinythemes)
library(caret)
library(dplyr)

diab_dat <- read.csv('/Users/colingardner/Library/CloudStorage/OneDrive-UniversityofGeorgia/GCA Class/Healthcare Data Analysis/Healthcare-Diabetes.csv')

# Making the model to predict using logistic regression

mod <- train(as.factor(Outcome) ~ Pregnancies	+ Glucose +	BloodPressure	+ 
               SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age,
             data = diab_dat,
             method = "glm",
             family = "binomial")

# Defining the UI

ui <- fluidPage(
  titlePanel("Diabetes Risk Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("preg", "Number of Pregnancies:", value = 0, min = 0, max = 20),
      numericInput("gluc", "Glucose Level:", value = 100, min = 0, max = 300),
      numericInput("bp", "Diastolic Blood Pressure:", value = 70, min = 0, max = 200),
      numericInput("skin", "Triceps Skin Thickness:", value = 20, min = 0, max = 80),
      numericInput("ins", "Insulin Level:", value = 25, min = 0, max = 1000),
      numericInput("bmi", "BMI:", value = 25, min = 0, max = 70),
      numericInput("dpf", "Diabetes Pedigree Function:", value = 0.5, min = 0, max = 3),
      numericInput("age", "Age:", value = 30, min = 0, max = 120),
      actionButton("predict", "Calculate Risk")
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      textOutput("predictionText")
    )
  )
)

server <- function(input, output) {
  
  # A reactive value that stores the prediction value
  pred_result <- reactiveVal()
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      Pregnancies = input$preg,
      Glucose = input$gluc,
      BloodPressure = input$bp,
      SkinThickness = input$skin,
      Insulin = input$ins,
      BMI = input$bmi,
      DiabetesPedigreeFunction = input$dpf,
      Age = input$age
    )
    
    
    pred <- predict(mod, newdata = new_data, type = "prob")
    pred_result(round(pred[2] * 100, 2))
  })
  

  # Graphical Representation of the Data
  output$scatterPlot <- renderPlot({
    
    i_p <- data.frame(
      Glucose = input$gluc,
      BMI = input$bmi
    )
    
    ggplot(diab_dat, aes(x = Glucose, y = BMI, color = as.factor(Outcome))) +
           geom_point(alpha = 0.6) +
           geom_point(data = i_p, aes(x = Glucose, y = BMI),
                      color = 'red', size = 4, shape = 8) +
           scale_color_manual(values = c("blue", "green"),
                      labels = c("No Diabetes", "Diabetes"),
                      name = "Diabetes Status") +
             theme_minimal() +
             labs(title = "Glucose and BMI",
                  subtitle = "Red star shows current input")
  })
  
  #Display Prediction Text
  output$predictionText <- renderText({
    if (is.null(pred_result())) {
      "Please click 'Calculate Risk' to see prediction"
    } else {
      paste("Predicted Diabetes Risk: ", pred_result(), "%")
    }
  })
}

shinyApp(ui = ui, server = server)
  

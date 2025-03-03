# Install packages if needed
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(shinythemes)) install.packages("shinythemes")
if (!require(e1071)) install.packages("e1071")
if (!require(caret)) install.packages("caret")

# Load libraries
library(shiny)
library(randomForest)
library(ggplot2)
library(shinythemes)
library(e1071) 
library(caret)

# Read data
avgpred <- read.csv(text = getURL("https://raw.githubusercontent.com/crg79700/ShinyPractice1/refs/heads/master/cog.csv?token=GHSAT0AAAAAAC74VGNFIDYU74HEY73LK26WZ6GHDSA"))

avgpred$AVG.Status[avgpred$AVG.Status == 1] <- "AVGP" 
avgpred$AVG.Status[avgpred$AVG.Status == 0] <- "NAVG" 

avgpred$MOTS <- as.numeric(avgpred$MOTS)
avgpred$AVG.Status <- as.factor(avgpred$AVG.Status)

# Define UI
ui <- fluidPage(
  titlePanel("SVM for Action Video Game Prediction"),
  
  # Dropdown for kernel selection
  selectInput("kernel", "Select SVM Kernel:",
              choices = c("Linear" = "linear",
                          "Radial" = "radial",
                          "Polynomial" = "polynomial")),
  
  # Plot output
  plotOutput("svmPlot")
)

# Define Server
server <- function(input, output) {
  # Reactive SVM model 
  svm_model <- reactive({
    set.seed(123)
    svm(AVG.Status ~ ., 
        data = avgpred, 
        kernel = input$kernel, 
        cost = 1,  # Regularization parameter
        scale = TRUE)  # Scale features like PCA
  })
  
  # Render the plot (confusion matrix heatmap)
  #output$svmPlot <- renderPlot({
   # model <- svm_model()
    
    # Predictions
   # preds <- predict(model, avgpred)
    
    # Confusion matrix
    #conf_mat <- confusionMatrix(preds, avgpred$AVG.Status)
    #conf_df <- as.data.frame(as.table(conf_mat$table))
    
    # Heatmap with ggplot2
    #ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    #  geom_tile() +
    #  geom_text(aes(label = Freq), color = "white") +
    #  labs(title = paste("SVM Confusion Matrix (Kernel:", input$kernel, ")"),
    #       subtitle = "Predictions vs. Actual Gaming Status",
    #       x = "Actual", y = "Predicted") +
    #  scale_fill_gradient(low = "steelblue", high = "darkred") +
    #  theme_minimal()
  
    output$svmPlot <- renderPlot({
      model <- svm_model()
      plot_data <- avgpred[, c("EF", "COMATTN", "AVG.Status")]
      plot(model, plot_data, EF ~ COMATTN,
           svSymbol = "x", dataSymbol = "o",
           color.palette = terrain.colors)  
    
    
    })
}



# Run the app
shinyApp(ui = ui, server = server)
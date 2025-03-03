# Install packages if needed
if (!require(shiny)) install.packages("shiny")
if (!require(randomForest)) install.packages("randomForest")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(shinythemes)) install.packages("shinythemes")
if (!require(data.table)) install.packages("data.table")
if (!require(RCurl)) install.packages("RCurl")

# Load libraries
library(shiny)
library(randomForest)
library(ggplot2)
library(shinythemes)
library(data.table)
library(RCurl)



## NOTES

# Put a Figure output based on the RandomForest 

# NEXT STEPS
# Principle Components Analysis and Dimensionality Reduction
## Visualize data in 2 or 3 dimensions
# Support Vector Machines (SVM) <- Read up on this and PCA and DR


# Read data
avgpred <- d.dis_clr.wmc

avgpred$AVG.Status[avgpred$AVG.Status == 1] <- "AVGP" 
avgpred$AVG.Status[avgpred$AVG.Status == 0] <- "NAVG" 

avgpred$AVG.Status <- as.factor(avgpred$AVG.Status)

#User Interface is Below

ui <- fluidPage(theme = shinytheme("united"),
                
                # Dropdown to select which cognitive function is being shown
                selectInput("cog_test", "Select Cognitive Test:",
                            choices = c("Executive Function" = "EF",
                                        "Motor Speed" = "MOTS",
                                        "Simple Attention" = "SIMATTN",
                                        "Complex Attention" = "COMATTN",
                                        "Working Memory" = "WM",
                                        "Processing Speed" = "PS",
                                        "Reaction Time" = "RT")),
                
                # Page header
                headerPanel('Are you a Gamer?'),
                
                # Input values
                sidebarPanel(
                  HTML("<h4>Put in your Cognitive Test Scores Below</h4>"),
                  
                  sliderInput("EF", label = "Executive Function:", 
                              min = 89, max = 130, 
                              value = 100),
                  sliderInput("MOTS", "Motor Speed:",
                              min = 90, max = 160,
                              value = 110),
                  sliderInput("SIMATTN", "Simple Attention:",
                              min = 75, max = 130,
                              value = 90),
                  sliderInput("COMATTN", "Complex Attention:",
                              min = 80, max = 120,
                              value = 100),
                  sliderInput("WM", "Working Memory:",
                              min = 80, max = 120,
                              value = 100),
                  sliderInput("PS", "Processing Speed:",
                              min = 90, max = 160,
                              value = 120),
                  sliderInput("RT", "Reaction Time:",
                              min = 90, max = 125,
                              value = 105),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata'), # Prediction results table
                  plotOutput('apredplot')
                  
                )
)

#Server

server <- function(input, output, session) {
  
  mod <- reactive({randomForest(AVG.Status ~ ., data = avgpred, ntree = 880, mtry = 7, importance = TRUE, proximity = TRUE)
  })
  
  # Input Data
#  datasetInput <- reactive({  
    
    # gaming status and cognitive functions
#    df <- data.frame(
      
#      Name = c("EF",
#               "MOTS",
#               "SIMATTN",
#               "COMATTN",
#               "WM",
#               "PS",
#               "RT"),
#      
#      Value = as.character(c(input$EF,
#                             input$MOTS,
#                             input$SIMATTN,
#                             input$COMATTN,
#                             input$WM,
#                             input$PS,
#                             input$RT)),
#      stringsAsFactors = FALSE)
    
#    Gamer <- "Avg.Status"
#    df <- rbind(df, Gamer)
#    input <- transpose(df)
#    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
#    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    
#    Output <- data.frame(Prediction = predict(mod,test), round(predict(mod,test,type="prob"), 3))
#    print(Output)
    
#  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Status Shown Below.") 
    } else {
      return("Find out if you're as good as a Gamer!")
    }
  })

  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  #Graph Output
#  output$apredplot <- renderPlot({
    # Get the model
 #    model <- datasetInput()
#### Variable Importance Graph 
     # Uncomment to show
   # Extract variable importance
#   importance_data <- as.data.frame(importance(mod, type = 1)) # Type 1 for mean decrease in accuracy
#  importance_data$Variable <- rownames(importance_data)
#   colnames(importance_data)[1] <- "Importance"
   
   # Create a ggplot2 bar plot
#   ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance)) +
#     geom_bar(stat = "identity", fill = "steelblue") +
#     coord_flip() + # Flip for readability
#     labs(title = "Variable Importance from Random Forest",
#          x = "Variables", y = "Importance (Mean Decrease in Accuracy)") +
#     theme_minimal()
#  })
  
#### Partial Dependence Plot for Working Memory
  # Uncomment to show
# output$apredplot <- renderPlot({
#  model <- mod()
  
#  partialPlot(model, 
#              pred.data = avgpred, 
#              x.var = input$cog_test, 
#             main = paste("Partial Dependence of", input$cog_test, 
#                          "on Action Video Game Play Prediction"),
#             xlab = input$cog_test,
#             ylab = "Partial Dependence",
#             col = "steelblue", lwd = 2, rug = TRUE)
#  })

#### Multi-Dimensional Scaling Plot
#output$apredplot <- renderPlot({
#   model <- datasetInput()
#   MDSplot(mod, avgpred$AVG.Status, palette = rainbow(2), 
#               main = "MDS Plot of Random Forest Proximity")
#     })
  
# Prediction Plot for Cognitive Function
  output$apredplot <- renderPlot({
    model <- mod()
    preds <- predict(model, newdata = avgpred)
    
    cog_var <- avgpred[[input$cog_test]]
    cog_levels <- cut(cog_var, breaks = 3, labels = c("Low", "Mid", "High"))
    
    # Confusion Matrix
    conf_mat <- table(Predicted = preds, Actual = avgpred$AVG.Status, Cog_Level = cog_levels)
    conf_df <- as.data.frame(as.table(conf_mat))
    
    ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white") +
      facet_wrap(~Cog_Level, labeller = label_both) + 
      labs(title = "Confusion Matrix for", input$cog_test,
            subtitle = "Faceted by Test Score Levels") +
      scale_fill_gradient(low = "steelblue", high = "darkred") +
      theme_minimal()
  })  
  
  
  
  
  output$message <- renderText({
    if (datasetInput$Gamer == "AVGP") { 
      "You are an Action Gamer!" 
    } else{
      "You are not an Action Gamer :("
    }
  })
  
  output$txtout <- renderText({
    if (input$submitbutton > 0) {
     paste(datasetInput$Prediction) 
    }
  })
  

  
}


shinyApp(ui = ui, server = server)

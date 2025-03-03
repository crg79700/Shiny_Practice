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

avgpred$MOTS <- as.numeric(avgpred$MOTS)
avgpred$AVG.Status <- as.factor(avgpred$AVG.Status)

#User Interface 

ui <- fluidPage(
  titlePanel("PCA on Cognitive Tests for Action Video Games"),
  
  # Toggle scaling or centering
  checkboxInput("scale", "Scale variables (recommended for PCA)", value = TRUE),
  checkboxInput("center", "Center variables", value = TRUE),
  
  # Output for PCA biplot
  plotOutput("pcaPlot")
)

# Define Server
server <- function(input, output, session) {
  
  pca_data <- reactive({
    
    avgpred[, sapply(avgpred, is.numeric)] # Select only numeric columns
  })
  
  # PCA computation
  pca_result <- reactive({
    prcomp(pca_data(), scale. = input$scale, center = input$center)
  })
  
  # Render PCA biplot
  output$pcaPlot <- renderPlot({
    pca <- pca_result()
    game_factor <- avgpred$AVG.Status # For coloring points
    
    # Biplot with ggplot2
    pca_df <- as.data.frame(pca$x[, 1:2]) # First two components
    pca_df$AVG.Status <- game_factor
    
    ggplot(pca_df, aes(x = PC1, y = PC2, color = AVG.Status)) +
      geom_point(size = 3) +
      labs(title = "PCA Biplot of Cognitive Tests",
           subtitle = "Points colored by Action Video Game Play",
           x = paste("PC1 (", round(pca$sdev[1]^2 / sum(pca$sdev^2) * 100, 1), "%)"),
           y = paste("PC2 (", round(pca$sdev[2]^2 / sum(pca$sdev^2) * 100, 1), "%)")) +
      scale_color_manual(values = list("AVGP" = "darkred", "NAVG" = "steelblue")) +
      theme_minimal() +
      
      # Add variable loadings (arrows)
      geom_segment(data = as.data.frame(pca$rotation[, 1:2] * 5), # Scale for visibility
                   aes(x = 0, y = 0, xend = PC1, yend = PC2),
                   arrow = arrow(length = unit(0.2, "cm")), color = "black") +
      geom_text(data = as.data.frame(pca$rotation[, 1:2] * 5),
                aes(x = PC1, y = PC2, label = rownames(pca$rotation)),
                vjust = 1.5, color = "black")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

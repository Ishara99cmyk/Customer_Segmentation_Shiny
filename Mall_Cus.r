library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

df <- read.csv("Mall_Customers.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Mall Customers Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Segmentation", tabName = "segmentation", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "Gender Distribution",
            plotOutput("genderPlot", height = 400)
          ),
          
          box(
            title = "Age Distribution",
            plotOutput("agePlot", height = 400)
          ),
          
          box(
            title = "Annual Income Distribution",
            plotOutput("incomePlot", height = 400)
          ),
          
          box(
            title = "Spending Score Distribution",
            plotOutput("scorePlot", height = 400)
          )
        )
      ),
      
      tabItem(
        tabName = "segmentation",
        fluidRow(
  
          box(
            title = "Elbow Method for K-means",
            plotOutput("elbowPlot", height = 400)
          ),
          

          box(
            title = "K-means Clustering",
            plotOutput("kmeansPlot", height = 400)
          )
        ),

        verbatimTextOutput("segmentationExplanation")
      )
    )
  )
)


server <- function(input, output) {
  
  # Gender plot
  output$genderPlot <- renderPlot({
    ggplot(df, aes(x = Gender)) + geom_bar(fill = "skyblue") +
      labs(title = "Gender Distribution")
  })
  
  # Age plot
  output$agePlot <- renderPlot({
    ggplot(df, aes(x = Age)) + geom_histogram(fill = "lightgreen", bins = 30) +
      labs(title = "Age Distribution")
  })
  
  # Annual Income plot
  output$incomePlot <- renderPlot({
    ggplot(df, aes(x = Annual_Income)) + geom_histogram(fill = "orange", bins = 30) +
      labs(title = "Annual Income Distribution")
  })
  
  # Spending Score plot
  output$scorePlot <- renderPlot({
    ggplot(df, aes(x = Spending_Score)) + geom_histogram(fill = "pink", bins = 30) +
      labs(title = "Spending Score Distribution")
  })
  
  # Segmentation server logic
  elbow_data <- reactive({
    wss_values <- numeric(10)
    for (i in 1:10) {
      kmeans_result <- kmeans(df[, c("Annual_Income", "Spending_Score")], centers = i)
      wss_values[i] <- kmeans_result$tot.withinss
    }
    data.frame(k = 1:10, wss = wss_values)
  })
  
  output$elbowPlot <- renderPlot({
    plot(elbow_data()$k, elbow_data()$wss, type = "b", pch = 19, frame = FALSE, 
         xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares",
         main = "Elbow Method for K-means")
  })
  
  output$kmeansPlot <- renderPlot({
    k_optimal <- 6
    kmeans_result <- kmeans(df[, c("Annual_Income", "Spending_Score")], centers = k_optimal)
    
    ggplot(df, aes(x = Annual_Income, y = Spending_Score, color = factor(kmeans_result$cluster))) +
      geom_point() +
      labs(title = "K-means Clustering") +
      theme_minimal()
  })
  
  
  # Explanation of the segmentation process
  output$segmentationExplanation <- renderText({
    paste("Segmentation Explanation:",
          "1. The elbow method is used to find the optimal number of clusters (k).",
          "2. The 'Elbow Method for K-means' plot shows the total within sum of squares for different values of k.",
          "3. The optimal number of clusters (k) is determined to be: 6",
          "4. The 'K-means Clustering' plot displays the data points with colors representing different clusters based on the optimal k.",
          sep = "\n")
  })
}

# Run the Shiny app
shinyApp(ui, server)




# Load necessary libraries
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
file_path <- "imdb_top_1000.csv"  # Path to the dataset in the working directory

# Check if the file exists
if (!file.exists(file_path)) {
  stop("File not found. Please make sure 'imdb_top_1000.csv' is in the working directory.")
}

# Load the data
imdb_data <- tryCatch({
  read_csv(file_path, show_col_types = FALSE)
}, error = function(e) {
  stop("Error reading the dataset: ", e$message)
})

# Define UI for application
ui <- fluidPage(
  titlePanel("IMDb Top 1000 Movies Explorer"),
  
  tabsetPanel(
    tabPanel("IMDb Ratings",
             sidebarLayout(
               sidebarPanel(
                 selectInput("genre", "Select Genre:", 
                             choices = c("All Genres", unique(imdb_data$Genre))),
                 selectInput("director", "Select Director:", 
                             choices = unique(imdb_data$Director))
               ),
               mainPanel(
                 plotOutput("ratingPlot"),
                 tableOutput("movieTable")
               )
             )
    ),
    tabPanel("Average Revenues by Genre",
             sidebarLayout(
               sidebarPanel(
                 helpText("This tab shows the average revenues for the top 15 genres.")
               ),
               mainPanel(
                 plotOutput("revenuePlot"),
                 tableOutput("revenueTable")
               )
             )
    ),
    tabPanel("Top 15 Directors by Average Gross",
             sidebarLayout(
               sidebarPanel(
                 helpText("This tab shows the top 15 directors by average gross.")
               ),
               mainPanel(
                 plotOutput("directorPlot"),
                 tableOutput("directorTable")
               )
             )
    ),
    tabPanel("Gross Earnings by Director",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_director", "Select Director:", 
                             choices = unique(imdb_data$Director))
               ),
               mainPanel(
                 plotOutput("grossPlot"),
                 tableOutput("grossTable")
               )
             )
    ),
    tabPanel("Darren Aronofsky's Movies",
             sidebarLayout(
               sidebarPanel(
                 selectInput("metric", "Select Metric:", 
                             choices = c("IMDb Rating" = "IMDB_Rating", 
                                         "Meta Score" = "Meta_score", 
                                         "Gross" = "Gross"))
               ),
               mainPanel(
                 plotOutput("aronofskyPlot"),
                 tableOutput("aronofskyTable")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    if (input$genre == "All Genres") {
      imdb_data %>%
        filter(Director == input$director)
    } else {
      imdb_data %>%
        filter(grepl(input$genre, Genre)) %>%
        filter(Director == input$director)
    }
  })
  
  output$ratingPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = reorder(Series_Title, IMDB_Rating), y = IMDB_Rating)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = paste("IMDb Ratings for", input$genre, "movies directed by", input$director),
           x = "Movie Title", y = "IMDb Rating") +
      theme_minimal()
  })
  
  output$movieTable <- renderTable({
    filtered_data() %>%
      select(Series_Title, Released_Year, Genre, Director, IMDB_Rating, Meta_score, Gross) %>%
      arrange(desc(IMDB_Rating))
  })
  
  average_revenues <- reactive({
    imdb_data %>%
      filter(!is.na(Gross)) %>%
      group_by(Genre) %>%
      summarize(avg_revenue = mean(Gross, na.rm = TRUE)) %>%
      arrange(desc(avg_revenue)) %>%
      slice(1:15)
  })
  
  output$revenuePlot <- renderPlot({
    ggplot(average_revenues(), aes(x = reorder(Genre, avg_revenue), y = avg_revenue)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      coord_flip() +
      labs(title = "Top 15 Average Revenues by Genre",
           x = "Genre", y = "Average Revenue") +
      theme_minimal()
  })
  
  output$revenueTable <- renderTable({
    average_revenues() %>%
      arrange(desc(avg_revenue))
  })
  
  top_directors <- reactive({
    imdb_data %>%
      filter(!is.na(Gross)) %>%
      group_by(Director) %>%
      summarize(avg_gross = mean(Gross, na.rm = TRUE)) %>%
      arrange(desc(avg_gross)) %>%
      slice(1:15)
  })
  
  output$directorPlot <- renderPlot({
    ggplot(top_directors(), aes(x = reorder(Director, avg_gross), y = avg_gross)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      coord_flip() +
      labs(title = "Top 15 Directors by Average Gross",
           x = "Director", y = "Average Gross") +
      theme_minimal()
  })
  
  output$directorTable <- renderTable({
    top_directors() %>%
      arrange(desc(avg_gross))
  })
  
  director_gross <- reactive({
    imdb_data %>%
      filter(Director == input$selected_director) %>%
      select(Series_Title, Gross) %>%
      arrange(desc(Gross))
  })
  
  output$grossPlot <- renderPlot({
    ggplot(director_gross(), aes(x = reorder(Series_Title, Gross), y = Gross)) +
      geom_bar(stat = "identity", fill = "lightcoral") +
      coord_flip() +
      labs(title = paste("Gross Earnings for Movies Directed by", input$selected_director),
           x = "Movie Title", y = "Gross Earnings") +
      theme_minimal()
  })
  
  output$grossTable <- renderTable({
    director_gross()
  })
  
  aronofsky_data <- reactive({
    imdb_data %>%
      filter(Director == "Darren Aronofsky") %>%
      arrange(desc(!!sym(input$metric)))
  })
  
  output$aronofskyPlot <- renderPlot({
    ggplot(aronofsky_data(), aes(x = reorder(Series_Title, !!sym(input$metric)), y = !!sym(input$metric))) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      labs(title = paste(input$metric, "for Darren Aronofsky's Movies"),
           x = "Movie Title", y = input$metric) +
      theme_minimal()
  })
  
  output$aronofskyTable <- renderTable({
    aronofsky_data() %>%
      select(Series_Title, Released_Year, Genre, IMDB_Rating, Meta_score, Gross) %>%
      arrange(desc(!!sym(input$metric)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

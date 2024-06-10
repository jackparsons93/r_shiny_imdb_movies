# Load necessary libraries
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(wordcloud2)
library(shinyjs)
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
  useShinyjs(),
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
    tabPanel("Top 25 Grossing Movies",
             sidebarLayout(
               sidebarPanel(
                 helpText("This tab shows a bar chart and list of the top 25 grossing movies.")
               ),
               mainPanel(
                 plotOutput("topGrossingMoviesBarChart"),
                 uiOutput("topGrossingMoviesText"),  # Changed from textOutput to uiOutput
                 tags$style("#topGrossingMoviesText {font-size: 16px; font-weight: bold;}")
               )
             )
    
  ),
  tabPanel("Top 25 Rated Movies by IMDb",
           sidebarLayout(
             sidebarPanel(
               helpText("This tab shows a bar chart of the top 25 rated movies by IMDb rating.")
             ),
             mainPanel(
               plotOutput("topRatedMoviesBarChart"),
               uiOutput("topRatedMoviesText"),
               tags$style("#topRatedMoviesText {font-size: 16px; font-weight: bold;}")
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
    ),
    tabPanel("Steven Spielberg's Movies",
             sidebarLayout(
               sidebarPanel(
                 selectInput("spielberg_metric", "Select Metric:", 
                             choices = c("IMDb Rating" = "IMDB_Rating", 
                                         "Meta Score" = "Meta_score", 
                                         "Gross" = "Gross"))
               ),
               mainPanel(
                 plotOutput("spielbergPlot"),
                 tableOutput("spielbergTable")
               )
             )
    ),
    
    tabPanel("Distribution of IMDb Ratings",
             sidebarLayout(
               sidebarPanel(
                 helpText("This tab shows the distribution of IMDb ratings across the top 10 most frequent genres.")
               ),
               mainPanel(
                 plotOutput("violinPlot")
               )
             )
    ),
    tabPanel("IMDb Score vs Revenue",
             sidebarLayout(
               sidebarPanel(
                 helpText("This tab shows the relationship between IMDb scores and revenue.")
               ),
               mainPanel(
                 plotOutput("scatterPlot")
               )
             )
    ),
    tabPanel("Top Grossing Films Word Cloud",
             sidebarLayout(
               sidebarPanel(
                 helpText("This tab shows a word cloud made up of the titles of the top grossing films.")
               ),
               mainPanel(
                 wordcloud2Output("wordcloudPlot"),
                 textOutput("selectedMovie"),
                 tags$style("#selectedMovie {font-size: 24px; font-weight: bold;}")
               )
             )
    ),
    tabPanel("Directors' Average IMDb Ratings Word Cloud",
             sidebarLayout(
               sidebarPanel(
                 helpText("This tab shows a word cloud made up of directors' names, with size based on their average IMDb rating.")
               ),
               mainPanel(
                 wordcloud2Output("directorsWordcloud"),
                 textOutput("selectedDirector"),
                 tags$style("#selectedDirector {font-size: 24px; font-weight: bold;}")
               )
             )
    ),
    tabPanel("Gross vs IMDb Score Bubble Plot",
             sidebarLayout(
               sidebarPanel(
                 helpText("This tab shows a bubble plot of the relationship between gross earnings and IMDb score.")
               ),
               mainPanel(
                 plotOutput("grossVsImdbBubblePlot")
               )
             )
    )
    

  )
)
    



# Define server logic
server <- function(input, output,session) {
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
  top_25_rated_movies <- imdb_data %>%
    filter(!is.na(IMDB_Rating)) %>%
    arrange(desc(IMDB_Rating)) %>%
    slice(1:25) %>%
    select(Series_Title, IMDB_Rating)
  
  output$topRatedMoviesBarChart <- renderPlot({
    ggplot(top_25_rated_movies, aes(x = reorder(Series_Title, IMDB_Rating), y = IMDB_Rating)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Top 25 Rated Movies by IMDb",
           x = "Movie Title", y = "IMDb Rating") +
      theme_minimal()
  })
  
  output$topRatedMoviesText <- renderUI({
    HTML(paste(
      apply(top_25_rated_movies, 1, function(row) {
        paste("Title:", row["Series_Title"], "- IMDb Rating:", row["IMDB_Rating"])
      }),
      collapse = "<br>"
    ))
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
  # Top 25 grossing movies
  top_25_grossing_movies <- imdb_data %>%
    filter(!is.na(Gross)) %>%
    arrange(desc(Gross)) %>%
    slice(1:25) %>%
    select(Series_Title, Gross)
  
  output$topGrossingMoviesBarChart <- renderPlot({
    ggplot(top_25_grossing_movies, aes(x = reorder(Series_Title, Gross), y = Gross)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Top 25 Grossing Movies",
           x = "Movie Title", y = "Gross Earnings") +
      theme_minimal()
  })
  
  output$topGrossingMoviesText <- renderUI({
    HTML(paste(
      apply(top_25_grossing_movies, 1, function(row) {
        paste(row["Series_Title"], ":", format(as.numeric(row["Gross"]), big.mark = ","), sep = "")
      }),
      collapse = "<br>"
    ))
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
  spielberg_data <- reactive({
    imdb_data %>%
      filter(Director == "Steven Spielberg") %>%
      arrange(desc(!!sym(input$spielberg_metric)))
  })
  
  output$spielbergPlot <- renderPlot({
    ggplot(spielberg_data(), aes(x = reorder(Series_Title, !!sym(input$spielberg_metric)), y = !!sym(input$spielberg_metric))) +
      geom_bar(stat = "identity", fill = "blue") +
      coord_flip() +
      labs(title = paste(input$spielberg_metric, "for Steven Spielberg's Movies"),
           x = "Movie Title", y = input$spielberg_metric) +
      theme_minimal()
  })
  
  output$spielbergTable <- renderTable({
    spielberg_data() %>%
      select(Series_Title, Released_Year, Genre, IMDB_Rating, Meta_score, Gross) %>%
      arrange(desc(!!sym(input$spielberg_metric)))
  })
  

  
  output$violinPlot <- renderPlot({
    imdb_data_clean <- imdb_data %>%
      mutate(Genre = strsplit(Genre, ", ")) %>%
      unnest(Genre) %>%
      filter(!is.na(IMDB_Rating)) %>%
      count(Genre, sort = TRUE) %>%
      top_n(10, n) %>%
      inner_join(imdb_data %>%
                   mutate(Genre = strsplit(Genre, ", ")) %>%
                   unnest(Genre) %>%
                   filter(!is.na(IMDB_Rating)), by = "Genre") 
    
    ggplot(imdb_data_clean, aes(x = Genre, y = IMDB_Rating)) +
      geom_violin(trim = FALSE, fill = "skyblue", alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "white") +
      coord_flip() +
      labs(title = "Distribution of IMDb Ratings Across Top 10 Most Frequent Genres",
           x = "Genre", y = "IMDb Rating") +
      theme_minimal()
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(imdb_data, aes(x = IMDB_Rating, y = Gross)) +
      geom_point(alpha = 0.7, color = "blue") +
      geom_smooth(method = "lm", color = "red") +
      labs(title = "Relationship Between IMDb Scores and Revenue",
           x = "IMDb Rating", y = "Revenue") +
      theme_minimal()
  })
  top_grossing_films <- imdb_data %>%
    filter(!is.na(Gross)) %>%
    arrange(desc(Gross)) %>%
    slice(1:100) %>%
    select(Series_Title, Gross)
  
  output$wordcloudPlot <- renderWordcloud2({
    wordcloud2(
      data = data.frame(word = top_grossing_films$Series_Title, freq = top_grossing_films$Gross), 
      size = 0.3, 
      rotateRatio = 0.5,
      minRotation = -45,
      maxRotation = 45,
      color = 'random-light',
      backgroundColor = "white"
    ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          el.on('click', function(event) {
            var word = event.target.textContent;
            Shiny.setInputValue('wordcloudPlot_click', word);
          });
        }
      ")
  })
  
  output$selectedMovie <- renderText({
    paste("Hover on a movie title in the word cloud to see its gross earnings.")
  })
  
  observeEvent(input$wordcloudPlot_click, {
    selected_word <- input$wordcloudPlot_click
    selected_gross <- top_grossing_films %>%
      filter(Series_Title == selected_word) %>%
      pull(Gross)
    
    output$selectedMovie <- renderText({
      paste("Title:", selected_word, "- Gross Earnings: $", format(selected_gross, big.mark = ","))
    })
  })
  # Compute average IMDb rating for each director
  director_avg_rating <- imdb_data %>%
    filter(!is.na(IMDB_Rating)) %>%
    group_by(Director) %>%
    summarize(avg_rating = mean(IMDB_Rating, na.rm = TRUE)) %>%
    arrange(desc(avg_rating)) %>%
    slice(1:100)  # Top 100 directors by average rating
  
  # Scale the font sizes
  director_avg_rating <- director_avg_rating %>%
    mutate(rank = row_number(),
           scaled_rating = avg_rating * (101 - rank) / 100)  # Decrease font size with rank
  
  output$directorsWordcloud <- renderWordcloud2({
    wordcloud2(
      data = data.frame(word = director_avg_rating$Director, freq = director_avg_rating$scaled_rating), 
      size = 0.2,  # Base size
      rotateRatio = 0.5,
      minRotation = -45,
      maxRotation = 45,
      color = 'random-light',
      backgroundColor = "white"
    ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          el.on('click', function(event) {
            var word = event.target.textContent;
            Shiny.setInputValue('directorsWordcloud_click', word);
          });
        }
      ")
  })
  
  output$selectedDirector <- renderText({
    paste("Hover on a director's name in the word cloud to see their average IMDb rating.")
  })
  
  observeEvent(input$directorsWordcloud_click, {
    selected_director <- input$directorsWordcloud_click
    selected_avg_rating <- director_avg_rating %>%
      filter(Director == selected_director) %>%
      pull(avg_rating)
    
    output$selectedDirector <- renderText({
      paste("Director:", selected_director, "- Average IMDb Rating:", round(selected_avg_rating, 2))
    })
  })
  # Data for bubble plot
  bubble_plot_data <- imdb_data %>%
    filter(!is.na(Gross) & !is.na(IMDB_Rating))
  
  output$grossVsImdbBubblePlot <- renderPlot({
    ggplot(bubble_plot_data, aes(x = IMDB_Rating, y = Gross, size = Gross, label = Series_Title)) +
      geom_point(alpha = 0.7, color = "skyblue") +
      scale_size_continuous(range = c(1, 10)) +
      labs(title = "Gross vs IMDb Score Bubble Plot",
           x = "IMDb Score", y = "Gross Earnings") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

  


# Run the application 
shinyApp(ui = ui, server = server)

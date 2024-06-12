# Load necessary libraries
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(wordcloud2)
library(shinyjs)
library(fmsb)
library(plotly)
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
colnames(imdb_data)
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
                             choices = unique(imdb_data$Director)),
                 selectInput("ratingCriterion", "Select Criterion:",
                             choices = c("IMDb Rating", "Meta Score", "Gross", "Number of Votes"))
               ),
               mainPanel(
                 plotOutput("ratingPlot"),
                 tableOutput("movieTable")
               )
             )
    ),
    tabPanel("Genre Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("genreCriterion", "Select Criterion:",
                             choices = c("Average Revenue", "Average IMDb Score", "Average Meta Score", "Average Number of Votes"))
               ),
               mainPanel(
                 plotOutput("revenuePlot")
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
  tabPanel("Bubble Plot: Gross vs IMDb Score",
           sidebarLayout(
             sidebarPanel(
               helpText("This tab shows a bubble plot of the relationship between gross earnings and IMDb score, colored by genre.")
             ),
             mainPanel(
               plotlyOutput("grossVsImdbBubblePlot")
             )
           )
  ),
  tabPanel("Top Movies",
           sidebarLayout(
             sidebarPanel(
               selectInput("top25Criterion", "Select Criterion:",
                           choices = c("Gross", "IMDb Rating", "Meta Score", "Number of Votes")),
               sliderInput("numTopMovies", "Number of Top Movies:", 
                           min = 1, max = 50, value = 25)
             ),
             mainPanel(
               plotOutput("top25MoviesBarChart"),
               uiOutput("top25MoviesText")
             )
           )
  ),
  tabPanel("Top Directors",
           sidebarLayout(
             sidebarPanel(
               selectInput("topDirectorsCriterion", "Select Criterion:",
                           choices = c("Average Gross", "Average IMDb Score", "Average Meta Score", "Average Votes")),
               sliderInput("numTopDirectors", "Number of Top Directors:", 
                           min = 1, max = 50, value = 25)
             ),
             mainPanel(
               plotOutput("topDirectorsBarChart"),
               uiOutput("topDirectorsText")
             )
           )
  ),
  
  tabPanel("Bubble Plot: Year vs. Rating",
           sidebarLayout(
             sidebarPanel(
               helpText("This tab shows a bubble plot with Released Year on the X-axis, IMDb Rating on the Y-axis, bubble size representing the number of votes, and bubble color representing the genre.")
             ),
             mainPanel(
               plotOutput("yearRatingBubblePlot")
             )
           )
  ),
  tabPanel("Radar Plot: Top 5 Directors",
           sidebarLayout(
             sidebarPanel(
               helpText("This tab shows a radar plot of the top 5 directors based on their average IMDb rating, Meta score, and Gross earnings.")
             ),
             mainPanel(
               plotOutput("radarPlot", width = "800px", height = "800px")
             )
           )
  ),
  tabPanel("Star Wars Movies",
           sidebarLayout(
             sidebarPanel(
               selectInput("starWarsMetric", "Select Metric:", 
                           choices = c("IMDb Rating", "Meta Score", "Gross Earnings")),
               helpText("This tab shows the average IMDb ratings, metascores, and gross earnings of all Star Wars movies.")
             ),
             mainPanel(
               plotOutput("starWarsPlot")
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
  imdb_data <- imdb_data %>%
    mutate(Series_Title = ifelse(Series_Title == "Star Wars: Episode VII - The Force Awakens", 
                                 "Star Wars: Episode VII", 
                                 Series_Title))
  
  director_movies <- reactive({
    criterion <- switch(input$ratingCriterion,
                        "IMDb Rating" = "IMDB_Rating",
                        "Meta Score" = "Meta_score",
                        "Gross" = "Gross",
                        "Number of Votes" = "No_of_Votes")
    
    data <- imdb_data %>%
      filter(Director == input$director)
    
    if (input$genre != "All Genres") {
      data <- data %>%
        filter(str_detect(Genre, input$genre))
    }
    
    data %>%
      filter(!is.na(.data[[criterion]])) %>%
      select(Series_Title, !!sym(criterion)) %>%
      rename(Value = !!sym(criterion))
  })
  
  output$ratingPlot <- renderPlot({
    ggplot(director_movies(), aes(x = reorder(Series_Title, Value), y = Value)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      coord_flip() +
      labs(title = paste("Movies by", input$director, "based on", input$ratingCriterion),
           x = "Movie Title", y = input$ratingCriterion) +
      theme_minimal()
  })
  
  output$movieTable <- renderTable({
    director_movies()
  })
  
  # Reactive expression to get the average values by genre based on selected criterion
  average_values_by_genre <- reactive({
    criterion <- switch(input$genreCriterion,
                        "Average Revenue" = "Gross",
                        "Average IMDb Score" = "IMDB_Rating",
                        "Average Meta Score" = "Meta_score",
                        "Average Number of Votes" = "No_of_Votes")
    
    imdb_data %>%
      filter(!is.na(.data[[criterion]])) %>%
      mutate(Genre = strsplit(Genre, ", ")) %>%
      unnest(Genre) %>%
      group_by(Genre) %>%
      summarize(avg_value = mean(.data[[criterion]], na.rm = TRUE)) %>%
      arrange(desc(avg_value)) %>%
      slice(1:15) %>%
      select(Genre, avg_value)
  })
  
  output$revenuePlot <- renderPlot({
    ggplot(average_values_by_genre(), aes(x = reorder(Genre, avg_value), y = avg_value)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      coord_flip() +
      labs(title = paste("Top 15 Genres by", input$genreCriterion),
           x = "Genre", y = input$genreCriterion) +
      theme_minimal()
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
      backgroundColor = "black"
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
      color = 'random-dark',
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
    paste("Directors Name Size Average IMDB Rating")
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
  #data for bubble plot
  bubble_plot_data <- imdb_data %>%
    filter(!is.na(Gross) & !is.na(IMDB_Rating) & !is.na(Genre)) %>%
    mutate(Gross = as.numeric(gsub("[^0-9.]", "", Gross))) %>%
    mutate(Genre = strsplit(Genre, ", ")) %>%
    unnest(Genre)
  
  output$grossVsImdbBubblePlot <- renderPlotly({
    p <- ggplot(bubble_plot_data, aes(x = IMDB_Rating, y = Gross, size = Gross, color = Genre, text = paste("Title:", Series_Title, "<br>Genre:", Genre))) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(1, 10)) +
      labs(title = "Gross vs IMDb Score Bubble Plot",
           x = "IMDb Score", y = "Gross Earnings") +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(color = guide_legend(title = "Genre"))
    
    ggplotly(p, tooltip = "text")
  })

  # Reactive expression to get the top movies based on selected criterion
  top_25_movies <- reactive({
    criterion <- switch(input$top25Criterion,
                        "Gross" = "Gross",
                        "IMDb Rating" = "IMDB_Rating",
                        "Meta Score" = "Meta_score",
                        "Number of Votes" = "No_of_Votes")
    
    imdb_data %>%
      filter(!is.na(.data[[criterion]])) %>%
      arrange(desc(.data[[criterion]])) %>%
      slice(1:input$numTopMovies) %>%
      select(Series_Title, !!sym(criterion)) %>%
      rename(Value = !!sym(criterion))
  })
  
  output$top25MoviesBarChart <- renderPlot({
    ggplot(top_25_movies(), aes(x = reorder(Series_Title, Value), y = Value)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = paste("Top", input$numTopMovies, "Movies by", input$top25Criterion),
           x = "Movie Title", y = input$top25Criterion) +
      theme_minimal()
  })
  
  output$top25MoviesText <- renderUI({
    HTML(paste(
      apply(top_25_movies(), 1, function(row) {
        paste("Title:", row["Series_Title"], "-", input$top25Criterion, ":", row["Value"])
      }),
      collapse = "<br>"
    ))
  })
  

# Reactive expression to get the top directors based on selected criterion
top_directors <- reactive({
  criterion <- switch(input$topDirectorsCriterion,
                      "Average Gross" = "Gross",
                      "Average IMDb Score" = "IMDB_Rating",
                      "Average Meta Score" = "Meta_score",
                      "Average Votes" = "No_of_Votes")
  
  imdb_data %>%
    mutate(across(all_of(criterion), ~as.numeric(gsub("[^0-9.]", "", .)))) %>%
    filter(!is.na(.data[[criterion]])) %>%
    group_by(Director) %>%
    summarize(Average_Value = mean(.data[[criterion]], na.rm = TRUE)) %>%
    arrange(desc(Average_Value)) %>%
    slice(1:input$numTopDirectors) %>%
    select(Director, Average_Value)
})

output$topDirectorsBarChart <- renderPlot({
  ggplot(top_directors(), aes(x = reorder(Director, Average_Value), y = Average_Value)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = paste("Top Directors by", input$topDirectorsCriterion),
         x = "Director", y = input$topDirectorsCriterion) +
    theme_minimal()
})

output$topDirectorsText <- renderUI({
  HTML(paste(
    apply(top_directors(), 1, function(row) {
      paste("Director:", row["Director"], "-", input$topDirectorsCriterion, ":", round(as.numeric(row["Average_Value"]), 2))
    }),
    collapse = "<br>"
  ))
})


output$topDirectorsText <- renderUI({
  HTML(paste(
    apply(top_directors(), 1, function(row) {
      paste("Director:", row["Director"], "-", input$topDirectorsCriterion, ":", round(as.numeric(row["Average_Value"]), 2))
    }),
    collapse = "<br>"
  ))
})

# Data for bubble plot
bubble_plot_data <- imdb_data %>%
  filter(!is.na(Released_Year) & !is.na(IMDB_Rating) & !is.na(No_of_Votes) & !is.na(Genre)) %>%
  mutate(Genre = strsplit(Genre, ", ")) %>%
  unnest(Genre)

output$yearRatingBubblePlot <- renderPlot({
  ggplot(bubble_plot_data, aes(x = as.numeric(Released_Year), y = IMDB_Rating, size = No_of_Votes, color = Genre, label = Series_Title)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(1, 15)) +
    labs(title = "Bubble Plot: Year vs. Rating",
         x = "Released Year", y = "IMDb Rating") +
    theme_minimal() +
    theme(legend.position = "right")
})
# Radar plot for top 5 directors with normalization
top_5_directors_data <- reactive({
  imdb_data %>%
    filter(!is.na(Gross) & !is.na(IMDB_Rating) & !is.na(Meta_score)) %>%
    mutate(Gross = as.numeric(gsub("[^0-9.]", "", Gross))) %>%
    group_by(Director) %>%
    summarize(
      avg_gross = mean(Gross, na.rm = TRUE),
      avg_imdb = mean(IMDB_Rating, na.rm = TRUE),
      avg_meta = mean(Meta_score, na.rm = TRUE)
    ) %>%
    arrange(desc(avg_imdb)) %>%
    slice(1:5) %>%
    mutate(
      avg_gross = (avg_gross - min(avg_gross)) / (max(avg_gross) - min(avg_gross)),
      avg_imdb = (avg_imdb - min(avg_imdb)) / (max(avg_imdb) - min(avg_imdb)),
      avg_meta = (avg_meta - min(avg_meta)) / (max(avg_meta) - min(avg_meta))
    )
})

output$radarPlot <- renderPlot({
  library(fmsb)
  
  data <- top_5_directors_data()
  directors <- data$Director  # Save directors' names before transformation
  rownames(data) <- directors
  data <- data[,-1]
  data <- rbind(rep(1, ncol(data)), rep(0, ncol(data)), data)
  
  colors <- rainbow(nrow(data) - 2)
  radarchart(data, axistype = 1, 
             pcol = colors, pfcol = scales::alpha(colors, 0.5), plwd = 2,
             cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 1, 0.2), cglwd = 0.8,
             vlcex = 0.8)
  legend(x = "topright", legend = directors, col = colors, lty = 1, lwd = 2)
})
# Data for Star Wars movies
  star_wars_data <- reactive({
    imdb_data %>%
      filter(grepl("Star Wars", Series_Title, ignore.case = TRUE)) %>%
      mutate(Gross = as.numeric(gsub("[^0-9.]", "", Gross)),
             Meta_score = as.numeric(Meta_score))
  })
  
  output$starWarsPlot <- renderPlot({
    data <- star_wars_data()
    metric <- switch(input$starWarsMetric,
                     "IMDb Rating" = data$IMDB_Rating,
                     "Meta Score" = data$Meta_score,
                     "Gross Earnings" = data$Gross)
    
    ggplot(data, aes(x = reorder(Series_Title, metric), y = metric, fill = Series_Title)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste(input$starWarsMetric, "of Star Wars Movies"),
           x = "Movie Title", y = input$starWarsMetric) +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

  


# Run the application 
shinyApp(ui = ui, server = server)

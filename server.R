## server.R

# load functions
source('functions/helpers.R')

# Renders radio buttons under each image so that user can select ratings from 1 to 5
# NR means not rated - This is default selection.
# https://shiny.rstudio.com/reference/shiny/1.6.0/radioButtons.html
ratingInput = function(movieId)
{
  radioButtons(inputId = movieId,
               label = NULL,
               choices = c("NR" = 0, 1:5),
               selected = NULL,
               # choices = c(1:5),
               # selected = character(0),
               inline = TRUE,
               width = NULL)
               # choiceNames = rep("",6),
               # choiceValues = c("NR", 1:5))
}

# Read in movie data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)

colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# Extracting release year of movie from title.
year = regmatches(movies$Title, regexpr("\\((19|20)\\d{2}\\)", movies$Title))
year = strtoi(substr(year, 2, 5)) # Remove brackets
movies = cbind(movies, year)

# Reading ratings data
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# Adding age of rating in days.
# Age of newest rating is 1.
k = difftime(time1 = as_datetime(ratings$Timestamp), time2 = as_datetime(max(ratings$Timestamp)), units = "days")
ratings$age = as.numeric(round(abs(k))) + 1 # Adding 1 so that newest rating has non-zero age.


# Training model directly on ShinyApp will result into OutofMemory exception.
# This problem is solved by initializing the model from a file on disk.
# Setting 'onweb' to FALSE will train the model and save the model on disk with file name r_ubcf.rds.
# When file is available, model can be initialized by loading the file.
# This can be done by setting 'onweb' to TRUE.
onweb = FALSE
r_ubcf = NULL
modelFileName = "r_ubcf.rds"

if (onweb == TRUE){
  r_ubcf = readRDS(modelFileName)
  # unlink(modelFileName)
} else {
  r_ubcf = Recommender(createRatingMatrix(ratings), "SVD", parameter = list(k=70, normalize = "Z-score"))
  saveRDS(r_ubcf, file = modelFileName)
}

maxResults = 10
imgsPerRow = 5

shinyServer(function(input, output, session) {
  
  ############################################
  # 1. Recommendations based on ratings.
  ############################################
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    # Randomizing movie display.
    display_movie = movies[movies$MovieID %in% sample(movies$MovieID, size = num_rows * num_movies), ]
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = display_movie$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(display_movie$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 100%; color: #000000;", ratingInput(paste0("select_", display_movie$MovieID[(i - 1) * num_movies + j])))
                 ))
      })))
    })
  })
  
  # Calculate recommendations when the button is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      if (nrow(user_ratings) == 0)
      {
        pred = topNMostPopular(movies, createRatingMatrix(ratings), n = maxResults)
        movieIndex = sapply(names(pred), function(x) which(movies$MovieID %in% x))
      }
      else
      {
        print("some ratings")
        newdata = createUserRating_RatingMatrix(ratings, user_ratings)
        
        pred = predict(r_ubcf, newdata, type="ratings")
        pred_matrix = as(pred, "matrix")
        top10 = pred_matrix[, pred_matrix %in% tail(sort(pred_matrix),maxResults)][1:maxResults]
        
        # Remove "m" from movie. It will give movie ID.
        movieIds = strtoi(sub('.', '', names(top10)))
        movieIndex = sapply(movieIds, function(x) which(movies$MovieID %in% x))
        # recom_results = data.table(Rank = 1:maxResults, MovieIndex = movieIndex)
      }
      
      recom_results = data.table(Rank = 1:maxResults, MovieIndex = movieIndex)
    }) # still busy
    
  }) # clicked on button
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- maxResults / imgsPerRow
    num_movies <- imgsPerRow
    recom_result = df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieIndex[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieIndex[(i - 1) * num_movies + j]])
            )
        )
      }))) # columns
    }) # rows
  }) # renderUI function
  
  ############################################
  # 2. Recommendations based on favorite genre.
  ############################################
  
  # show dropdown with list of genres
  output$genre <- renderUI({
    selectInput(
      inputId = "genre_list",
      label = "",
      choices = colnames(getGenreMatrix(movies)),
      selected = NULL,
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  
  # Calculate most popular movies when the button is clicked
  df_genre <- eventReactive(input$btngenre, {
    withBusyIndicatorServer("btngenre", { # showing the busy indicator
      # hide the genre container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      pred = topNByGenre(movies, ratings, input$genre_list, n = maxResults)
      movieIndex = sapply(names(pred), function(x) which(movies$MovieID %in% x))
      genreresults <- data.table(Rank = 1:maxResults, MovieIndex = movieIndex)
      
    }) # still busy
    
  }) # clicked on button
  
  # display most popular by genre.
  output$genreresults <- renderUI({
    num_rows <- maxResults / imgsPerRow
    num_movies <- imgsPerRow
    recom_result = df_genre()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieIndex[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieIndex[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  ############################################
  # End
  ############################################
  
}) # server function
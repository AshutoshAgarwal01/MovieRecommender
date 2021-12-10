# All the code in this file needs to be copied to your Shiny app, and you need
# to call `withBusyIndicatorUI()` and `withBusyIndicatorServer()` in your app.
# You can also include the `appCSS` in your UI, as the example app shows.

# =============================================

# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                     time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

getGenreMatrix = function(movies)
{
  genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
  tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                                type.convert=TRUE),
                      stringsAsFactors=FALSE)
  # Get unique genres
  genre_list = unique(stack(tmp)$values)
  genre_list = sort(genre_list[which(genre_list != "NA")])
  
  m = length(genre_list)
  genre_matrix = matrix(0, nrow(movies), length(genre_list))
  for(i in 1:nrow(tmp)){
    genre_matrix[i,genre_list %in% tmp[i,]]=1
  }
  colnames(genre_matrix) = genre_list
  remove("tmp", "genres")
  genre_matrix
}

# Create 'realRatingMatrix' matrix for given ratings.
createRatingMatrix = function(ratings){
  i = paste0('u', ratings$UserID)
  j = paste0('m', ratings$MovieID)
  x = ratings$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  Rmat
}

# This rating matrix contains two users
# 9 - real user
# 8 fake user - we added fake user so that we can create a rating matrix containing same number of columns as full ratingMatrix
# We will not consider this user's predictions.
createUserRating_RatingMatrix = function(ratings, user_ratings)
{
  user_ratings = cbind(UserID = rep(9, nrow(user_ratings)), user_ratings)
  
  unique_movies = unique(ratings$MovieID)
  newdata = data.frame(UserID = rep(8, length(unique_movies)), MovieID = unique_movies, Rating = rep(1, length(unique_movies)))
  newdata = rbind(newdata, user_ratings)
  
  as(newdata, "realRatingMatrix")[2, ]
}

# Get all movie ids for given genre.
getMovieIdByGenre = function(movies, genre){
  m = movies[which(grepl(genre, movies$Genres)), ]
  m$MovieID
}

# Create rating matrix for given genre.
createRatingMatrixByGenre = function(movies, ratings, genre){
  mIds = getMovieIdByGenre(movies, genre)
  filteredRatings = ratings[ratings$MovieID %in% mIds, ]
  
  createRatingMatrix(filteredRatings)
}

# Return top N most popular movies from given rating matrix.
topNMostPopular = function(movies, ratingMatrix, n = 20)
{
  ratingMatrix_norm = normalize(ratingMatrix, method="Z-score")
  
  # Count of ratings for movies.
  # rcount = apply(as(ratingMatrix, "matrix"), 2, function(x) sum(!is.na(x)))
  rcount = colCounts(ratingMatrix)
  
  # Assign more weight to movies that had high count of ratings.
  # This will be an issue for negative ratings because smaller ratings multiplied with smaller weights will end in larger product.
  rweight = scale(rcount, center = FALSE)
  
  top_n = sort(colMeans(ratingMatrix_norm) * as.numeric(rweight), decreasing = TRUE)[1:n]
  
  # movie Ids start with "m" in ratings matrix
  # removing "m"
  names(top_n) = strtoi(sub('.', '', names(top_n)))
  top_n
}

# Return top N most popular movies for a genre.
topNByGenre = function(movies, ratings, genre, n = 20)
{
  rmat_genre = createRatingMatrixByGenre(movies, ratings, genre)
  topNMostPopular(movies, rmat_genre, n)
}

# Get user ratings from UI.
get_user_ratings = function(value_list) {
  print(names(value_list))
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

appCSS <- "
.btn-loading-container {
  margin-left: 10px;
  font-size: 1.2em;
}
.btn-done-indicator {
  color: green;
}
.btn-err {
  margin-top: 10px;
  color: red;
}
"

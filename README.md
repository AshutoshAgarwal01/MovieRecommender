# MovieRecommender

This code is for a Shiny app which recommends movies to users. This app provides two types of recommendations:

1. Recommendations based on user's favorite genre.
2. Recommendations based on SVD collaborative filtering - App recommends moviews based on moview ratings given by user.

The app is hosted here https://ashutosh1.shinyapps.io/MovieRecommender1/

Files:
1. Server.R: This file contains server side code of the app.
This file can create a brand new SVD model or it can load a pre-tranined model from a file. If you do not wish to load from the file then change value of variable 'onweb' from TRUE to FALSE.

2. ui.R: This file contains UI code.
3. functions\helpers.R: This file contains helper functions that are called from Server.R

How to run app locally from code:
Open Server.R in RStudio. Click RunApp button located on top-right section.
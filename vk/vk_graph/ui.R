library(shiny)
library(r2d3)
library(vkR)
library(dplyr)
library(plyr)

a_link <- "window.open('https://oauth.vk.com/authorize?client_id=3116505&scope=1319942&redirect_uri=https://oauth.vk.com/blank.html&display=page&response_type=token&revoke=1', '_blank')"


ui <- fluidPage(
                
  titlePanel("The Big Graph Theory"),

  
  column(6,
         "Authentification",
         
    textInput(inputId = 'token',
                 label ="Your token:"),
    
    actionButton(inputId="Auth_button", 
                 label="Auth",
                 onclick = a_link)
  ),
  
  column(6,
         "Graph building",
    numericInput(inputId = "root_id",
              label = "Target user id:",
              value=347654),
    

    actionButton(inputId="build", 
                 label="Build graph")
  ),
  

  d3Output("d3", height = "900px")
   
)


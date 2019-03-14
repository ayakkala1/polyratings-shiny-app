#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(tidytext)
library(lubridate)

polyrating <- read_csv(
  "https://raw.githubusercontent.com/ayakkala1/stat_final/master/vignettes/polyrating.csv"
) %>% 
  mutate(date = parse_date_time(date,"%m%y")) %>%
  drop_na()

subjects <- polyrating %>%
            distinct(subject) %>%
            pull() %>%
            unlist()

token_words <- polyrating %>%
  unnest_tokens(word, review) %>%
  distinct(word) %>%
  pull() %>%
  unlist()

shinyUI(fluidPage(
  
  navbarPage("PolyRating",
             tabPanel("Term Frequencies",
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput(
                      'subject', label = "Subject: ", choices = c("ALL",subjects),
                      options = list(maxItems  = 100), selected = "ALL"
                    ),
                    hr(),
                    wellPanel(
                    checkboxGroupInput(inputId = "selected_type",
                                       label = "Take out Stopwords?",
                                       choices = c("Yes"),
                                       selected = "Yes"),
                    textInput(inputId = "add_stop", 
                              label = "Add to the Stop Word lexicon: ", 
                              value = "", 
                              width = NULL, 
                              placeholder = NULL),
                    actionButton("add","Add"),
                    br(),
                    br(),
                    textInput(inputId = "delete", 
                              label = "Remove a word from Stop Word lexicon: ", 
                              value = "", 
                              width = NULL, 
                              placeholder = NULL),
                    actionButton("delete_butt","Delete"),
                    br(), br(),
                    actionButton("default","Default")),
                    wellPanel(checkboxGroupInput(inputId = "use_tf",
                                                 label = "Use TF-IDF metric?",
                                                 choices = c("Yes"),
                                                 selected = NULL))
                  ,
                  br(), br(),
                  h5("Built with",
                     img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                     "using", img(src = "https://raw.githubusercontent.com/juliasilge/tidytext/master/tools/tidytext.png",
                                  height = "30px"),
                     "on",
                     img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", 
                         height = "30px"),
                     "."),
                  width = 4),
                   
          
                  mainPanel(
                     plotOutput("distPlot")
                           )
                          )
                        ),
             tabPanel("Word Over Time",
                      h4("Examine how word use changes over time in PolyRating reviews!"),
                      sidebarLayout(
                        sidebarPanel(
                          wellPanel(
                            selectizeInput(
                              'timesubject', label = "Subject: ", choices = c("ALL",subjects),
                              options = list(maxItems  = 100), selected = "ALL"
                            ),
                            selectizeInput(inputId = "timeword", 
                                      label = "Choose words: ", 
                                      choices = c(token_words), 
                                      options = list(maxItems  = 100), 
                                      selected = "her"),
                            actionButton("examine","Examine")),
                          br(), br(),
                          h5("Built with",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                             "using", img(src = "https://raw.githubusercontent.com/juliasilge/tidytext/master/tools/tidytext.png",
                                          height = "30px"),
                             "on",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", 
                                 height = "30px"),
                             "."),
                          width = 4),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("timePlot")
                        )
                      )
             )
                      )
                    )
)

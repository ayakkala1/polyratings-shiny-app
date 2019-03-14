#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

data(stop_words)

review_words <- tokens %>%
  count(subject, word, sort = TRUE) %>%
  ungroup()

total_words <- review_words %>%
  group_by(subject) %>%
  summarize(total = sum(n))

review_words <- left_join(review_words, total_words)


shinyServer(function(input, output, session) {
  add_to_stop_r <- reactiveValues()
  
  delete_to_stop_r <- reactiveValues()
  
  observe({
    if(input$add > 0){
      add_to_stop_r$dList <- c(isolate(add_to_stop_r$dList), isolate(input$add_stop))
    }
  })
  
  observe({
    if(input$delete_butt > 0){
      delete_to_stop_r$dList <- c(isolate(delete_to_stop_r$dList), isolate(input$delete))
    }
  })
  
  observe({
    if(input$default > 0){
      add_to_stop_r$dList <- reactiveValues()
      delete_to_stop_r$dList <- reactiveValues()
    }
  })

  stop_words_new <- reactive({stop_words %>%
                          filter(!word %in% delete_to_stop_r$dList)})
  
  
  tokens <- reactive({
    polyrating %>%
      unnest_tokens(word, review)
  })
  
  subject_selected <- reactive({
    req(input$subject)
    if ("ALL" %in% input$subject){tokens()
      }else{
    filter(tokens(), subject == input$subject)}
  })
  
  
  output$distPlot <- renderPlot({
    validate(
      need(input$subject != "", "Please select a subject.")
    )
    subject_words <- paste(input$subject,collapse = ", ")

    new_filter <- replace(input$subject, input$subject == "ALL", "[\\s\\S]+")

    if (length(input$use_tf) != 0){
      if (length(input$selected_type) == 0){
        stop_filter = FALSE
      } else{
        stop_filter = TRUE
      }
    if (stop_filter){
      review_words %>%
        bind_tf_idf(word, subject, n) %>%
        filter(str_detect(subject,new_filter)) %>%
        anti_join(stop_words_new()) %>%
        filter(!word %in% add_to_stop_r$dList) %>%
        top_n(10, tf_idf) %>%
        mutate(word = fct_reorder(word,tf_idf)) %>%
        ggplot(aes(word, tf_idf, fill = word )) + geom_col() +
        xlab(NULL) + coord_flip() + guides(fill = FALSE) + ylab("TF-IDF") +
        ggtitle(paste(c("Most important words in PolyRating Reviews for",subject_words),collapse = " ")) + 
        guides(fill = FALSE)
    }else{
      review_words %>%
        bind_tf_idf(word, subject, n) %>%
        filter(str_detect(subject,new_filter)) %>%
        top_n(10, tf_idf) %>%
        mutate(word = fct_reorder(word,tf_idf)) %>%
        ggplot(aes(word, tf_idf, fill = word )) + geom_col() +
        xlab(NULL) + coord_flip() + guides(fill = FALSE) + ylab("TF-IDF") +
        ggtitle(paste(c("Most important words in PolyRating Reviews for",subject_words),collapse = " ")) + 
        guides(fill = FALSE)
    }
    }else{
    if (length(input$selected_type) == 0){
      stop_filter = FALSE
    } else{
      stop_filter = TRUE
    }
    if (stop_filter){
      subject_selected() %>%
        anti_join(stop_words_new()) %>%
        filter(!word %in% add_to_stop_r$dList) %>%
        count(word) %>%
        top_n(10, n) %>%
        mutate(word = fct_reorder(word,n)) %>%
        ggplot(aes(word, n, fill = word )) + geom_col() +
        xlab(NULL) + coord_flip() + guides(fill = FALSE) + ylab("Term Frequency") +
        ggtitle(paste(c("Most important words in PolyRating Reviews for",subject_words),collapse = " ")) + 
                guides(fill = FALSE)
    }else{
      subject_selected() %>%
        count(word) %>%
        top_n(10, n) %>%
        mutate(word = fct_reorder(word,n)) %>%
        ggplot(aes(word, n, fill = word )) + geom_col() +
        xlab(NULL) + coord_flip() + guides(fill = FALSE) + ylab("Term Frequency") +
        ggtitle(paste(c("Most important words in PolyRating Reviews for",subject_words),collapse = " ")) + 
        guides(fill = FALSE)
    }
    }
    }
  )
  
  #------------------------------ NEW TAB -------------------------------
  
  year_counts <- reactive({
    req(input$timesubject)
    if ("ALL" %in% input$timesubject){tokens() %>%
        count(date,word) %>%
        complete(date, word, fill = list(n = 0))
    }else{
      tokens() %>%
        filter(subject %in% input$timesubject) %>%
        count(date,word) %>%
        complete(date, word, fill = list(n = 0))}
  })
  
  year_totals <- reactive({year_counts() %>%
                            group_by(date) %>%
                            summarize(year_total = sum(n))})
  
  year_vals <- reactive({year_counts() %>%
      left_join(year_totals(), by = "date")})
  
  timeplot <- eventReactive(input$examine,
    {
    year_vals() %>%
        filter(word %in% input$timeword) %>%
        ggplot(aes(date, n / year_total,fill = word, color = word)) +
        geom_point() +
        geom_smooth() +
        scale_y_continuous(labels = scales::percent_format()) +
        ylab("% frequency of word in review") +
        xlab(element_blank())
  })
  
  output$timePlot <- renderPlot({timeplot()})
  
  
  }
)

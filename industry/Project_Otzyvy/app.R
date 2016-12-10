## app.R ##
library(shinydashboard)
library(shiny)

# ------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Bank Recall Analysis"),
  dashboardSidebar(
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/IrinaGoloshchapova/DevDataProd")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title = "Your recall",
        status = "success",
        solidHeader = TRUE,
        "Please, make your  recall and push a button", br(),
        textInput("input", "Your recall: ", "...")
      )
    ), 
    fluidRow(
      valueBoxOutput("approvalBox")
    ),
    fluidRow(
      box(width = 5, actionButton("goButton", "Get rating for your recall"))
    )
  )
)

# ------------------------------------------------------------------------------

server <- function(input, output) {
  
  library(text2vec)
  library(tidyr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(data.table)
  require(tm)
  library(caret)
  
  # load('./app/shiny/model_xgboost_tfidf.rda')
  # load('./app/shiny/App_Data.RData')
  load('~/Programming/Python/Learning/MIPT/Homeworks/Project_Otzyvy/model_xgboost_tfidf.rda')
  load('~/Programming/Python/Learning/MIPT/Homeworks/Project_Otzyvy/App_Data.RData')
  
  # link <- paste0(getwd(), '/GitHub/MyApplication/dist/mystem-3.0-win7-64bit/mystem -cl -e cp1251')
  # link <- paste0('C:/Users/User/Documents/Programming/Python/Learning/MIPT/Homeworks/Project_Otzyvy/mystem-3.0-win7-64bit/mystem -cl -e cp1251')
  
  stem_text <-  function(x) {
    res <- system('C:/Users/User/Documents/Programming/Python/Learning/MIPT/Homeworks/Project_Otzyvy/mystem-3.0-win7-64bit/mystem -cl -e cp1251', intern = TRUE, input = x)
    res <- paste(res, sep = '', collapse = '')
    res <- gsub("[{}]", "", res)
    res <- gsub("(\\|[^ ]+)", "", res)
    res <- gsub("\\?", "", res)
    res <- gsub("\\s+", " ", res)
    res
  }
  
  prediction <- eventReactive(input$goButton, {
      text <- input$input
      
      text_final <- stem_text(clean_text(text))
      test <- data.frame(id = 1, text = text_final)
      
      setDT(test)
      setkey(test, id)
      
      test$text <- as.character(test$text)
    
      # functions
      tok_fun <- word_tokenizer
      prep_fun <- identity
    
      # input preparation
      it_test = test$text %>% 
        prep_fun %>% 
        tok_fun %>% 
        itoken(ids = test$id, 
               # turn off progressbar because it won't look nice in rmd
               progressbar = TRUE)
      
      dtm_test <- create_dtm(it_test, bigram_vectorizer)
      dtm_test_tfidf <- create_dtm(it_test, bigram_vectorizer) %>% 
        transform(tfidf)
      bestRound <- 185
      pred_tfidf <- xgboost::predict(model_xgboost_tfidf, dtm_test_tfidf, ntreelimit = bestRound) + 1
  })
  
icon_label <- eventReactive(input$goButton, { ifelse(as.numeric(prediction()) >= 4, 'thumbs-up', 'thumbs-down') })
colour_label <- eventReactive(input$goButton, { ifelse(as.numeric(prediction()) >= 4, 'green', 'red') })
  
  # ---------output----------------------------------------------------
  output$approvalBox <- renderInfoBox({
    infoBox(
        "Rating", paste0(prediction(), " out of 5"), icon = icon(icon_label(), lib = "glyphicon"),
        color = colour_label()
      )
  })
}

shinyApp(ui, server)
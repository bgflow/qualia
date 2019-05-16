library(shiny)
library(shinythemes)
library(tm)
library(tidyverse)
library(udpipe)
library(tidytext)
library(sentimentr)
library(DT)
library(wordcloud)


ui <- fluidPage(theme = "superhero",
                sidebarLayout(
                  sidebarPanel(
                    img(src = "perception.jpg", width = "50%"),
                    titlePanel("Qualia"),
                    textInput("vooeys_question", "Question", width = "100%"),
                    textAreaInput(
                      "text_entry",
                      "Enter text: ",
                      width = "100%",
                      height = "200px"
                    ),
                    # Input: Select a file ----
                    fileInput(
                      "file1",
                      "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                    ),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Checkbox if file has header ----
                    checkboxInput("header", "Header", TRUE),
                    
                    # Input: Select separator ----
                    radioButtons(
                      "sep",
                      "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"
                      ),
                      selected = ","
                    ),
                    
                    # Input: Select quotes ----
                    radioButtons(
                      "quote",
                      "Quote",
                      choices = c(
                        None = "",
                        "Double Quote" = '"',
                        "Single Quote" = "'"
                      ),
                      selected = '"'
                    ),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Select number of rows to display ----
                    radioButtons(
                      "disp",
                      "Display",
                      choices = c(Head = "head",
                                  All = "all"),
                      selected = "head"
                    )
                    
                  )
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Data",
                             h2("Data"),
                             tableOutput("contents")),
                    tabPanel(
                      "Word Frequency",
                      h2("Word Frequency"),
                      plotOutput("freq_cloud"),
                      radioButtons(
                        "pos",
                        "Filter by:",
                        c(
                          "All" = "pos_all",
                          "Nouns" = "pos_nouns",
                          "Adjectives" = "pos_adjectives",
                          "Verbs" = "pos_verbs"
                        )
                      ),
                      textInput("remove_words", "Remove words:", width = "100%"),
                      dataTableOutput("freq_table")
                    ),
                    tabPanel(
                      "Parts of Speech",
                      h2("Parts of Speech"),
                      h4("Nouns"),
                      verbatimTextOutput("pos_debug1"),
                      h4("Verbs"),
                      verbatimTextOutput("pos_debug2"),
                      h4("Adjectives"),
                      verbatimTextOutput("pos_debug3")
                    ),
                    tabPanel(
                      "Sentiment",
                      h2("Sentence Sentiment"),
                      dataTableOutput("sent_table"),
                      h4("Filtered by Keyword Table"),
                      textInput("keyword", "Enter keyword: ", width = "500px"),
                      tableOutput("filtered_by_keyword_table")
                    ),
                    tabPanel("Topic Modeling"),
                    tabPanel("Auto-Coding")
                  )
                ))

server <- function(input, output) {
  udmodel_download <- udpipe_download_model(language = "english")
  udmodel <- udpipe_load_model(file = udmodel_download$file_model)
  
  tagged_pos_table <- reactive({
    text_entry_character_vector <-
      unlist(strsplit(input$text_entry, "(?<=[[:punct:]])\\s(?=[A-Z])", perl =
                        T))
    x <- udpipe_annotate(udmodel, text_entry_character_vector)
    x_as_frame <- as.data.frame(x)
    pos_tagged_words_table <- x_as_frame %>%
      select(token, upos)
    return(pos_tagged_words_table)
  })
  
  filter_by_pos_nouns <- reactive({
    nouns_only <- tagged_pos_table() %>%
      filter(upos == "NOUN") %>%
      select(token)
    return(unique(nouns_only$token))
  })
  
  filter_by_pos_verbs <- reactive({
    verbs_only <- tagged_pos_table() %>%
      filter(upos == "VERB") %>%
      select(token)
    return(unique(verbs_only$token))
  })
  
  filter_by_pos_adjectives <- reactive({
    adjectives_only <- tagged_pos_table() %>%
      filter(upos == "ADJ") %>%
      select(token)
    return(unique(adjectives_only$token))
  })
  
  calculate_frequency <- reactive({
    split_words <- unlist(strsplit(input$remove_words, ";"))
    themes_source <- VectorSource(input$text_entry)
    themes_corpus <- VCorpus(themes_source)
    other_words_to_remove <- split_words
    corpus <- tm_map(themes_corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <-
      tm_map(corpus, removeWords, c(stopwords("en"), other_words_to_remove))
    cleaned_themes_corpus <- corpus
    themes_tdm <- TermDocumentMatrix(cleaned_themes_corpus)
    themes_m <- as.matrix(themes_tdm)
    term_frequency <- rowSums(themes_m)
    term_frequency <- sort(term_frequency, decreasing = TRUE)
    term <- names(term_frequency)
    freq_frame <-
      data.frame(input$vooeys_question, term, term_frequency)
    if (input$pos == "pos_nouns") {
      freq_frame <- data.frame(input$vooeys_question, term, term_frequency)
      freq_frame <- freq_frame %>%
        filter(term %in% filter_by_pos_nouns())
    }
    if (input$pos == "pos_verbs") {
      freq_frame <- data.frame(input$vooeys_question, term, term_frequency)
      freq_frame <- freq_frame %>%
        filter(term %in% filter_by_pos_verbs())
    }
    if (input$pos == "pos_adjectives") {
      freq_frame <- data.frame(input$vooeys_question, term, term_frequency)
      freq_frame <- freq_frame %>%
        filter(term %in% filter_by_pos_adjectives())
    }
    names(freq_frame) <- c("question", "term", "term_frequency")
    freq_frame <-
      datatable(freq_frame,
                options = list(searchHighlight = TRUE),
                rownames = FALSE)
    return(freq_frame)
  })
  
  create_wordcloud <- reactive({
    split_words <- unlist(strsplit(input$remove_words, ";"))
    themes_source <- VectorSource(input$text_entry)
    themes_corpus <- VCorpus(themes_source)
    other_words_to_remove <- split_words
    corpus <- tm_map(themes_corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <-
      tm_map(corpus, removeWords, c(stopwords("en"), other_words_to_remove))
    cleaned_themes_corpus <- corpus
    themes_tdm <- TermDocumentMatrix(cleaned_themes_corpus)
    themes_m <- as.matrix(themes_tdm)
    term_frequency <- rowSums(themes_m)
    term_frequency <- sort(term_frequency, decreasing = TRUE)
    term <- names(term_frequency)
    freq_frame <-
      data.frame(input$vooeys_question, term, term_frequency)
    if (input$pos == "pos_nouns") {
      freq_frame <- data.frame(input$vooeys_question, term, term_frequency)
      freq_frame <- freq_frame %>%
        filter(term %in% filter_by_pos_nouns())
    }
    if (input$pos == "pos_verbs") {
      freq_frame <- data.frame(input$vooeys_question, term, term_frequency)
      freq_frame <- freq_frame %>%
        filter(term %in% filter_by_pos_verbs())
    }
    if (input$pos == "pos_adjectives") {
      freq_frame <- data.frame(input$vooeys_question, term, term_frequency)
      freq_frame <- freq_frame %>%
        filter(term %in% filter_by_pos_adjectives())
    }
    cloud <-
      wordcloud(
        words = freq_frame$term,
        freq = freq_frame$term_frequency,
        min.freq = 1,
        max.words = 200,
        random.order = FALSE,
        rot.per = 0.35
      )
    return(cloud)
  })
  
  calculate_sentiment <- reactive({
    sentences <- get_sentences(tolower(input$text_entry))
    sentence_sentiment <- sentiment(sentences)
    sent_frame <-
      data.frame(input$vooeys_question,
                 sentences,
                 sentence_sentiment$sentiment)
    names(sent_frame) <- c("question", "sentence", "sentiment")
    sent_frame <-
      datatable(sent_frame,
                options = list(searchHighlight = TRUE),
                rownames = FALSE)
    return(sent_frame)
  })
  
  calculate_sentiment_dataDotFrame <- reactive({
    sentences <- get_sentences(tolower(input$text_entry))
    sentence_sentiment <- sentiment(sentences)
    sent_frame <-
      data.frame(input$vooeys_question,
                 sentences,
                 sentence_sentiment$sentiment)
    names(sent_frame) <- c("question", "sentence", "sentiment")
    return(sent_frame)
  })
  
  filter_by_keyword <- reactive({
    filtered_by_keyword_table <-
      calculate_sentiment_dataDotFrame() %>%
      filter(grepl(input$keyword, sentence)) %>%
      summarize(average_sentiment = mean(sentiment))
    return(filtered_by_keyword_table)
  })
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({
      df <- read.csv(
        input$file1$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    if (input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$freq_table <- renderDataTable(calculate_frequency())
  output$freq_cloud <- renderPlot(create_wordcloud())
  output$sent_table <- renderDataTable(calculate_sentiment())
  output$filtered_by_keyword_table <-
    renderTable(filter_by_keyword())
  output$pos_debug1 <- renderPrint(filter_by_pos_nouns())
  output$pos_debug2 <- renderPrint(filter_by_pos_verbs())
  output$pos_debug3 <- renderPrint(filter_by_pos_adjectives())
}


shinyApp(ui = ui, server = server)
#Shiny Packages####
library(shiny)
library(shinythemes)
library(shinyDND)
#Data Rendering Packages####
library(DT)
library(rhandsontable)
library(shinyDND)
#General Packages####
library(tidyverse)
library(rlang)
library(xlsx)
#NLP####
library(tm)
library(topicmodels)
library(tidytext)
library(udpipe)
library(sentimentr)
library(wordcloud)
library(SnowballC)
library(textdata)
library(text2vec)
library(LDAvis)
library(ggraph)
library(igraph)
#Server Options####
options(shiny.maxRequestSize=1000*1024^2,shiny.sanitize.errors=FALSE,shiny.suppressMissingContextError=TRUE)
#Server####
server <- function(input, output){
  #Data ####
  loaded_data <- reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      return(NULL)
    }
    return(read.csv(inFile$datapath))
  })
  output$choose_variable_data_file <- renderUI(varSelectInput("choose_variable_source_data_file","Select variable:",loaded_data()))
  data_vector <- reactive({
    if(input$data_source == "source_data_file"){
      data_vector_chosen_variable_selected <- loaded_data() %>%
        select(!!input$choose_variable_source_data_file)
      data_vector <- as.character(data_vector_chosen_variable_selected[[1]])
      return(data_vector)
    }
    if(input$data_source == "source_note_taker"){
      data_vector <- hot_to_r(input$notetaker) %>% 
        select(!!input$choose_variable_source_note_taker)
      data_vector <- as.character(data_vector[[1]])
      return(data_vector)
    }
    if(input$data_source == "source_text_entry"){
      data_vector <- get_sentences(input$text_entry)
      data_vector <- unlist(data_vector)
      return(data_vector)
    }
  })
  dt_loaded_data <- reactive({DT::datatable(loaded_data(),editable='cell',rownames=FALSE)})
  output$datatable <- DT::renderDT(dt_loaded_data())
  #Frequency#### 
  calculate_frequency <- reactive({
    split_words <- unlist(strsplit(input$remove_words, ";"))
    vsource <- VectorSource(unlist(data_vector()))
    vcorpus <- VCorpus(vsource)
    clean_text <- function(corpus){
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, c(stopwords("en"), split_words))
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, stripWhitespace)
    }
    cleaned_corpus <- clean_text(vcorpus)
    tdm <- TermDocumentMatrix(cleaned_corpus)
    tdm_as_matrix <- as.matrix(tdm)
    term_frequency <- rowSums(tdm_as_matrix)
    term_frequency <- sort(term_frequency, decreasing = TRUE)
    term <- names(term_frequency)
    freq_frame <- data.frame(term, term_frequency)
    return(freq_frame)
  })
  freq_table <- reactive({
    datatable(calculate_frequency(),
              options = list(searchHighlight = TRUE),
              rownames = FALSE)
  })
  create_wordcloud <- reactive({
    cloud <-
      wordcloud(
        words = calculate_frequency()$term,
        freq = calculate_frequency()$term_frequency,
        min.freq = 1,
        max.words = 100,
        random.order = FALSE,
        rot.per = 0.35
      )
    return(cloud)
  })
  output$freq_table <- renderDataTable(freq_table())
  output$freq_cloud <- renderPlot(create_wordcloud())
  find_word_frequency <- reactive({
    text_df <- tibble(response=length(data_vector),text=data_vector())
    data(stop_words)
    text_df <- text_df %>%
      unnest_tokens(word,text, to_lower = TRUE) %>%
      anti_join(stop_words)
    text_df_plot <- text_df %>%
      count(word,sort=TRUE) %>%
      filter(n > input$minimum_frequency) %>%
      mutate(word = reorder(word,n)) %>%
      ggplot(aes(word,n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
    return(text_df_plot)
  })
  output$frequency <- renderPlot(find_word_frequency())
  find_bigrams <- reactive({
    count_bigrams <- function(dataset) {
      dataset %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word) %>%
        count(word1, word2, sort = TRUE)
    }
    visualize_bigrams <- function(bigrams) {
      set.seed(2016)
      a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
      
      bigrams %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()
    }
    text_df <- tibble(text=data_vector())
    text_bigrams <- text_df %>%
      count_bigrams() %>%
      filter(n>input$minimum_bigram_frequency) %>% 
      visualize_bigrams()
    return(text_bigrams)
  })
  output$bigrams <- renderPlot(find_bigrams())
  
  
  #Autocoding####
  
  loaded_codebook <- reactive({
    inFile <- input$codebook
    if(is.null(inFile)){
      return(NULL)
    }
    return(read.xlsx(inFile$datapath,sheetName = 1))
  })
  
  
  
  codes <- reactive({as.character(loaded_codebook()$Code)})
  keyphrases <- reactive({as.character(loaded_codebook()$Keyphrase)})
  create_keyphrase_list <- reactive({
    newlist <- list()
    for(c in 1:length(codes())){
      newlist[c] <- strsplit(keyphrases()[c],split=",")
    }
    names(newlist) <- codes()
    return(newlist)
    })
  assign_codes_to_text <- reactive({
    coded_data <- list()
    for(r in 1:length(data_vector())) {
      matched_keyphrases <- list()
      for (c in 1:length(create_keyphrase_list())) {
        for (k in 1:length(create_keyphrase_list()[[c]])) {
          if(grepl(create_keyphrase_list()[[c]][k],
                    data_vector()[r],
                    ignore.case = TRUE) == TRUE) {
            matched_keyphrases[c] <- names(create_keyphrase_list()[c])
          }
        }
      }
      codestring <- paste(unlist(matched_keyphrases), collapse = ";")
      coded_data[r] <- codestring
    }
    coded_data <- as.character(coded_data)
    return(coded_data)  
    
  })
  
  
  dt_coded_data <- reactive({
    add_codes_to_data_vector <- data.frame(data_vector(),assign_codes_to_text())
    names(add_codes_to_data_vector) <- c("Response","Matched Codes")
    data <- DT::datatable(add_codes_to_data_vector,rownames=FALSE)
    return(data)
    })
  
  output$coded_data <- DT::renderDT(dt_coded_data())
  
  dt_loaded_codebook <- reactive({DT::datatable(loaded_codebook(),rownames=FALSE)})
    output$codebook_datatable <- DT::renderDT(dt_loaded_codebook())
  
  #Sentiment####
  user_lexicon <-   
    loaded_lexicon <- reactive({
    inFile <- input$user_lexicon
    if(is.null(inFile)){
      return(NULL)
    }
    return(read.xlsx(inFile$datapath, sheetIndex = 1))
  })
    
  calculate_sentiment <- reactive({
    switch_lexicon <- switch(input$choose_lexicon,
                      jockers_rinker = lexicon::hash_sentiment_jockers_rinker,
                      jockers = lexicon::hash_sentiment_jockers,
                      huliu = lexicon::hash_sentiment_huliu,
                      loughran_mcdonald = lexicon::hash_sentiment_loughran_mcdonald,
                      nrc = lexicon::hash_sentiment_nrc,
                      senticnet = lexicon::hash_sentiment_senticnet,
                      sentiword = lexicon::hash_sentiment_sentiword,
                      slangsd = lexicon::hash_sentiment_slangsd,
                      socal_google = lexicon::hash_sentiment_socal_google,
                      lexicon::hash_sentiment_jockers
    )
    

    if(input$data_source=="source_text_entry"){
      sentences <- data_vector()
    }else if(input$data_source=="source_data_file"){
      sentences <- get_sentences(paste(data_vector(),collapse=" ")) 
    }else if(input$data_source=="source_note_taker"){
      sentences <- get_sentences(paste(data_vector(),collapse=" ")) 
    }

    if(is.null(user_lexicon())==TRUE){
      sentence_sentiment <-
      sentiment(sentences,
                polarity_dt = switch_lexicon,
                valence_shifters_dt = lexicon::hash_valence_shifters,
                hyphen = "",
                amplifier.weight = 0.8,
                n.before = Inf,
                n.after = Inf,
                question.weight = 1,
                adversative.weight = 0.25,
                neutral.nonverb.like = FALSE,
                missing_value = 0)
    sent_frame <- data.frame(sentences,sentence_sentiment$sentiment)
    names(sent_frame) <- c("sentence","sentiment")
    return(sent_frame)
  } else {
    sentence_sentiment <-
        
        sentiment(sentences,
                  polarity_dt = update_key(as_key(user_lexicon()),x = switch_lexicon),
                  valence_shifters_dt = lexicon::hash_valence_shifters, 
                  hyphen = "",
                  amplifier.weight = 0.8, 
                  n.before = Inf, 
                  n.after = Inf,
                  question.weight = 1, 
                  adversative.weight = 0.25,
                  neutral.nonverb.like = FALSE, 
                  missing_value = 0)
      sent_frame <- data.frame(sentences,sentence_sentiment$sentiment)
      names(sent_frame) <- c("sentence","sentiment")
      return(sent_frame)
    }
      
  })

  
  
  
  filter_by_keyword <- reactive({
    filtered_by_keyword_table <-   calculate_sentiment() %>% 
      filter(grepl(input$keyword,sentence)) %>% 
      summarize(average_sentiment = mean(sentiment))
    return(filtered_by_keyword_table)
  })
  
  output$sent_table <- DT::renderDT(datatable(calculate_sentiment()))
  output$average_sentiment <- renderTable(filter_by_keyword())
  output$lexicon_table <- DT::renderDT(datatable(user_lexicon()))
  
  
  
  #NoteTaker####
  nt_matrix <- matrix("",ncol=10,nrow=10)
  colnames(nt_matrix) <- c("A","B","C","D","E","F","G","H","I","J")
  nt_df <- as.data.frame(nt_matrix)
  nt_hot <- rhandsontable(nt_df,useTypes = FALSE) %>%
    hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE, customOpts = list(
      csv = list(name = "Download to CSV",
                 callback = htmlwidgets::JS(
                   "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"))))
  output$choose_variable_note_taker <- renderUI(varSelectInput("choose_variable_source_note_taker","Select a column from the note taker:",hot_to_r(input$notetaker)))
  output$notetaker <- renderRHandsontable(nt_hot)
  #output$test <- renderPrint(hot_to_r(input$notetaker))
  
  #Semantics####
  find_semantics<- eventReactive(input$create_topic_model,{
    tokens = tolower(data_vector())
    tokens = word_tokenizer(tokens)
    it = itoken(tokens, progressbar = FALSE)
    v = create_vocabulary(it)
    v = prune_vocabulary(v, term_count_min = 10, doc_proportion_max = 0.2)
    vectorizer = vocab_vectorizer(v)
    dtm = create_dtm(it, vectorizer, type = "dgTMatrix")
    lda_model = LDA$new(n_topics = input$number_of_topics, doc_topic_prior = 0.1, topic_word_prior = 0.01)
    doc_topic_distr = 
      lda_model$fit_transform(x = dtm, n_iter = 1000, 
                              convergence_tol = 0.001, n_check_convergence = 25, 
                              progressbar = FALSE)
    return(lda_model$plot())
  })
  
  output$semantics <- renderPlot(find_semantics())
  
  
  
  #New Section####
  # find_FEATURE<- reactive({
  #   
  # })
  # 
  # output$FEATURE <- renderPlot(find_FEATURE())

}
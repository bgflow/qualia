ui <- fluidPage(
  includeCSS("style.css"),
  br(),
  sidebarLayout(
        #Sidebar####
        sidebarPanel(
          h1("temba")
        ),
    mainPanel(
      tabsetPanel(
        #Home####
        tabPanel(
          "Home",
          br(),
          img(src="perception.jpg",width="25%"),
          br(),
          h2("Welcome to temba!"),
          p("temba is a natural language processing tool intended to help User Researchers quickly analyze text data."),
          br(),
          h4("Changelog:"),
          tags$li("Added support for keyphrase coding using a codebook"),
          tags$li("Added custom user sentiment lexicons"),
          tags$li("Added an in-app notetaker"),
          tags$li("Added ngrams"),
        ),
        #Upload####
        tabPanel(
          "Upload",
          h2("Choose Data Source"),
          radioButtons("data_source", "Select Data Source:",
                       c("Text Entry" = "source_text_entry",
                         "Data File" = "source_data_file",
                         "Note Taker" = "source_note_taker")),
          uiOutput("choose_variable_data_file"),
          h2("Upload Data File"),
          fileInput(
            "file1",
            "Select Dataset",
            multiple = FALSE,
            placeholder = "No file selected."),
          h2("Upload Codebook"),
          fileInput(
            "codebook",
            "Select Codebook:",
            multiple = FALSE),
          h2("Upload Lexicon:"),
          fileInput(
            "user_lexicon",
            "Select Lexicon:",
            multiple = FALSE),
          h2("Type in Text"),
          textAreaInput("text_entry","Enter Text:",value="Natural language processing (NLP) is a subfield of linguistics, computer science, information engineering, and artificial intelligence concerned with the interactions between computers and human (natural) languages, in particular how to program computers to process and analyze large amounts of natural language data.
Challenges in natural language processing frequently involve speech recognition, natural language understanding, and natural language generation.The history of natural language processing (NLP) generally started in the 1950s, although work can be found from earlier periods. In 1950, Alan Turing published an article titled Computing Machinery and Intelligence which proposed what is now called the Turing test as a criterion of intelligence[clarification needed].

The Georgetown experiment in 1954 involved fully automatic translation of more than sixty Russian sentences into English. The authors claimed that within three or five years, machine translation would be a solved problem.[2] However, real progress was much slower, and after the ALPAC report in 1966, which found that ten-year-long research had failed to fulfill the expectations, funding for machine translation was dramatically reduced. Little further research in machine translation was conducted until the late 1980s when the first statistical machine translation systems were developed.

Some notably successful natural language processing systems developed in the 1960s were SHRDLU, a natural language system working in restricted blocks worlds with restricted vocabularies, and ELIZA, a simulation of a Rogerian psychotherapist, written by Joseph Weizenbaum between 1964 and 1966. Using almost no information about human thought or emotion, ELIZA sometimes provided a startlingly human-like interaction. When the patient exceeded the very small knowledge base, ELIZA might provide a generic response, for example, responding to My head hurts with Why do you say your head hurts?.

During the 1970s, many programmers began to write conceptual ontologies, which structured real-world information into computer-understandable data. Examples are MARGIE (Schank, 1975), SAM (Cullingford, 1978), PAM (Wilensky, 1978), TaleSpin (Meehan, 1976), QUALM (Lehnert, 1977), Politics (Carbonell, 1979), and Plot Units (Lehnert 1981). During this time, many chatterbots were written including PARRY, Racter, and Jabberwacky.

Up to the 1980s, most natural language processing systems were based on complex sets of hand-written rules. Starting in the late 1980s, however, there was a revolution in natural language processing with the introduction of machine learning algorithms for language processing. This was due to both the steady increase in computational power (see Moore's law) and the gradual lessening of the dominance of Chomskyan theories of linguistics (e.g. transformational grammar), whose theoretical underpinnings discouraged the sort of corpus linguistics that underlies the machine-learning approach to language processing.[3] Some of the earliest-used machine learning algorithms, such as decision trees, produced systems of hard if-then rules similar to existing hand-written rules. However, part-of-speech tagging introduced the use of hidden Markov models to natural language processing, and increasingly, research has focused on statistical models, which make soft, probabilistic decisions based on attaching real-valued weights to the features making up the input data. The cache language models upon which many speech recognition systems now rely are examples of such statistical models. Such models are generally more robust when given unfamiliar input, especially input that contains errors (as is very common for real-world data), and produce more reliable results when integrated into a larger system comprising multiple subtasks.

Many of the notable early successes occurred in the field of machine translation, due especially to work at IBM Research, where successively more complicated statistical models were developed. These systems were able to take advantage of existing multilingual textual corpora that had been produced by the Parliament of Canada and the European Union as a result of laws calling for the translation of all governmental proceedings into all official languages of the corresponding systems of government. However, most other systems depended on corpora specifically developed for the tasks implemented by these systems, which was (and often continues to be) a major limitation in the success of these systems. As a result, a great deal of research has gone into methods of more effectively learning from limited amounts of data.

Recent research has increasingly focused on unsupervised and semi-supervised learning algorithms. Such algorithms can learn from data that has not been hand-annotated with the desired answers or using a combination of annotated and non-annotated data. Generally, this task is much more difficult than supervised learning, and typically produces less accurate results for a given amount of input data. However, there is an enormous amount of non-annotated data available (including, among other things, the entire content of the World Wide Web), which can often make up for the inferior results if the algorithm used has a low enough time complexity to be practical.

In the 2010s, representation learning and deep neural network-style machine learning methods became widespread in natural language processing, due in part to a flurry of results showing that such techniques[4][5] can achieve state-of-the-art results in many natural language tasks, for example in language modeling,[6] parsing,[7][8] and many others. Popular techniques include the use of word embeddings to capture semantic properties of words, and an increase in end-to-end learning of a higher-level task (e.g., question answering) instead of relying on a pipeline of separate intermediate tasks (e.g., part-of-speech tagging and dependency parsing). In some areas, this shift has entailed substantial changes in how NLP systems are designed, such that deep neural network-based approaches may be viewed as a new paradigm distinct from statistical natural language processing. For instance, the term neural machine translation (NMT) emphasizes the fact that deep learning-based approaches to machine translation directly learn sequence-to-sequence transformations, obviating the need for intermediate steps such as word alignment and language modeling that was used in statistical machine translation (SMT).
                        
In the early days, many language-processing systems were designed by hand-coding a set of rules:[9][10] such as by writing grammars or devising heuristic rules for stemming.

Since the so-called statistical revolution[11][12] in the late 1980s and mid-1990s, much natural language processing research has relied heavily on machine learning. The machine-learning paradigm calls instead for using statistical inference to automatically learn such rules through the analysis of large corpora (the plural form of corpus, is a set of documents, possibly with human or computer annotations) of typical real-world examples.

Many different classes of machine-learning algorithms have been applied to natural-language-processing tasks. These algorithms take as input a large set of features that are generated from the input data. Some of the earliest-used algorithms, such as decision trees, produced systems of hard if-then rules similar to the systems of handwritten rules that were then common. Increasingly, however, research has focused on statistical models, which make soft, probabilistic decisions based on attaching real-valued weights to each input feature. Such models have the advantage that they can express the relative certainty of many different possible answers rather than only one, producing more reliable results when such a model is included as a component of a larger system.

Systems based on machine-learning algorithms have many advantages over hand-produced rules:

The learning procedures used during machine learning automatically focus on the most common cases, whereas when writing rules by hand it is often not at all obvious where the effort should be directed.
Automatic learning procedures can make use of statistical inference algorithms to produce models that are robust to unfamiliar input (e.g. containing words or structures that have not been seen before) and to erroneous input (e.g. with misspelled words or words accidentally omitted). Generally, handling such input gracefully with handwritten rules, or, more generally, creating systems of handwritten rules that make soft decisions, is extremely difficult, error-prone and time-consuming.
Systems based on automatically learning the rules can be made more accurate simply by supplying more input data. However, systems based on handwritten rules can only be made more accurate by increasing the complexity of the rules, which is a much more difficult task. In particular, there is a limit to the complexity of systems based on handcrafted rules, beyond which the systems become more and more unmanageable. However, creating more data to input to machine-learning systems simply requires a corresponding increase in the number of man-hours worked, generally without significant increases in the complexity of the annotation process.                        
                        
                        
                        ",width="100%",height="150px")
        ),
        #Frequency####
        tabPanel(
          "Frequency",
          br(),
          h2("Wordcloud"),
          plotOutput("freq_cloud"),
          textInput("remove_words", "Remove words:", width = "100%"),
          h2("Frequency Table"),
          DT::DTOutput("freq_table"),
          h2("Frequency (graph format)"),
          plotOutput("frequency"),
          sliderInput("minimum_frequency",label="Minimum word frequency",value=10,min=1,max=1000,step=1),
          h2("N-Grams"),
          plotOutput("bigrams"),
          sliderInput("minimum_bigram_frequency",label="Minimum N-gram frequency",value=10,min=1,max=500,step=1)
      ),
        #Autocoding####
        tabPanel("Autocoding",
          h2("Keyphrase Coding"),
          DT::DTOutput("coded_data"),
          h2("Codebook"),
          DT::DTOutput("codebook_datatable"),
      ),
        #Sentiment####
        tabPanel(
          "Sentiment",
          br(),
          h2("Sentence-level Sentiment"),
          DT::DTOutput("sent_table"),
          selectInput("choose_lexicon",
                      "Select a Lexicon",
                      c(  "lexicon_jockers_rinker" = "jockers_rinker",
                          "lexicon_jockers" = "jockers",
                          "lexicon_huliu" = "huliu",
                          "lexicon_loughran_mcdonald" = "loughran_mcdonald",
                          "lexicon_nrc" = "nrc",
                          "lexicon_senticnet" = "senticnet",
                          "lexicon_sentiword" = "sentiword",
                          "lexicon_slangsd" = "slangsd",
                          "lexicon_socal_google" = "socal_google"
                      ),
                      selected="reactive_lexicon_jockers"
          ),
          h2("Average Sentiment by Keyword"),
          textInput("keyword","Enter keyword: ",width="500px"),
          tableOutput("average_sentiment"),        
          h2("Your Lexicon"),
          DT::DTOutput("lexicon_table"),
        ),
        #NoteTaker####
        tabPanel(
          "NoteTaker",
          br(),
          uiOutput("choose_variable_note_taker"),
          rhandsontable::rHandsontableOutput("notetaker"),
          verbatimTextOutput("test")
        )
      ,
        #Semantics####
      tabPanel(
        "Semantics",
        h2("Semantics"),
        actionButton("create_topic_model","Create Topic Model"),
        sliderInput("number_of_topics","Number of Topics",2,100,step=1,value=2),
        plotOutput("semantics")
      )
      #,
      #,
        #New Tab####
      # tabPanel(
      #   "Sample Tab",
      #   h2("Sample Tab")
      # )
      #
    )
  )
)
)

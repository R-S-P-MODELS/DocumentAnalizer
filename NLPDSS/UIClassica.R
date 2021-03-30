ui <- fluidPage(
   
   # Application title
   titlePanel("Document Analyzer"),
  
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput('Files','Insert Documents as PDF files',accept='pdf',multiple = TRUE,placeholder = 'Awaiting files'),
        uiOutput('RenderTokenizer'),
        uiOutput('Ngramgenerator'),
        selectInput('preprocesser','Select Preprocessing strategies',choices=c('lower','punctuation','numbers','whitespace','stopwords'),multiple=TRUE,selected = 'lower'),
        uiOutput('GeneratingPossibleLanguages'),
        uiOutput('GenerateNgramsTDM'),
        uiOutput('GenerateKindOfWordCloud'),
        uiOutput('GenerateNumberOfWordsInWordCloud'),
        #numericInput('NgramsTDM','Ngram for Term Document Matrix',min=1,max=10,value=1),
        uiOutput('GenerateNumberOfConnections'),
        uiOutput('GenerateNumberOfTopics'), #For LDA
        #numericInput('NumberOfConnections','Maximum number of connections considered in the network',value=100,min=3,max=1e5),
        actionButton('Run','Run preprocessing steps'),
        actionButton('RunTDM','Run analysis on preprocessed data'),
        uiOutput('GenerateExportNetwork')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id = 'Tab',
        tabPanel('Network', plotOutput("SeeNetwork") ),
        tabPanel('Topic Modelling',plotOutput('LDA')),
        tabPanel('Word Clouds',plotOutput('WordClouds')), #Aqui inserir algoritmos de word clouds
        tabPanel('About',h4('Application developed by Rafael Silva Pereira'),
                 h4('For more details please contact r.s.p.models@gmail.com')
                 )
      ) )
   )
)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Function that takes
MapListToMatrix<-function(df){
  require(igraph)
  Network=graph_from_data_frame(df)
  Adjacency=as_adjacency_matrix(Network,sparse = FALSE)
  df[,1]=as.character(df[,1])
  df[,2]=as.character(df[,2])
  for(i in 1:nrow(df)){
    Adjacency[df[i,1],df[i,2]]=df[i,3]
  }
  return(Adjacency)
}


library(shiny)
source('Functions.R')
#source('UIClassica.R')
source('UIDashboard.R')
# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1*1024^3)
############################################################################### Code Block to define outputs   
  
  
  #Generates the numeric input for number of words inword clouds
  output$GenerateNumberOfWordsInWordCloud<-renderUI({
    if(input$Tab=='WordCloudstab')
      numericInput('NumberOfWordsInWordCloud','Number of words in word cloud',min=1,max=100,value=10)
  })
  #Generates the input type for word cloud algorithm
  output$GenerateKindOfWordCloud<-renderUI({
    if(input$Tab=='WordCloudstab'){
      selectInput('KindOfWordCloud','Select Word Cloud type',choices=c('Comparison Cloud','Commonality Cloud'))
    }
  })
  #Generates the download button when the user selects the network tab, the download button returns a csv file with the adjacency matrix of the generated network
  output$GenerateExportNetwork<-renderUI({
    if(input$Tab=='Network'){
      downloadButton('ExportNetwork','Export Network as adjacency matrix')
    }
  })
  
  output$GenerateNormalizer<-renderUI({
    if(input$Tab=="Network")
      selectInput('Normalizer','Select normalization strategy for term document matrix',choices=c('none','tf-idf'))
  })
  
  output$GenerateMaxWords<-renderUI({
    numericInput('MaxWords','Maximum number of words to be considered after preprocessing',min = 10,max=1e5,value=1000)
  })
  
  #Download handler associated with the download button.
  output$ExportNetwork<-downloadHandler(
    filename = function() {
      paste("AdjacencyMatrix", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data=GenerateAdjacencyList()
      data=MapListToMatrix(data)
      write.csv(data, file,row.names=FALSE)
    }
  
  )
  # Creates a button to select the language of the stopwords if stopwords are selected as a preprocessing strategy
  output$GeneratingPossibleLanguages<-renderUI({
    if('stopwords' %in% input$preprocesser)
        selectInput('language','Select language of the documents',choices=c('pt','en') )
    
  })
  
  output$GeneratingNewStopWords<-renderUI({
    if('stopwords' %in% input$preprocesser)
      textAreaInput('NewStopWords','Write additional stop words here separated by ,' )
    
  })
  #Generates the tokenization strategy, chosing an renderUI allows us to create a condition when there is a tab that does not require it
  output$RenderTokenizer<-renderUI({
    selectInput('Tokenizer','Select tokenization method',choices=c('lines','sentences','paragraphs','ngrams','none'))
    
  })
  # Generates the input for the size of the ngram in the term document matrix
  output$GenerateNgramsTDM<-renderUI({
    if(input$Tab!='FrequencyTable'){
      numericInput('NgramsTDM','Ngram for Term Document Matrix',min=1,max=10,value=1)
    }else{
      numericInput('NgramsTDM','Ngrams to calculate frequency',min=1,max=10,value=1)
    }
  })
  #If the tokenization method is a ngram creates an input  value to put the size of ngram
  output$Ngramgenerator<-renderUI({
    if(input$Tokenizer=='ngrams')
      numericInput('Ngrams','Select size of ngram',min=1,max=10,value=2)
    
  })
  
  #If network is selected create an inpur for the number of connections to be displayed, returns strongest networks.
   output$GenerateNumberOfConnections<-renderUI({
     if(input$Tab=='Network')
      numericInput('NumberOfConnections','Maximum number of connections considered in the network',value=100,min=3,max=1e5)
   })
   #If topic modelling is selected creates an input to select the number of topics
   output$GenerateNumberOfTopics<-renderUI({
     if(input$Tab=='TopicModelling')
      numericInput('NumberOfTopics',label = 'Number of topics to cluster',min=1,max=100,value = 2)
   })
   ################################################# end of input elements, beggining of output elements
   GenerateAdjacencyListEvent<-eventReactive(input$Run,{
     return(GenerateAdjacencyList() )
   })
   
   #The following function creates the network visualization, it uses the ggraph package to deliver an graph visualization based on ggplot sintax
   #It receives the top n strongest links of the graph as an adjacency list, and builds the network object to plot it.
   output$SeeNetwork<-renderPlot({
     AdjacencyList=GenerateAdjacencyListEvent()
     require(igraph)
     require(ggraph)
     #m=m[m[,3]>1000,]
     wordnetwork=graph_from_data_frame(AdjacencyList)
     p<-ggraph(wordnetwork, layout = "fr") +
       geom_edge_link(aes(width = Weights, edge_alpha = Weights), edge_colour = "pink") +
       geom_node_label(aes(label = name),label.size=1,repel=TRUE, col = "darkgreen", size = 4) +
       theme_graph(base_family = "Arial Narrow") +
       theme(legend.position = "none")  #+ labs(title = "")
     print(p)
     
   })
   
   LdaEvent<-eventReactive(input$Run,{
     LDA=GenerateLDAReac()
     VisualizeLDA(LDA)
   })
   
   
   #This function plots the most important terms for each topic according to Latent dirichilet allocation (LDA)
   output$LDA<-renderPlot({
     LdaEvent()
   })
   #This function plots word clouds according to the algorithm of choice
   output$WordClouds<-renderPlot({
     termdoc=TDM()
     N=input$NumberOfWordsInWordCloud
     if(input$KindOfWordCloud=='Commonality Cloud'){
     out <- tryCatch(
                
       {         CommonCloud(termdoc,N)
        
       },
       error=function(cond) {
         par(mar = c(0,0,0,0))
         plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
         text(x = 0.5, y = 0.5, paste("There re no common terms in the current tokenization method between documents"), 
              cex = 1.6, col = "black")
         par(mar = c(5, 4, 4, 2) + 0.1)
         
       }
    )
     }
     else if(input$KindOfWordCloud=='Comparison Cloud'){
       CompCloud(termdoc,MaxWords = N)
     }
    # out
     
    # CompCloud(termdoc,MaxWords=N)
   })
    
   ProcessarTexto<-reactive({
     Content=ReadFiles()
     Text=unlist(Content)
     Arguments=input$preprocesser
     language=input$language
     if('lower' %in% Arguments)
       Text=tolower(Text)
     if('punctuation' %in% Arguments)
       Text=removePunctuation(Text)
     #Texto=tm_map(Texto,removePunctuation)
     if('numbers' %in% Arguments)
       Text=removeNumbers(Text)
     #Texto=tm_map(Texto,removeNumbers)
     if('whitespace' %in% Arguments)
       Text=stripWhitespace(Text)
     #Texto=tm_map(Texto,stripWhitespace)
     if('stopwords' %in% Arguments){
       NewWords=unlist(strsplit(input$NewStopWords,split=",",fixed=TRUE)  )
       RemovableWords=c(stopwords(language),NewWords)
       Text=removeWords(Text,words=RemovableWords)
     }
     return(Text)
     
   })
   
   output$FrequencyTable<-renderDataTable({
    Text=ProcessarTexto()
     #Text=TidyFormat(Text,'ngrams',input$ngrams)
     Text=TidyFormat(Text,'ngrams',input$NgramsTDM)
    # print(Text)
     require(dplyr)
     Frequency=Text %>% count(Text) %>% arrange(desc(n))
     print(Frequency)
   })
   
   LimitMaxWords<-reactive({
     Text=ProcessarTexto()
     Text=TidyFormat(Text,'ngrams',input$NgramsTDM)
     # print(Text)
     require(dplyr)
     Frequency=Text %>% count(Text) %>% arrange(desc(n))
     #print(Frequency)
     if(nrow(Frequency)<=input$MaxWords){
       WordsToRemove=c()
     }else{
       WordsToRemove=Frequency$Text[ (input$MaxWords+1):nrow(Frequency)] #Aqui isolamos as palavras que não aparecem pelo menos que as k palavras mais frequentes
     }
     #print(WordsToRemove[1:3])
     return(WordsToRemove)
   })
   ############################################################################## end of output definition
   
   ##############################################################################Code block to define reactive and eventReactive functions
   
   #Function to read pdf files 
   ReadFiles<-reactive({
     if(!is.null(input$Files) ){
       content=list()
       for(i in 1:length(input$Files$datapath) ){
        content[[i]]=leitura(input$Files$datapath[i])
       }
     }
     return(content)
    })
   #The following function takes text from multiple files and reorganizes then into a tibble which contains at each line the text in respect to the desired tokenization.
   #When tokenization is none each line of the document corresponds to a line in the tibble
   FormatTextIntoTidy<-reactive({
     Content=ReadFiles()
     if(input$Tokenizer=='none'){
       for(i in 1:length(Content))
        Content[[i]]=data.frame(Document=i,Text=Content[[i]])
     }else{
       for(i in 1:length(Content))
         Content[[i]]=TidyFormat(Content[[i]],input$Tokenizer,input$Ngrams)
     }
     Text=do.call(rbind,Content)
     return(Text)
   })
   #The following function aims to preprocess the text according to preprocessing strategies inputted by the user, it returns a volatile corpus object to be fed to the term document matrix function
   PreprocessedText<-reactive({
     TidiedText=FormatTextIntoTidy()
     NewWords=unlist(strsplit(input$NewStopWords,split=",",fixed=TRUE)  )
     Preprocessed= Preprocessing(TidiedText$Text,input$preprocesser,input$language,NewWords)
     return(Preprocessed)
   })
   
   CleanedText<-reactive({
     Text=PreprocessedText()
     NewWordsToRemove=LimitMaxWords()
     print(length(NewWordsToRemove))
     print(length(unique(NewWordsToRemove)))
     print(NewWordsToRemove[1:3])
     WordsPerIteration=500
     if(length(NewWordsToRemove)>0){
       NumeroDeVezes=as.integer(length(NewWordsToRemove)/WordsPerIteration)
       if(NumeroDeVezes>0){
         withProgress(message = 'Removing Words', value = 0, {
           
           for (remocoes in 1:NumeroDeVezes){
             Text=tm_map(Text,removeWords,words=NewWordsToRemove[ (1+(remocoes-1)*WordsPerIteration):(remocoes*WordsPerIteration) ])        
             incProgress(1/NumeroDeVezes, detail = paste("Removed ", remocoes*WordsPerIteration,"Words."))
             
           }
         })
         #print(remocoes)
         if(length(NewWordsToRemove)>(NumeroDeVezes*WordsPerIteration) )
           Text=tm_map(Text,removeWords,words=NewWordsToRemove[(1+(remocoes)*WordsPerIteration):length(NewWordsToRemove) ])
       }
     }
     
     return(Text)
     
     
     
   })
   #The following function creates the term document matriz from the volatile corpus according to user parameters
   TDM<-reactive({
     require(RWeka)
     Text=CleanedText()
      #Enquanto o bloco acima poderia ser apenas uma chamada de removewords passando todo o vetor de newwordstoremove, verificamos que quando este numero e muito grande ele gera uma expressão regular grande demais para que a função a processe, então resolvi fazer iterativamente.
     print(input$NgramsTDM)
     Peso=input$Normalizer
     if(input$Tab=='TopicModelling')
       Peso='None'
     TermDocMatrix=GenerateTDM(Text,input$NgramsTDM,Normalizacao=Peso )
     print(TermDocMatrix)
     return(TermDocMatrix)
   })
   #The following function generates an adjacency list with the strongest links and filters for links with no connection
   GenerateAdjacencyList<-reactive({
     TermDoc<-TDM()
     Adjacency<-WordNetworkList(TermDoc,input$NumberOfConnections)
     Adjacency=Adjacency[Adjacency[,3]>0,]
     print(Adjacency)
   })
   
   #The following function calculates the latent dirichilet allocation algorithm on the term document matrix
   GenerateLDAReac<-reactive({
     matriz=TDM()
     matriz=matriz[apply(matriz,1,sum)>0,]
    return(GenerateLDA(matriz,input$NumberOfTopics) )
   })
   

   
#################### end of reactive and event reactive functions   
}

# Run the application 
shinyApp(ui = ui, server = server)


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
  #Generates the tokenization strategy, chosing an renderUI allows us to create a condition when there is a tab that does not require it
  output$RenderTokenizer<-renderUI({
    selectInput('Tokenizer','Select tokenization method',choices=c('lines','sentences','paragraphs','ngrams','none'))
    
  })
  # Generates the input for the size of the ngram in the term document matrix
  output$GenerateNgramsTDM<-renderUI({
    numericInput('NgramsTDM','Ngram for Term Document Matrix',min=1,max=10,value=1)
    
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
   
   #The following function creates the network visualization, it uses the ggraph package to deliver an graph visualization based on ggplot sintax
   #It receives the top n strongest links of the graph as an adjacency list, and builds the network object to plot it.
   output$SeeNetwork<-renderPlot({
     AdjacencyList=GenerateAdjacencyList()
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
   #This function plots the most important terms for each topic according to Latent dirichilet allocation (LDA)
   output$LDA<-renderPlot({
     LDA=GenerateLDAReac()
     VisualizeLDA(LDA)
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
   FormatTextIntoTidy<-eventReactive(input$Run,{
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
   PreprocessedText<-eventReactive(input$Run,{
     TidiedText=FormatTextIntoTidy()
     Preprocessed= Preprocessing(TidiedText$Text,input$preprocesser,input$language)
     return(Preprocessed)
   })
   #The following function creates the term document matriz from the volatile corpus according to user parameters
   TDM<-eventReactive(input$RunTDM,{
     require(RWeka)
     Text=PreprocessedText()
     print(input$NgramsTDM)
     Peso='tf-idf'
     if(input$Tab=='TopicModelling')
       Peso='None'
     TermDocMatrix=GenerateTDM(Text,input$NgramsTDM,Normalizacao=Peso )
     print(TermDocMatrix)
     return(TermDocMatrix)
   })
   #The following function generates an adjacency list with the strongest links and filters for links with no connection
   GenerateAdjacencyList<-eventReactive(input$RunTDM,{
     TermDoc<-TDM()
     Adjacency<-WordNetworkList(TermDoc,input$NumberOfConnections)
     Adjacency=Adjacency[Adjacency[,3]>0,]
     print(Adjacency)
   })
   
   #The following function calculates the latent dirichilet allocation algorithm on the term document matrix
   GenerateLDAReac<-eventReactive(input$RunTDM,{
     matriz=TDM()
     
    return(GenerateLDA(matriz,input$NumberOfTopics) )
   })
   
   
   
#################### end of reactive and event reactive functions   
}

# Run the application 
shinyApp(ui = ui, server = server)


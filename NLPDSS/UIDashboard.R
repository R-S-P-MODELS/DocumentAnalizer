library(shiny)
 library(shinydashboard)
library(shinydashboardPlus)



titulo<-dashboardHeaderPlus(title = "Document Analyzer",titleWidth = 450,enable_rightsidebar = TRUE, rightSidebarIcon = "gears",disable = FALSE)
 
                           

menu<- dashboardSidebar(width = 250,
 tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
    sidebarMenu(id="Tab",
      menuItem("Network", tabName = "Network", icon = icon("dashboard")),
      menuItem("Topic Modelling", tabName = "TopicModelling", icon = icon("dashboard")),
      #menuItem("Word Clouds",tabName="WordCloudstab",icon=icon("dashboard")),
      menuItem('Frequency Table',tabName = 'FrequencyTable',icon=icon('dashboard')),
menuItem("About", tabName = "About", icon = icon("th"))

    ),

       
	      uiOutput('RenderTokenizer'),
        uiOutput('Ngramgenerator'),
        selectInput('preprocesser','Select Preprocessing strategies',choices=c('lower','punctuation','numbers','whitespace','stopwords'),multiple=TRUE,selected = 'lower'),
        uiOutput('GeneratingPossibleLanguages'),
        uiOutput('GeneratingNewStopWords'),
        uiOutput('GenerateNgramsTDM'),
        uiOutput('GenerateKindOfWordCloud'),
        uiOutput('GenerateNumberOfWordsInWordCloud'),
        #numericInput('NgramsTDM','Ngram for Term Document Matrix',min=1,max=10,value=1),
        uiOutput('GenerateNumberOfConnections'),
        uiOutput('GenerateNumberOfTopics'), #For LDA
        uiOutput('GenerateMaxWords'),
        uiOutput('GenerateNormalizer')
        #numericInput('NumberOfConnections','Maximum number of connections considered in the network',value=100,min=3,max=1e5),
               
  )



## Body content
 corpo<- dashboardBody(
      fileInput('Files','Insert Documents as PDF files',accept='pdf',multiple = TRUE,placeholder = 'Awaiting files'),
      #textOutput("res"),

    tabItems(
      # First tab content
      tabItem(tabName = "Network",
        fluidRow(
         plotOutput("SeeNetwork")

          
        )
      ),

tabItem(tabName = "TopicModelling",
        fluidRow(
          plotOutput("LDA")
	  
          
        )
      ),

#tabItem(tabName = "WordCloudstab",
 #       fluidRow(
#	plotOutput('WordClouds')

          
 #       )
  #    ),


tabItem(tabName = "FrequencyTable",
        fluidRow(
          dataTableOutput('FrequencyTable')
          
          
        )
),



tabItem(tabName = "About",
        fluidRow(
          h4('Application developed by Rafael Silva Pereira'),
           h4('For more details please contact r.s.p.models@gmail.com')
          
        )
      )

      
    ),
conditionalPanel(condition='input.Tab!="About" ',
 	actionButton('Run','Run Analysis')
        #actionButton('RunTDM','Run analysis on preprocessed data')
),
        uiOutput('GenerateExportNetwork')
)




ui <- dashboardPagePlus(titulo,menu,corpo,skin="blue",title = "Document Analyzer" )


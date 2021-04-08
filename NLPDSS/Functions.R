
leitura=function(caminho){
library(tm)
read <- readPDF(control = list(text = "-layout"))
document <- Corpus(URISource(caminho), readerControl = list(reader = read))
doc <- content(document[[1]])
return(doc)
}

Palavras=function(texto,linguagem){
alvo=strsplit(texto," ")
for(i in 1:length(alvo)){
	alvo[[i]]=alvo[[i]][alvo[[i]]!=""] #eliminando espacos

}
alvo=unlist(alvo)

alvo=alvo[which(!(alvo %in% stopwords(kind=linguagem)))]

#alvo= setdiff(alvo,stopwords(kind="pt"))

return(alvo)
}

ConversorMatriz=function(z,reject){
cont=1

u=list()
u1=c()
for(i in 1:length(unique(z))){
if(length(which(unique(z)[i]==z) )>reject ){

u[[cont]]=which(unique(z)[i]==z)
u1[[cont]]=unique(z)[i]
cont=cont+1

}
#u=u[[-1]]
}
conjunto=list(u,u1) #u1 são os nomes de u
#return(u)
return(conjunto)
}


TidyFormat<-function(Text,token,n=2){ #token should be one of the following: "words",ngrams,skip_ngrams,sentences,lines,paragraphs,regex
require(tidytext)
require(dplyr)
if(token %in% c('ngrams','skip_ngrams') ){
Output=tibble(txt=Text) %>%   unnest_tokens(Text, txt,token=token,n=n)
}else{
Output=tibble(txt=Text) %>%   unnest_tokens(Text, txt,token=token)
}
return(Output)
}


clean_corpus <- function(corpus,stopwords) {
require(tm)
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus,content_transformer( tolower) )
  # Add more stopwords
  corpus <- tm_map(corpus, removeWords, words = stopwords)
  # Strip whitespace
  corpus<-tm_map(corpus,stripWhitespace)
  return(corpus)
}

common_cloud<-function(all_corpus){
require(wordcloud)
#all_clean <- clean_corpus(all_corpus)
all_clean=all_corpus
# Create all_tdm
all_tdm <- TermDocumentMatrix(all_clean)

# Create all_m
all_m <- as.matrix(all_tdm)

# Print a commonality cloud
commonality.cloud(all_m, max.words = 100, colors = "steelblue1")

}


CommonCloud<-function(tdm,nwords){
  require(wordcloud)
  
  WordMatrix <- as.matrix(tdm)
  ToRemove=which(colSums(WordMatrix)==0)
  WordMatrix=WordMatrix[,-ToRemove]
print(class(WordMatrix) )
print(sum(colSums(WordMatrix)==0) )
aux=dim(WordMatrix)
  if(aux[1]>0){
  # Print a commonality cloud
  	commonality.cloud(WordMatrix, max.words = nwords, colors = "steelblue1")
  }
  else{
	par(mar = c(0,0,0,0))
	plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
	text(x = 0.5, y = 0.5, paste("There re no common terms in the current tokenization method between documents"), 
     	cex = 1.6, col = "black")
	par(mar = c(5, 4, 4, 2) + 0.1)
  }
}


CompCloud<-function(all_tdm,VarNames='',stopwords=stopwords('pt' ),MaxWords ){
require(wordcloud)
# Clean the corpus
#all_clean <- clean_corpus(all_corpus,stopwords)
#all_clean=all_corpus
# Create all_tdm
#all_tdm <- TermDocumentMatrix(all_clean)

# Give the columns distinct names
#colnames(all_tdm) <- VarNames

# Create all_m
all_m <- as.matrix(all_tdm)
  ToRemove=which(colSums(all_m)==0)
  all_m=all_m[,-ToRemove]
# Create comparison cloud
comp <- comparison.cloud(all_m, max.words = MaxWords, colors = c("orange", "blue"))
return(comp)
}



GerarRedePalavra<-function(Texto,stopwords,string){
require(qdap)
# Word association
word_associate(Texto, match.string = string, 
               stopwords = stopwords, 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))

# Add title
#title(main = "Barista Coffee Tweet Associations")


}

#tokenizer <- function(x) {
 #  NGramTokenizer(x, Weka_control(min = 2, max = 2))
#}

#weighting = weightTfIdf #Argumento para termdocumentmatriz

tokenizer <- function(x) {
require(RWeka)
   NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

MatrizCoocorreencia<-function(Texto,Conectividade,Ngrams,Peso='TfIdf'){
require(RWeka)

tokenizer <- function(x) {
require(RWeka)
   NGramTokenizer(x, Weka_control(min = Ngrams, max = Ngrams))
}


if(tolower(Peso)=='tfidf'){
	x<-DocumentTermMatrix(Texto,control=list(tokenize = tokenizer,weighting = weightTfIdf ) )
}else{
	x<-DocumentTermMatrix(Texto,control=list(tokenize = tokenizer ) )
}
x=removeSparseTerms(x, 1-Conectividade)
x=crossprod(as.matrix(x) )
return(x)
}



Preprocessing<-function(Text,Arguments,language='en',NewWords=c()){ #Arguments is a vector that contains lower,punctuation,numbers,whitespace,stopwords

#Texto=VCorpus(VectorSource(Text))
if('lower' %in% Arguments)
	Text=tolower(Text)
	#Texto=tm_map(Texto,tolower)
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
	#Texto=tm_map(Texto,removeWords,words=stopwords(language))
	#Text=removeWords(Text,words=stopwords(language))
	#NewWords=unlist(strsplit(input$NewStopWords,split=",",fixed=TRUE)  )
	RemovableWords=c(stopwords(language),NewWords)
	Text=removeWords(Text,words=RemovableWords)
}
Texto=VCorpus(VectorSource(Text))
return(Texto)
}


GenerateTDM<-function(Text,Ngrams=c(1,1),Normalizacao='tf-idf'  ){

 # Ngrams=2
  
tokenizer <- function(x) {
require(RWeka)
   NGramTokenizer(x, Weka_control(min = min(Ngrams), max = max(Ngrams) ))
}
print(Ngrams)
require(tm)
if(Normalizacao=='tf-idf'){
  x<-DocumentTermMatrix(Text,control=list(tokenize = tokenizer,weighting = weightTfIdf ) )
}else{
    x<-DocumentTermMatrix(Text,control=list(tokenize = tokenizer) )
}
return(x)
}

WordNetworkMatrix<-function(tdm,max.words=100){


x=as.matrix(tdm)
x<-crossprod(x)
PontoCorte=x[order(x,decreasing = TRUE)[max.words]]
x[x<PontoCorte]=0
Indices=which(x>0,arr.ind = TRUE)

Adjacencia=x[unique(Indices[,1]),unique(Indices[,2] )] 
return(Adjacencia)
}


WordNetworkList<-function(tdm,max.words=100){
  #A tdm pode conter muitos elementos e a conversão para matriz estourar a memoria, para limitar isto eu extraio os indices nao nulos da matriz tdm que tende a ser esparsa e converto na matriz esparsa x
  require(Matrix) 
  x=sparseMatrix(i =tdm$i,j = tdm$j,x = tdm$v,dims=c(tdm$nrow,tdm$ncol) )
  x<-crossprod(x)
  #x=as.matrix(x)
  #PontoCorte=x[order(x,decreasing = TRUE)[max.words]]
  #x[x<PontoCorte]=0
  #Indices=which(x>0,arr.ind = TRUE)
  diag(x)=0 #Zeroing auto connections
  x1=x  
  l=list()
  withProgress(message = 'Finding Top k connections', value = 0, {
    for(i in 1:max.words){
      maximo=max(x1)
      ind=which(x1==maximo,arr.ind = TRUE)
      if(nrow(ind)>1)
        ind=ind[1,]
      l[[i]]=ind
      x1[ind[1],ind[2] ]=0
      x1[ind[2],ind[1]  ]=0
      incProgress(1/max.words, detail = paste("Found connection", i))
    }  
  } )
  Indices=do.call(rbind,l)
  #Adjacencia=data.frame(Source=1:max.words,Target=1:max.words,Weights=1:max.words)
  #Source=rownames(x)[Indices[,1]]
  
  #Aqui extraio a lista de adjacencias das top max.words conexoes, source e a origem, target o destino e weights o peso da conexão
  Source=tdm$dimnames$Terms[Indices[,1]]
  
  #Target=colnames(x)[Indices[,2]]
  Target=tdm$dimnames$Terms[Indices[,2]]
  
  Weights=c(x[Indices])
  Adjacencia=data.frame(Source,Target,Weights)
  #Adjacencia$Source=rownames(x)[Indices[,1]]
  #Adjacencia$Target=colnames(x)[Indices[,2]]
  #Adjacencia$Weights=c(x[Indices])
  #Adjacencia=x[unique(Indices[,1]),unique(Indices[,2] )] 
  return(Adjacencia)
}

WordNetworkListNonShiny<-function(tdm,max.words=100){
  #A tdm pode conter muitos elementos e a conversão para matriz estourar a memoria, para limitar isto eu extraio os indices nao nulos da matriz tdm que tende a ser esparsa e converto na matriz esparsa x
  require(Matrix) 
  x=sparseMatrix(i =tdm$i,j = tdm$j,x = tdm$v,dims=c(tdm$nrow,tdm$ncol) )
  x<-crossprod(x)
  #x=as.matrix(x)
  #PontoCorte=x[order(x,decreasing = TRUE)[max.words]]
  #x[x<PontoCorte]=0
  #Indices=which(x>0,arr.ind = TRUE)
  diag(x)=0 #Zeroing auto connections
  x1=x  
  l=list()

    for(i in 1:max.words){
      maximo=max(x1)
      ind=which(x1==maximo,arr.ind = TRUE)
      if(nrow(ind)>1)
        ind=ind[1,]
      l[[i]]=ind
      x1[ind[1],ind[2] ]=0
      x1[ind[2],ind[1]  ]=0

    }  

  Indices=do.call(rbind,l)
  #Adjacencia=data.frame(Source=1:max.words,Target=1:max.words,Weights=1:max.words)
  #Source=rownames(x)[Indices[,1]]
  
  #Aqui extraio a lista de adjacencias das top max.words conexoes, source e a origem, target o destino e weights o peso da conexão
  Source=tdm$dimnames$Terms[Indices[,1]]
  
  #Target=colnames(x)[Indices[,2]]
  Target=tdm$dimnames$Terms[Indices[,2]]
  
  Weights=c(x[Indices])
  Adjacencia=data.frame(Source,Target,Weights)
  #Adjacencia$Source=rownames(x)[Indices[,1]]
  #Adjacencia$Target=colnames(x)[Indices[,2]]
  #Adjacencia$Weights=c(x[Indices])
  #Adjacencia=x[unique(Indices[,1]),unique(Indices[,2] )] 
  return(Adjacencia)
}



WordNetworkListOld<-function(tdm,max.words=100){
  #A tdm pode conter muitos elementos e a conversão para matriz estourar a memoria, para limitar isto eu extraio os indices nao nulos da matriz tdm que tende a ser esparsa e converto na matriz esparsa x
  require(Matrix) 
  x=sparseMatrix(i =tdm$i,j = tdm$j,x = tdm$v,dims=c(tdm$nrow,tdm$ncol) )
  x<-crossprod(x)
  #x=as.matrix(x)
  PontoCorte=x[order(x,decreasing = TRUE)[max.words]]
  x[x<PontoCorte]=0
  Indices=which(x>0,arr.ind = TRUE)
  #Adjacencia=data.frame(Source=1:max.words,Target=1:max.words,Weights=1:max.words)
  #Source=rownames(x)[Indices[,1]]
  
  #Aqui extraio a lista de adjacencias das top max.words conexoes, source e a origem, target o destino e weights o peso da conexão
  Source=tdm$dimnames$Terms[Indices[,1]]
  
  #Target=colnames(x)[Indices[,2]]
  Target=tdm$dimnames$Terms[Indices[,2]]
  
  Weights=c(x[Indices])
  Adjacencia=data.frame(Source,Target,Weights)
  #Adjacencia$Source=rownames(x)[Indices[,1]]
  #Adjacencia$Target=colnames(x)[Indices[,2]]
  #Adjacencia$Weights=c(x[Indices])
  #Adjacencia=x[unique(Indices[,1]),unique(Indices[,2] )] 
  return(Adjacencia)
}



GenerateLDA<-function(dtm,topics=2){
# Load the topicmodels package
require(topicmodels)

# Run an LDA with 2 topics and a Gibbs sampler
lda_out <- LDA(
  dtm,
  k = topics,
  method = 'Gibbs'
)

lda_topics <- lda_out %>% 
  tidy('beta')

return(lda_topics)
}

VisualizeLDA<-function(lda_topics){
require(ggplot2)
require(forcats)
word_probs2 <- lda_topics %>% 
  group_by(topic) %>% 
  top_n(15,beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term,beta)   )


ggplot(
  word_probs2, 
  aes(term2,beta,fill=as.factor(topic) )
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

}


VisualizeHierarchicalCluster<-function(tdm){

# Create tweets_dist
TextDistance <- dist(tdm)

# Create hc
hc <- hclust(TextDistance)

# Plot the dendrogram
#plot(hc)
return(hc)
}




# Create associations

EncontrarTermosMaisCorrelacionados<-function(Texto,Termo,Correlacao,plot=TRUE){
require(ggplot2)
require(tm)
require(qdap)
require(RWeka)
Ngrams=length(unlist(strsplit(Termo," "))) #Descobrindo ngram pelo termo de busca

tokenizer <- function(x) {
   NGramTokenizer(x, Weka_control(min = Ngrams, max = Ngrams))
}


tweets_tdm=TermDocumentMatrix(Texto,control=list(tokenize=tokenizer) )
associations <- findAssocs(tweets_tdm, Termo, Correlacao)

# View the venti associations
associations

# Create associations_df
associations_df <- list_vect2df(associations, col2='word', col3='score')

if(plot){
# Plot the associations_df values
p<-ggplot(associations_df, aes(score, word)) + 
  geom_point(size = 3) #+ 
#  theme_gdocs()
print(p)
}
return(associations_df)
}



DiferencaEntreDoisTextosPiramidade<-function(Texto1,Texto2){
require(dplyr)
require(tm)
require(qdap)
Texto1=colSums( as.matrix(DocumentTermMatrix(Texto1) ) )
Texto2=colSums( as.matrix(DocumentTermMatrix(Texto2) ) )

Texto1=data.frame(Termo=names(Texto1),Valor1=as.numeric(Texto1))
Texto2=data.frame(Termo=names(Texto2),Valor2=as.numeric(Texto2))
all_tdm_df=Texto1 %>% inner_join(Texto2)

common_words <- all_tdm_df %>% 
  filter(
    Valor1 != 0,
    Valor2 != 0
  ) %>%
  mutate(diff = abs(Valor1 - Valor2))

# Extract top 5 common bigrams
(top5_df <- top_n(common_words,5, diff))

ggplot(top5_df) + geom_bar(aes(x=Termo,y=Valor1,fill='First Text'),stat='identity') + geom_bar(aes(x=Termo,y=-Valor2,fill='Second Text'),stat='identity') +coord_flip() + labs(fill='Text')
#return(top5_df)
# Create the pyramid plot
#pyramid.plot(top5_df$Valor1, top5_df$Valor2, 
#             labels = top5_df$Termo, gap = 12, 
#             top.labels = c("Text1", "Pro Words", "Text2"), 
#             main = "Words in Common", unit = NULL)


}



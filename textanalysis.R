library(tm)
require(tm)
library(SnowballC)
library(wordcloud)
library(cluster)

# read in new data as list of strings -- no demo data availible 

docs<- Corpus(VectorSource(new_data))
#remove stopwords
docs<-tm_map(docs, removePunctuation)   # *Removing punctuation:*  
docs <- tm_map(docs, removeNumbers)      # *Removing numbers:*    
docs <- tm_map(docs, tolower)   # *Converting to lowercase:*    
docs <- tm_map(docs, removeWords, stopwords("english"))   # *Removing "stopwords" 
docs <- tm_map(docs, stemDocument)   # *Removing common word endings* (e.g., "ing", "es")   
docs <- tm_map(docs, stripWhitespace)   # *Stripping whitespace   
docs <- tm_map(docs, PlainTextDocument)  

dtm = DocumentTermMatrix(docs)
tdm<-TermDocumentMatrix(docs)

#check frequency of the terms
freq<-colSums(as.matrix(dtm))
freq
length(freq)   
ord<-order(freq)
ord
dtm
#condense data to 10% less empty space
dtms<-removeSparseTerms(dtm, 0.1)
freq[head(ord)]  
freq[tail(ord)]   

head(table(freq), 20)   
tail(table(freq), 20)  
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   

#find low frequency terms
findFreqTerms(dtm, lowfreq=10) 
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  


#plot word frequency in ggplot2
library(ggplot2)   
p <- ggplot(subset(wf, freq>2), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p

#find association words
findAssocs(dtm, c("city" , "comfort"), corlimit=0.8) # specifying a correlation limit of 0.8   
findAssocs(dtm, "city", corlimit=0.90)


set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)   
set.seed(142)   
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   


#clustering by term similarity 
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
library(cluster)   
d <- dist(t(dtm), method="euclidian")   

fit <- hclust(d=d, method="ward.D") #using hierarchy clustering and plot dendrogram
fit   
plot(fit, hang=-1) 
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   

### K-means clustering   
dtms <- removeSparseTerms(dtm, 0.4) # Prepare the data (max 15% empty space)   
d <- dist(t(dtm), method="euclidian")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   



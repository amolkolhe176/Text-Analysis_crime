#Required Pakages

library(tm) #for corpus and term document matrix creation/processing
library(SnowballC) #for stemming
library(wordcloud)
library(cluster)
library(rpart)

# Reading the clean file

crime <- readRDS(file = "crime_cleaned_data")

#Creating a file for text analysis

crime_text <- data.frame(crime$desc)  
names(crime_text) <- "desc"

crime_text$desc <- as.character(crime_text$desc)

## remove / to a space
crime_text$desc <- gsub("/","",   crime_text$desc)

## remove : to a space
crime_text$desc <- gsub(":","",   crime_text$desc)
## remove - to a space
crime_text$desc <- gsub("-","",   crime_text$desc)


#list of stopwords
stopwords("english")


#create corpus and clean up text before creating document term matrix
crime_corpus <- Corpus(VectorSource(crime_text$desc))

crime_corpus <- tm_map(crime_corpus, stemDocument)
crime_corpus <- tm_map(crime_corpus, removePunctuation)
crime_corpus <- tm_map(crime_corpus, removeNumbers)
crime_corpus <- tm_map(crime_corpus, removeWords, stopwords("english"))
crime_corpus <- tm_map(crime_corpus, stripWhitespace) 


#create term document matrix (terms as rows, documents as columns)
tdm <- TermDocumentMatrix(crime_corpus)

#count row (i.e, terms)
tdm$nrow 

#inspect the term document matrix, make sure to subset it is very large 
inspect(tdm[1:30, 1:30])

#remove words that are over 98% sparse 
tdm <- removeSparseTerms(tdm, 0.98)
tdm$nrow #now 46 terms
tdm$ncol #100000 desc
inspect(tdm[1:46, 1:3])

inspect(tdm)


# define tdm as matrix
m = as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d #lets see frequency of words

# plot wordcloud
set.seed(100)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Checking the association of the most frequent words
findAssocs(tdm, terms = c("vehicle", "theft"), corlimit = .0)

findAssocs(tdm, terms = c("owners"), corlimit = .0) 



#bar chart of frequent words
barplot(d[2:10,]$freq, las = 2, names.arg = d[2:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#clustering the documents, find optimal k
wss <- numeric(15) 
for (k in 1:10) wss[k] <- sum(kmeans(tdm, centers=k)$withinss)
plot(wss, type="b") #seems like 2 or 3 will cover it

crime.kmeans <- kmeans(tdm,3)
crime.kmeans$cluster #lets looks at cluster membership

tdm$cluster <- crime.kmeans$cluster
length(tdm[tdm$cluster==1,]$words)
tdm[tdm$cluster==2,]$words





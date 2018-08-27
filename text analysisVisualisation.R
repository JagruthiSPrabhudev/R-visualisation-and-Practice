library(dplyr)
library(ggplot2)
library(tm)
library(tmap)
library(RColorBrewer)     
library(wordcloud)
library(qdap)
library(corpus)

reviews = read.csv("S:\\Admission and Planning\\graduate_student_worker_2013\\Jagruthi\\Python tutorials\\reviews.csv", stringsAsFactors = F,sep="|")

#############################Word clouds####################
#Function to make a word cloud. Can specify the dataframe, the filename of the resulting
# wordcloud, the colorschee, and additional words you don't want to appear (common
# english stopwords are already removed.) This code is adapted from:
#http://www.r-bloggers.com/word-cloud-in-r/
makewordcloud <- function(data, column, filename, colorscheme = "BuGn", extraRemove = NULL)
{
  data.corpus <- Corpus(VectorSource(data))
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  data.corpus <- tm_map(data.corpus, content_transformer(function(x) removeWords(x, c(stopwords("english"), 
                                                                                      extraRemove))))
  tdm <- TermDocumentMatrix(data.corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  pal <- brewer.pal(9, colorscheme)
  pal <- pal[-(1:2)]
  png(filename, width=1280,height=800)
  wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, 
            random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
  dev.off()
}

#Positive/negative reviews
positive = subset(reviews, label=='positive')
negative = subset(reviews, label=='negative')

#Sample 35000 rows from positive and negative reviews. The wordcloud function
# cannot handle the entire postive and negative dataframes.
positivesample = positive[sample(nrow(positive), 40000),]
negativesample = negative[sample(nrow(negative), 40000),]
#frequent_terms <- freq_terms(positivesample, 30)
#plot(frequent_terms)

#Make wordclouds.
makewordcloud(data = positivesample, column = 'text', 
              filename = 'positiveSampleText.png', colorscheme = 'Greens')
makewordcloud(data = negativesample, column = 'text', 
              filename = 'negativeSampleText.png', colorscheme = 'OrRd')
makewordcloud(data = reviews, column = 'text', 
              filename = 'TotalsampleText.png', colorscheme = 'Greens')

n <- nrow(reviews[reviews$text.length >= 8000])
data <- reviews[reviews$text.length <= 8000]
hist(data$review_length, 
     ylim = c(0,3000), 
     main = "Distribution of review length" )




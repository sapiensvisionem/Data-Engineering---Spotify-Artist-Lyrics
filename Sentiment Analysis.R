library(tidytext)
library(dplyr)
library(stringr)

# text data has been made clean in excel - removed the following: " ' , . ! # $ % ^ & * ( ) [ ]
data=read.table("/Users/jihun/Desktop/LyricsOnly.csv",header=FALSE, fill=TRUE, sep="@")
length(data$V1)
data = as.vector(data$V1)

n = length(data)
sentscore = numeric(n)
options(warn=-1)
for (i in 1:n) {
  lyric = data[i]
  if (lyric == "URL Does Not Exist") {
    sentscore[i] = 0
  }
  else {
    words <- strsplit(lyric,split=" ")
    text <- data.frame(words)
    names(text)[1]<-"word"
    test <- text %>% inner_join(get_sentiments("afinn"))
    sentscore[i] <- sum(test$score)
  }
}
# write.table(sentscore, 'Users/jihun/Desktop/sentimentscore.csv', sep = ',',row.names = F)

# histogram
library(ggvis)

data.frame(sentscore) %>% filter(!is.na(sentscore)) %>%
  ggvis(~sentscore) %>%
  layer_histograms()
# histogram (same)
library(ggplot2)

data.frame(sentscore) %>% 
  ggplot(aes(sentscore)) + 
  geom_histogram()

# full data
fulldata <- read.table("/Users/jihun/Desktop/ArtistSong.csv", header=FALSE, fill=TRUE, sep=",", 
                       col.names = c("Artist","Song"))
fulldata$lyrics <- data
fulldata$stream <- read.table("/Users/jihun/Desktop/Streams.csv", header=TRUE)
fulldata$sentscore <- sentscore
stream <- read.table("/Users/jihun/Desktop/Streams.csv", header=TRUE)
stream <- stream$stream
# how negative is each sentiment?
# look into the

# correlation between sentscore and stream: -0.12
# negative songs tend to be 
with(fulldata,
     cor(sentscore,stream))
lmod <- lm(stream ~ sentscore)
plot(sentscore,stream)
lines(sentscore,lmod$fitted.values)

# divide sentscore into factors -> # cluster by artist -> # ad wants to use # heat map (sentscore)+ bubble chart, 

write.csv(fulldata, '/UserS/jihun/Desktop/jamesfulldata.csv')

# get top 10 streamed artists with positive sentiment scores - find the most popular artists
# who have good songs
fulldata2 <- read.table("/Users/jihun/Desktop/ArtistSong.csv", header=FALSE, fill=TRUE, sep=",", 
                        col.names = c("Artist","Song"))
fulldata2$sentscore <- sentscore
fulldata2$stream <- stream
head(fulldata2[sentscore>0 & order(fulldata2$stream,decreasing=TRUE,na.last=NA),],10)

# cluster analysis: 5 clusters
mydata <- fulldata2[,c(3,4)]
fit <- kmeans(mydata, centers=6) # why 5? visually
fulldata2$cluster <- fit$cluster # what are the clusters? 1-6
# hist(fit$cluster) # distribution of cluster
(meancluster=aggregate(mydata,by=list(fit$cluster),FUN=mean))     # mean of each cluster

# Cluster plot (standardized)
library(cluster) 
partialdata <- fulldata2[,c(3,4)]
# row.names(partialdata, make.names=FALSE) <- fulldata2[,2]
clusplot(partialdata, fit$cluster, stand=TRUE, color=TRUE, shade=TRUE, 
         labels=4, lines=0, span=FALSE, xlab="Stream Numbers", ylab="Sentiment Score", main="Cluster Plot")

# I want to 
## SKJ AKA Raja Hummushit
setwd("/Users/IIMS Bloomberg ONE/Downloads/Kaggle_Do_NOT_Delete/FPM003/Hackathon/EA/FaceData/")

df.bjp <- read.csv("bjp.csv")
View(df.bjp)
dim(df.bjp)## 39986    10

### Other than english 
length(noneng.match <- grep("\\?", df.bjp$message)) ## 9237

df.bjp.eng <- df.bjp[-noneng.match,]
dim(df.bjp.eng)
View(df.bjp.eng)
sum(is.na(df.bjp.eng)) #### Blank spaces though 

textdata.bjp <- df.bjp.eng$message
View(textdata.bjp)

sum(!trimws(textdata.bjp, 'b') == "")
textdata.bjp <- textdata.bjp[!trimws(textdata.bjp, 'b') == ""]
sum(trimws(textdata.bjp, 'b') == "")


### Let's Play

textdata.bjp = gsub("[[:punct:]]", "", textdata.bjp)
textdata.bjp = gsub("[[:punct:]]", "", textdata.bjp)
textdata.bjp = gsub("[[:digit:]]", "", textdata.bjp)
textdata.bjp = gsub("http\\w+", "", textdata.bjp)
textdata.bjp = gsub("[ \t]{2,}", "", textdata.bjp)
textdata.bjp = gsub("^\\s+|\\s+$", "", textdata.bjp)

## classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv(system.file("data/emotions.csv.gz",package="sentiment"),header=FALSE)
  
  counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[,2]==key),]
        index <- pmatch(word,emotions[,1],nomatch=0)
        if (index > 0) {
          entry <- emotions[index,]
          
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          
          score <- 1.0
          if (algorithm=="bayes") score <- abs(log(score*prior/count))
          
          if (verbose) {
            print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
          }
          
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
  }
  
  colnames(documents) <- c("ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
  return(documents)
}
source("classify_emotion.R")
source("create_matrix.R")
source("classify_polarity.R")
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}


test = sapply(textdata.bjp, try.error)

textdata.bjp <- textdata.bjp[!trimws(textdata.bjp, 'b') == ""]
sum(trimws(textdata.bjp, 'b') == "")

sum(is.na(textdata.bjp))

#### Text mining :: The real one 
require(tm)
require(SnowballC)
class_emo = classify_emotion(textdata.bjp, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(textdata.bjp, algorithm="bayes")
polarity = class_pol[,4]

save.image("bjpCong.RData")


sent_df = data.frame(text=textdata.bjp, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


require(ggplot2)
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")


emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = textdata.bjp[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
require(SnowballC)
require(tm)

require(wordcloud)
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE,
                 title.size = 1.5)

plot(tdm, corThreshold = 0.2, weighting = TRUE)

v <- sort(rowSums(tdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(d$word,d$freq)


########### 


setwd("/Users/IIMS Bloomberg ONE/Downloads/Kaggle_Do_NOT_Delete/FPM003/Hackathon/EA/FaceData/")

df.bjp <- read.csv("cong.csv")
View(df.bjp)
dim(df.bjp)## 31759    10

### Other than english 
length(noneng.match <- grep("\\?", df.bjp$message)) ## 9237

df.bjp.eng <- df.bjp[-noneng.match,]
dim(df.bjp.eng)
View(df.bjp.eng)
sum(is.na(df.bjp.eng$message)) #### Blank spaces though 

textdata.bjp <- df.bjp.eng$message
View(textdata.bjp)

sum(!trimws(textdata.bjp, 'b') == "")
textdata.bjp <- textdata.bjp[!trimws(textdata.bjp, 'b') == ""]
sum(trimws(textdata.bjp, 'b') == "")


### Let's Play

textdata.bjp = gsub("[[:punct:]]", "", textdata.bjp)
textdata.bjp = gsub("[[:punct:]]", "", textdata.bjp)
textdata.bjp = gsub("[[:digit:]]", "", textdata.bjp)
textdata.bjp = gsub("http\\w+", "", textdata.bjp)
textdata.bjp = gsub("[ \t]{2,}", "", textdata.bjp)
textdata.bjp = gsub("^\\s+|\\s+$", "", textdata.bjp)

## 
##classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
matrix <- create_matrix(textColumns,...)
lexicon <- read.csv(system.file("data/emotions.csv.gz",package="sentiment"),header=FALSE)

counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
documents <- c()

for (i in 1:nrow(matrix)) {
  if (verbose) print(paste("DOCUMENT",i))
  scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
  doc <- matrix[i,]
  words <- findFreqTerms(doc,lowfreq=1)
  
  for (word in words) {
    for (key in names(scores)) {
      emotions <- lexicon[which(lexicon[,2]==key),]
      index <- pmatch(word,emotions[,1],nomatch=0)
      if (index > 0) {
        entry <- emotions[index,]
        
        category <- as.character(entry[[2]])
        count <- counts[[category]]
        
        score <- 1.0
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        if (verbose) {
          print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
        }
        
        scores[[category]] <- scores[[category]]+score
      }
    }
  }
  
  if (algorithm=="bayes") {
    for (key in names(scores)) {
      count <- counts[[key]]
      total <- counts[["total"]]
      score <- abs(log(count/total))
      scores[[key]] <- scores[[key]]+score
    }
  } else {
    for (key in names(scores)) {
      scores[[key]] <- scores[[key]]+0.000001
    }
  }
  
  best_fit <- names(scores)[which.max(unlist(scores))]
  if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
  documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
}

colnames(documents) <- c("ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
return(documents)
}
source("classify_emotion.R")
source("create_matrix.R")
source("classify_polarity.R")
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}


textdata.bjp = sapply(textdata.bjp, try.error)

textdata.bjp <- textdata.bjp[!trimws(textdata.bjp, 'b') == ""]
sum(trimws(textdata.bjp, 'b') == "")

sum(is.na(textdata.bjp))

#### Text mining :: The real one 
require(tm)
require(SnowballC)
class_emo = classify_emotion(textdata.bjp, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(textdata.bjp, algorithm="bayes")
polarity = class_pol[,4]

save.image("CongBjp.RData")


sent_df = data.frame(text=textdata.bjp, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


require(ggplot2)
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")


emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = textdata.bjp[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
require(SnowballC)
require(tm)

require(wordcloud)
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE,
                 title.size = 1.5)

plot(tdm, corThreshold = 0.2, weighting = TRUE)

v <- sort(rowSums(tdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(d$word,d$freq)



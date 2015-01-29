############### Set-Up Packages ################
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(stringr)
library(plyr)
library(sentiment)
############ Different for User ############
setwd("C:\\Users\\jlewyckyj\\Desktop\\TweetProject")

#### Install Sentiment Package ####
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
require(sentiment)
ls("package:sentiment")

############# Breen's Approach ##############

# function score.sentiment
score.sentiment = function(sentences, pos.words, neg.words, 
.progress='none')
{
   # Parameters
   # sentences: vector of text to score
   # pos.words: vector of words of postive sentiment
   # neg.words: vector of words of negative sentiment
   # .progress: passed to laply() to control of progress bar

   # create simple array of scores with laply
   scores = laply(sentences,
   function(sentence, pos.words, neg.words)
   {
      # remove punctuation
      sentence = gsub("[[:punct:]]", "", sentence)
      # remove control characters
      sentence = gsub("[[:cntrl:]]", "", sentence)
      # remove digits?
      sentence = gsub('\\d+', '', sentence)

      # define error handling function when trying tolower
      tryTolower = function(x)
      {
         # create missing value
         y = NA
         # tryCatch error
         try_error = tryCatch(tolower(x), error=function(e) e)
         # if not an error
         if (!inherits(try_error, "error"))
         y = tolower(x)
         # result
         return(y)
      }
      # use tryTolower with sapply 
      sentence = sapply(sentence, tryTolower)

      # split sentence into words with str_split (stringr package)
      word.list = str_split(sentence, "\\s+")
      words = unlist(word.list)

      # compare words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)

      # get the position of the matched term or NA
      # we just want a TRUE/FALSE
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)

      # final score
      score = sum(pos.matches) - sum(neg.matches)
      return(score)
      }, pos.words, neg.words, .progress=.progress )

   # data frame with scores for each sentence
   scores.df = data.frame(text=sentences, score=scores)
   return(scores.df)
}

# import positive and negative words
pos = readLines(".\\positive_words.txt")
neg = readLines(".\\negative_words.txt")

tweets <- read.csv(file=".\\TweetsDB.csv",head=TRUE,sep=",")

tweettext <- tweets$text
scores <- score.sentiment(tweettext, pos, neg, .progress='text')

############# Breen's Score Chart #####################
pdf(".\\BreenScores.pdf")
# plot distribution of scores
ggplot(fullData, aes(x=score)) +
geom_bar(aes(y=..count.., fill=score)) +
scale_fill_brewer(palette="Dark2") +
labs(x="score", y="number of tweets",title = "Sentiment Analysis of Tweets about Banks") +
theme(plot.title = element_text(size=12)) +
scale_x_continuous(breaks=seq(-4,5,1))
# turn off #
dev.off()


scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)
global_score = round(100*numpos / (numpos+numneg))

################### Breen's Spreadsheet ###########################
write.csv(scores, ".\\Breen.csv")


#### Some Frequency Analysis ####

tweetTextc <- as.character(tweettext)
chars_Per_Tweet = sapply(tweettextc, nchar)
words_List = strsplit(tweettextc, " ")
words_Per_Tweet = sapply(words_list, length)
barplot(table(chars_per_tweet), border= NA,
   main="Distribution of characters per tweet", cex.main=1)
barplot(table(words_per_tweet), border= NA,
   main="Distribution of words per tweet", cex.main=1)

# most frequent words
mfw = sort(table(unlist(words_list)), decreasing=TRUE)
# top-20 most frequent
top20Words = head(mfw, 20)
top50Words = head(mfw, 50)
top100Words = head(mfw, 100)
barplot(top100words, border=NA, las=2, main="Top 50 most frequent terms",
cex.main=1)

# get the hashtags
hashtags = str_extract_all(tweettext, "#\\w+")
# put tags in vector
hashtags_V = unlist(hashtags)
# calculate hashtag frequencies
hashtags_Freq = table(hashtags_V)
# hashtags wordcloud
pdf(".\\HashtagWordcloud.pdf")
wordcloud(names(hashtags_Freq), hashtags_Freq, random.order=FALSE,
    colors="#1B9E77")
title("\n\nHashtags in Tweets about Banks",
    cex.main=1.5, col.main="gray50")
dev.off()


############ Sentiment Package #######################

# remove retweet entities
tweettextz = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweettext)
# remove at people
tweettextz = gsub("@\\w+", "", tweettextz)
# remove punctuation
tweettextz = gsub("[[:punct:]]", "", tweettextz)
# remove html links
tweettextz = gsub("http\\w+", "", tweettextz)
# define "tolower error handling" function 
try.error = function(x)
{
   # create missing value
   y = NA
   # tryCatch error
   try_error = tryCatch(tolower(x), error=function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
   y = tolower(x)
   # result
   return(y)
}
# lower case using try.error with sapply 
tweettextz = sapply(tweettextz, try.error)
# remove NAs in tweettextz
tweettextz = tweettextz[!is.na(tweettextz)]
names(tweettextz) = NULL

# classify emotion
class_emo = classify_emotion(tweettextz, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"
# classify polarity
class_pol = classify_polarity(tweettextz, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

sentimentresults <- data.frame(text=tweettextz, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
sentimentresults = within(sentimentresults,
   emotion <- factor(emotion, levels=names(sort(table(emotion),
  decreasing=TRUE))))

sentimentresultsPlus <- data.frame(text=tweettextz, anger=class_emo[,1], disgust=class_emo[,2], fear=class_emo[,3], joy=class_emo[,4], sadness=class_emo[,5], surprise=class_emo[,6], emotion=emotion, pos=class_pol[,1], neg=class_pol[,2], pos_neg=class_pol[,3], polarity=polarity, stringsAsFactors=FALSE)   

################ Sentiment Package Spreadsheet ##########################
write.csv(sentimentresultsPlus, ".\\sentiment.csv")

#################### Combined Spreadsheet ####################
fullData <- cbind(tweets,scores,sentimentresultsPlus)
write.csv(fullData, ".\\FullData.csv")

############ Sentiment Package Histogram Charts #####################
pdf(".\\Emotion.pdf")
# plot distribution of emotions
ggplot(fullData, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
labs(x="emotion categories", y="number of tweets",title = "Sentiment Analysis of Tweets about Banks") +
theme(plot.title = element_text(size=12))
dev.off()

pdf(".\\Polarity.pdf")
# plot distribution of polarity
ggplot(fullData, aes(x=polarity)) +
geom_bar(aes(y=..count.., fill=polarity)) +
scale_fill_brewer(palette="Dark2") +
labs(x="polarity categories", y="number of tweets",title = "Sentiment Analysis of Tweets about Banks") +
theme(plot.title = element_text(size=12))
dev.off()

############# Sentiment Package Emotion Comparison Cloud ##############

# separating text by emotion
emos = levels(factor(sentimentresults$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
   tmp = tweettextz[emotion == emos[i]]
   emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

pdf(".\\Wordcloud_Emotion.pdf")
# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
   scale = c(3,.5), random.order = FALSE, title.size = 1.5)
dev.off()

############## Sentiment Score Function #####################

sentimentScore <- function(df) {
  length(df$polarity[df$polarity=="positive"])/length(df$polarity[df$polarity=="negative"])
}

################ Separate by Search Term #########################
tweetsBofA <- subset(fullData, grepl("@BofA_Help", fullData[[1]]), drop = TRUE)
write.csv(tweetsBofA, ".\\TweetsbyBank\\BofA.csv")
sentimentBofA <- sentimentScore(tweetsBofA)

tweetsCiti <- subset(fullData, grepl("@AskCiti", fullData[[1]]), drop = TRUE)
write.csv(tweetsCiti, ".\\TweetsbyBank\\Citi.csv")
sentimentCiti <- sentimentScore(tweetsCiti)

tweetsChase <- subset(fullData, grepl("@ChaseSupport", fullData[[1]]), drop = TRUE)
write.csv(tweetsChase, ".\\TweetsbyBank\\Chase.csv")
sentimentChase <- sentimentScore(tweetsChase)

tweetsWellsFargo <- subset(fullData, grepl("@Ask_WellsFargo", fullData[[1]]), drop = TRUE)
write.csv(tweetsWellsFargo, ".\\TweetsbyBank\\WellsFargo.csv")
sentimentWellsFargo <- sentimentScore(tweetsWellsFargo)

tweetsUSBank <- subset(fullData, grepl("@askusbank", fullData[[1]]), drop = TRUE)
write.csv(tweetsUSBank, ".\\TweetsbyBank\\USBank.csv")
sentimentUSBank <- sentimentScore(tweetsUSBank)

tweetsPNC <- subset(fullData, grepl("@PNCBank_Help", fullData[[1]]), drop = TRUE)
write.csv(tweetsPNC, ".\\TweetsbyBank\\PNC.csv")
sentimentPNC <- sentimentScore(tweetsPNC)

tweetsCapitalOne <- subset(fullData, grepl("@AskCapitalOne", fullData[[1]]), drop = TRUE)
write.csv(tweetsCapitalOne, ".\\TweetsbyBank\\CapitalOne.csv")
sentimentCapitalOne <- sentimentScore(tweetsCapitalOne)

tweetsSunTrust <- subset(fullData, grepl("@AskSunTrust", fullData[[1]]), drop = TRUE)
write.csv(tweetsSunTrust, ".\\TweetsbyBank\\SunTrust.csv")
sentimentSunTrust <- sentimentScore(tweetsSunTrust)

tweetsBBT <- subset(fullData, grepl("@askBBT", fullData[[1]]), drop = TRUE)
write.csv(tweetsBBT, ".\\TweetsbyBank\\BBT.csv")
sentimentBBT <- sentimentScore(tweetsBBT)

tweetsHSBC_US <- subset(fullData, grepl("@HSBC_US_Help", fullData[[1]]), drop = TRUE)
write.csv(tweetsHSBC_US, ".\\TweetsbyBank\\HSBC_US.csv")
sentimentHSBC_US <- sentimentScore(tweetsHSBC_US)

tweetsHSBC_UK <- subset(fullData, grepl("@HSBC_UK_Help", fullData[[1]]), drop = TRUE)
write.csv(tweetsHSBC_UK, ".\\TweetsbyBank\\HSBC_UK.csv")
sentimentHSBC_UK <- sentimentScore(tweetsHSBC_UK)

tweetsEverBank <- subset(fullData, grepl("@EverBankHelp", fullData[[1]]), drop = TRUE)
write.csv(tweetsEverBank, ".\\TweetsbyBank\\EverBank.csv")
sentimentEverBank <- sentimentScore(tweetsEverBank)

tweetsRBC <- subset(fullData, grepl("@AskRBC", fullData[[1]]), drop = TRUE)
write.csv(tweetsRBC, ".\\TweetsbyBank\\RBC.csv")
sentimentRBC <- sentimentScore(tweetsRBC)

tweetsScotia <- subset(fullData, grepl("@ScotiabankHelps", fullData[[1]]), drop = TRUE)
write.csv(tweetsScotia, ".\\TweetsbyBank\\Scotia.csv")
sentimentScotia <- sentimentScore(tweetsScotia)

tweetsAmex <- subset(fullData, grepl("@AskAmex", fullData[[1]]), drop = TRUE)
write.csv(tweetsAmex, ".\\TweetsbyBank\\Amex.csv")
sentimentAmex <- sentimentScore(tweetsAmex)

tweetsRegions <- subset(fullData, grepl("@askRegions", fullData[[1]]), drop = TRUE)
write.csv(tweetsRegions, ".\\TweetsbyBank\\Regions.csv")
sentimentRegions <- sentimentScore(tweetsRegions)

tweetsMT <- subset(fullData, grepl("@MandT_Help", fullData[[1]]), drop = TRUE)
write.csv(tweetsMT, ".\\TweetsbyBank\\M&T.csv")
sentimentMT <- sentimentScore(tweetsMT)

tweetsKeyBank <- subset(fullData, grepl("@KeyBank_Help", fullData[[1]]), drop = TRUE)
write.csv(tweetsKeyBank, ".\\TweetsbyBank\\KeyBank.csv")
sentimentKeyBank <- sentimentScore(tweetsKeyBank)

tweetsUSAA <- subset(fullData, grepl("@USAA_help", fullData[[1]]), drop = TRUE)
write.csv(tweetsUSAA, ".\\TweetsbyBank\\USAA.csv")
sentimentUSAA <- sentimentScore(tweetsUSAA)

tweetsAlly <- subset(fullData, grepl("@AllyCare", fullData[[1]]), drop = TRUE)
write.csv(tweetsAlly, ".\\TweetsbyBank\\Ally.csv")
sentimentAlly <- sentimentScore(tweetsAlly)

tweetsSantanderUK <- subset(fullData, grepl("@santanderukhelp", fullData[[1]]), drop = TRUE)
write.csv(tweetsSantanderUK, ".\\TweetsbyBank\\SantanderUK.csv")
sentimentSantanderUK <- sentimentScore(tweetsSantanderUK)

tweetsSantanderUS <- subset(fullData, grepl("@SantanderBankUS", fullData[[1]]), drop = TRUE)
write.csv(tweetsSantanderUS, ".\\TweetsbyBank\\SantanderUS.csv")
sentimentSantanderUS <- sentimentScore(tweetsSantanderUS)

tweetsHuntington <- subset(fullData, grepl("@AskHuntington", fullData[[1]]), drop = TRUE)
write.csv(tweetsHuntington, ".\\TweetsbyBank\\Huntington.csv")
sentimentHuntington <- sentimentScore(tweetsHuntington)

tweetsSynchrony <- subset(fullData, grepl("@AskSynchrony", fullData[[1]]), drop = TRUE)
write.csv(tweetsSynchrony, ".\\TweetsbyBank\\Synchrony.csv")
sentimentSyncrhony <- sentimentScore(tweetsSynchrony)

tweetsFirstMerit <- subset(fullData, grepl("@FirstMerit_Help", fullData[[1]]), drop = TRUE)
write.csv(tweetsFirstMerit, ".\\TweetsbyBank\\FirstMerit.csv")
sentimentFirstMerit <- sentimentScore(tweetsFirstMerit)

tweetsWebster <- subset(fullData, grepl("@AskWebster", fullData[[1]]), drop = TRUE)
write.csv(tweetsWebster, ".\\TweetsbyBank\\Webster.csv")
sentimentWebster <- sentimentScore(tweetsWebster)

tweetsZions <- subset(fullData, grepl("@AskZionsBank", fullData[[1]]), drop = TRUE)
write.csv(tweetsZions, ".\\TweetsbyBank\\Zions.csv")
sentimentZions <- sentimentScore(tweetsZions)

tweetsBarclays <- subset(fullData, grepl("@BarclaysOnline", fullData[[1]]), drop = TRUE)
write.csv(tweetsBarclays, ".\\TweetsbyBank\\Barclays.csv")
sentimentBarclays <- sentimentScore(tweetsBarclays)

sentimentMatrix <- matrix(c(sentimentBofA, sentimentCiti, sentimentChase, sentimentWellsFargo, sentimentPNC, sentimentCapitalOne, sentimentSunTrust, sentimentBBT, sentimentAmex), ncol=9)
colnames(sentimentMatrix) = c("BofA", "Citi", "Chase", "WellsFargo", "PNC", "CapitalOne", "SunTrust", "BBT", "Amex")
pdf(".\\Sentiment Score Graph.pdf")
barplot(sentimentMatrix, main="Sentiment Scores", ylab="Score", xlab="Banks", cex.names=.65)
dev.off()

############ Comparison Wordcloud: Top 4 ##################
# @ChaseSupport @BofA_Help @AskCiti @Ask_WellsFargo

tweetTextBofA <- subset(fullData, grepl("@BofA_Help", fullData[[1]]), drop = TRUE)
tweetTextCiti <- subset(fullData, grepl("@AskCiti", fullData[[1]]), drop = TRUE)
tweetTextChase <- subset(fullData, grepl("@ChaseSupport", fullData[[1]]), drop = TRUE)
tweetTextWellsFargo <- subset(fullData, grepl("@Ask_WellsFargo", fullData[[1]]), drop = TRUE)
tweetsUSBank <- subset(fullData, grepl("@askusbank", fullData[[1]]), drop = TRUE)

clean.text = function(x)
{
   # tolower
   x = tolower(x)
   # remove rt
   x = gsub("rt", "", x)
   # remove at
   x = gsub("@\\w+", "", x)
   # remove punctuation
   x = gsub("[[:punct:]]", "", x)
   # remove numbers
   x = gsub("[[:digit:]]", "", x)
   # remove links http
   x = gsub("http\\w+", "", x)
   # remove tabs
   x = gsub("[ |\t]{2,}", "", x)
   # remove blank spaces at the beginning
   x = gsub("^ ", "", x)
   # remove blank spaces at the end
   x = gsub(" $", "", x)
   return(x)
}

tweetTextBofA_Clean = clean.text(tweetTextBofA)
tweetTextCiti_Clean = clean.text(tweetTextCiti)
tweetTextChase_Clean = clean.text(tweetTextChase)
tweetTextWellsFargo_Clean = clean.text(tweetTextWellsFargo)

vector_BA = paste(tweetTextBofA_Clean, collapse=" ")
vector_Ci = paste(tweetTextCiti_Clean, collapse=" ")
vector_Ch = paste(tweetTextChase_Clean, collapse=" ")
vector_WF = paste(tweetTextWellsFargo_Clean, collapse=" ")
vector_All = c(vector_BA, vector_Ci, vector_Ch, vector_WF)

# create corpus
corpusComp = Corpus(VectorSource(vector_All))
# create term-document matrix
tdmComp= TermDocumentMatrix(corpusComp)
# convert as matrix
tdmComp = as.matrix(tdmComp)
# add column names
colnames(tdmComp) = c("BofA", "Citi", "Chase", "Wells Fargo")

# comparison cloud
pdf(".\\Wordcloud_Top4.pdf")
comparison.cloud(tdmComp, random.order=FALSE,
colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
title.size=1.5, max.words=500)
dev.off()
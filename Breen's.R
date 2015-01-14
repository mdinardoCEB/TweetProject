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
pos = readLines("C:\\Users\\jlewyckyj\\Desktop\\Twitter\\positive_words.txt")
neg = readLines("C:\\Users\\jlewyckyj\\Desktop\\Twitter\\negative_words.txt")

tweets <- read.csv(file="C:\\Users\\jlewyckyj\\Desktop\\Twitter\\First 1800 Tweets.csv",head=TRUE,sep=",")

tweettext <- tweets$text
scores <- score.sentiment(tweettext, pos, neg, .progress='text')

############# Chart #####################
pdf("C:\\Users\\jlewyckyj\\Desktop\\Twitter\\BreenScores.pdf")
# plot distribution of scores
ggplot(scores, aes(x=score)) +
geom_bar(aes(y=..count.., fill=score)) +
scale_fill_brewer(palette="Dark2") +
labs(x="score", y="number of tweets",title = "Sentiment Analysis of Tweets about Banks") +
theme(plot.title = element_text(size=12)) +
scale_x_continuous(breaks=seq(-4,5,1))
# turn off #
dev.off()

### Wait on this ###
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)
global_score = round(100*numpos / (numpos+numneg))

################### Spreadsheet ###########################
write.csv(scores, "C:\\Users\\jlewyckyj\\Desktop\\Twitter\\Breen3.csv")
write.csv(scoresz, "C:\\Users\\jlewyckyj\\Desktop\\Twitter\\Breen4.csv")


tweettextc <- as.character(tweettext)
chars_per_tweet = sapply(tweettextc, nchar)
words_list = strsplit(tweettextc, " ")
words_per_tweet = sapply(words_list, length)
barplot(table(chars_per_tweet), border= NA,
   main="Distribution of characters per tweet", cex.main=1)
barplot(table(words_per_tweet), border= NA,
   main="Distribution of words per tweet", cex.main=1)

# most frequent words
mfw = sort(table(unlist(words_list)), decreasing=TRUE)
# top-20 most frequent
top20words = head(mfw, 20)
top50words = head(mfw, 50)
top100words = head(mfw, 100)
barplot(top50words, border=NA, las=2, main="Top 50 most frequent terms",
cex.main=1)




install.packages("C:\\Users\\jlewyckyj\\Downloads\\sentiment_0.1.tar.gz",lib="C:\\Users\\jlewyckyj\\Desktop\\Twitter",repos=NULL)

install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
require(sentiment)
ls("package:sentiment")






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
# remove NAs in some_txt
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

################ Spreadsheet ##########################
write.csv(sentimentresults, "C:\\Users\\jlewyckyj\\Desktop\\Twitter\\sentimentresults2.csv")

############ Charts #####################
pdf("C:\\Users\\jlewyckyj\\Desktop\\Twitter\\Emotion2.pdf")
# plot distribution of emotions
ggplot(sentimentresults, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
labs(x="emotion categories", y="number of tweets",title = "Sentiment Analysis of Tweets about Banks") +
theme(plot.title = element_text(size=12))

pdf("C:\\Users\\jlewyckyj\\Desktop\\Twitter\\Polarity2.pdf")
# plot distribution of polarity
ggplot(sentimentresults, aes(x=polarity)) +
geom_bar(aes(y=..count.., fill=polarity)) +
scale_fill_brewer(palette="Dark2") +
labs(x="polarity categories", y="number of tweets",title = "Sentiment Analysis of Tweets about Banks") +
theme(plot.title = element_text(size=12))

# Turn off #
dev.off()

############# Word Cloud ##############

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

pdf("C:\\Users\\jlewyckyj\\Desktop\\Twitter\\Wordcloud_Emotion.pdf")
# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
   scale = c(3,.5), random.order = FALSE, title.size = 1.5)






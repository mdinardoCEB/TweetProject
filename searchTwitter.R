queryToCSV <- function(query = sample(c("@ChaseSupport", "@BofA_Help", "@Ask_WellsFargo", "@AskCiti", "@askusbank", "@PNCBank_Help", "@AskCapitalOne", "@AskSunTrust", "@askBBT", "@HSBC_US_Help", "@HSBC_UK_Help", "@EverBankHelp", "@AskRBC", "@ScotiabankHelps", "@AskAmex", "@askRegions", "@MandT_Help", "@KeyBank_Help", "@USAA_help", "@AllyCare", "@santanderukhelp", "SantanderBankUS", "@AskHuntington", "@AskSynchrony", "@FirstMerit_Help", "@AskWebster", "@AskZionsBank", "@BarclaysUKHelp"))
, numberTweets, saveFile) {
tweets = list()

# Loop through the keywords and store results

  for(i in 1:length(query)){
    result<-searchTwitter(query[i],n=numberTweets, lang="en")
    tweets <- c(tweets,result)
    tweets <- unique(tweets)
  }


# Create a placeholder for the file
  file<-NULL
  fileBackup <- NULL
# Check if tweets.csv exists
  if (file.exists(saveFile)){file<- read.csv(saveFile, stringsAsFactors = FALSE)}

backupFile <- "backupTweetsDB.csv"
if (file.exists(backupFile)){fileBackup<- read.csv("backupTweetsDB.csv", 
                                                   stringsAsFactors= FALSE)}

# Merge the data in the file with our new tweets
df <- do.call("rbind", lapply(tweets, as.data.frame))
df2 <- df
df<-rbind(df,file)
df2 <- rbind(df2, fileBackup)

# Remove duplicates
  df <- df[!duplicated(df[c("id")]),]
  df2 <- df2[!duplicated(df2[c("id")]),]
# Save
  write.csv(df,file=saveFile,row.names=FALSE)
  write.csv(df2, file=backupFile,row.names = FALSE)

} 

timelineToCSV <- function(query = sample(c("@ChaseSupport", "@BofA_Help", "@Ask_WellsFargo", "@AskCiti", "@askusbank", "@PNCBank_Help", "@AskCapitalOne", "@AskSunTrust", "@askBBT", "@HSBC_US_Help", "@HSBC_UK_Help", "@EverBankHelp", "@AskRBC", "@ScotiabankHelps", "@AskAmex", "@askRegions", "@MandT_Help", "@KeyBank_Help", "@USAA_help", "@AllyCare", "@santanderukhelp", "SantanderBankUS", "@AskHuntington", "@AskSynchrony", "@FirstMerit_Help", "@AskWebster", "@AskZionsBank", "@BarclaysUKHelp")), saveFile) {
  
  tweets = list()
  
  # Loop through the keywords and store results
  print("TestPoint")
  for(i in 1:length(query)){
    
    print(i)
    result <- userTimeline(query[[i]],n=3000,sinceID=552205449744109000)
    tweets <- c(tweets,result)
    tweets <- unique(tweets) }
  
  
  # Create a placeholder for the file
  file<-NULL
  fileBackup <- NULL
  # Check if tweets.csv exists
  if (file.exists(saveFile)){file<- read.csv(saveFile, stringsAsFactors = FALSE)}
  backupFile <- "backupTimelineDB.csv"
  if (file.exists(backupFile)){fileBackup<- read.csv(backupFile, stringsAsFactors=FALSE)}
  # Merge the data in the file with our new tweets
  df <- do.call("rbind", lapply(tweets, as.data.frame))
  df2 <- df
  df<-rbind(df2,file)
  df2 <- rbind(df, fileBackup)
  # Remove duplicates
  df <- df[!duplicated(df[c("id")]),]
  df2 <- df2[!duplicated(df2[c("id")]),]
  # Save
  write.csv(df,file=saveFile,row.names=FALSE)
  write.csv(df2, file=backupFile, row.names = FALSE)
  tweets
  
} 


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
  require(plyr)
  require(stringr)
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

# A function that parses and calls the score.sentiment function on the tweets
# in the given .csv file
score.TweetDb <- function(tweetsFile, posWordFile, negWordFile) {
  posWords <- readLines(posWordFile)
  negWords <- readLines(negWordFile)
  
  tweets <- read.csv(file=tweetsFile,head=TRUE,sep=",")
  
  tweetText <- tweets$text
  tweetID <- tweets$id
  
  scores <- score.sentiment(tweetText, posWords, negWords, .progress='text')
  scores$tweetId <- tweetID
  scores
}

wordCloudTweets <- function(tweetText, stopWordsVec) {

  #mach_tweets = searchTwitter("Morgan Stanley", n = 10)

  #mach_text = sapply(mach_tweets, function(x) x$getText())

  #create a corpus
  mach_corpus = Corpus(VectorSource(tweetText))

  # create document term matrix applying some transformations"

  tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = stopWordsVec, stopwords("english"),
                                        removeNumbers = TRUE, tolower = TRUE))

  # define tdm as matrix
  m = as.matrix(tdm)
  # get word counts in decreasing order
  word_freqs = sort(rowSums(m), decreasing=TRUE) 
  # create a data frame with words and their frequencies
  dm = data.frame(word=names(word_freqs), freq=word_freqs)

  # plot wordcloud
  #pdf("C:/Users/mdinardo/Desktop/MatTwitterWordcloud2.pdf")
  wordcloud(max.words = 200, dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
  #dev.off()
}

compileTweets <- function(bankTweets = TRUE, bankList = NULL) {
      # Compiles total unique tweets, includes bank timeline tweets by default
      tweets <- read.csv("TweetsDB.csv", stringsAsFactors = FALSE)
      
      if (bankTweets == TRUE) {
            timelineTweets <- read.csv("TimelineDB.csv", stringsAsFactors = FALSE)
            tweets <- rbind(tweets, timelineTweets)
      }
      
      # Make sure no bank accounts submitted the Tweet
      else if(bankTweets == FALSE) {
            tweets <- tweets[(tweets$screenName %in% bankList) == FALSE,]
      }
      # Remove duplicates
      tweets[!duplicated(tweets),]
      # Return dataframe
      
      tweets
}





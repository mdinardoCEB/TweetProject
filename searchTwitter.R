queryToCSV <- function(query, saveFile) {

tweets = list()

# Loop through the keywords and store results

  for(i in 1:length(query)){
    result<-searchTwitter(query[i],n=5000)
    tweets <- c(tweets,result)
    tweets <- unique(tweets)
  }

# Create a placeholder for the file
  file<-NULL

# Check if tweets.csv exists
  if (file.exists(saveFile)){file<- read.csv(saveFile)}

# Merge the data in the file with our new tweets
  df <- do.call("rbind", lapply(tweets, as.data.frame))
  df<-rbind(df,file)

# Remove duplicates
  df <- df[!duplicated(df[c("id")]),]

# Save
  write.csv(df,file=saveFile,row.names=FALSE)

} 

# Done!
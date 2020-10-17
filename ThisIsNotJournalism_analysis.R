# ThisIsNotJournalism analysis 

require(tweetbotornot2)
require(glue)
require(dplyr)
require(rtweet)

# sociology TASA keys 
api_key <- "YOUR KEY HERE"
api_secret_key <- "YOUR KEY HERE"
access_token <- "YOUR KEY HERE"
access_token_secret <- "YOUR KEY HERE"

token <- create_token(
  app = "YOUR APP NAME HERE",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

######################################################
#### COLLECTION - tweets containing #thisisnotjournalism
tweet_search_thisisnotjournalism <- search_tweets('#thisisnotjournalism', n = 18000, include_rts = TRUE, retryonratelimit = TRUE)
saveRDS(tweet_search_thisisnotjournalism, paste0(Sys.time()," tweet_search_thisisnotjournalism.rds"))
# length(unique(tweet_search_thisisnotjournalism$screen_name))

# SAVE TO DISK
library(dplyr)
df_combined_thisisnotjournalism <- tweet_search_thisisnotjournalism %>% distinct(status_id, .keep_all = TRUE)
dim(df_combined_thisisnotjournalism)
# subset only the columns we want to save to disk 
df_combined_thisisnotjournalism_TO_DISK <- df_combined_thisisnotjournalism[,c(1:6,14:16,48:62,63:66,82:83)]
write.csv(df_combined_thisisnotjournalism_TO_DISK,paste0(Sys.time()," tweet_search_thisisnotjournalism.csv"),row.names = F)
# write tweet IDs to disk
write.table(df_combined_thisisnotjournalism$status_id,paste0(Sys.time(),"_thisisnotjournalism_tweet_ids.csv"), row.names = F, col.names = F, sep=",")

# USER AND BOT ANALYSIS

userids_thisisnotjournalism2 <- unique(df_combined_thisisnotjournalism$user_id)
# collect timeline data (latest 500 tweets), to feed into the bot prediction model
# we have to use a custom function to avoid a curl error with rate limits
# from here: https://github.com/ropensci/rtweet/issues/266#issuecomment-471092678 
get_timeline_unlimited <- function(users, n){
  
  if (length(users) ==0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}

# just analyse the first 1000 users, due to rate limits 
thisisnotjournalism_user_timelines2 <- get_timeline_unlimited(userids_thisisnotjournalism2[1:1000],n=200)
saveRDS(thisisnotjournalism_user_timelines2,paste0(Sys.time(),"_thisisnotjournalism_user_timelines.rds")) # save data to disk 

# run tweetbotornot2 predictions 
bot_results_thisisnotjournalism2 <- predict_bot(thisisnotjournalism_user_timelines2)
bot_results_thisisnotjournalism2$screen_name[which(bot_results_thisisnotjournalism2$prob_bot > 0.5)]
write.csv(bot_results_thisisnotjournalism2,"bot_results_thisisnotjournalism2.csv",row.names = F)

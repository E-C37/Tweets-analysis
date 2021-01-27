if (!require(rtweet)) {install.packages('rtweet')}
if (!require(magrittr)) {install.packages('magrittr')}
if (!require(data.table)) {install.packages('data.table')}
if (!require(ggplot2)) {install.packages('ggplot2')}
if (!require(graphics)) {install.packages('graphics')}
if (!require(topicmodels)) {install.packages('topicmodels')}
if (!require(quanteda)) {install.packages('quanteda')}
if (!require(stats)) {install.packages('stats')}
if (!require(grDevices)) {install.packages('grDevices')}
if (!require(utils)) {install.packages('utils')}
if (!require(methods)) {install.packages('methods')}
if (!require(utf8)) {install.packages('utf8')}
if (!require(sentimentr)) {install.packages('sentimentr')}

#Using the package rtweet we can search twitter’s api without owning an app that is authorized.
library(rtweet)

query = '"Black Friday"'
number.of.tweets=18000
my_search=function(query, number.of.tweets,max_id=NULL){
    tweets.df<- search_tweets(
        query,
        n = number.of.tweets,
        type = "recent",
        include_rts = FALSE, #No retweets, only original tweets!
        geocode = NULL,
        max_id = max_id,
        parse = TRUE,
        token = NULL,
        retryonratelimit = FALSE,
        verbose = TRUE,
        lang = "fr",
        tweet_mode = "extended" # get 240 character tweets in full
    )  
    return(tweets.df)
}


C.df=my_search(query,number.of.tweets)
for(i in 1:8){
    max_id=tail(C.df$status_id,1)
    #print("wait for 15min...")
    #Sys.sleep(900)
    #print(paste("downloading",number.of.tweets,"tweets"))
    B.df=my_search(query,number.of.tweets,max_id)
    C.df=unique(rbind(C.df,B.df))
    
}
rm(B.df)

save(C.df,file = "BF11_20.Rdata")

#2nd batch on the 24/11
query = "BlackFriday"
number.of.tweets=18000
min_id=head(C.df$status_id,1)
my_search=function(query, number.of.tweets,min_id=NULL){
    tweets.df<- search_tweets(
        query,
        n = number.of.tweets,
        type = "recent",
        include_rts = FALSE, #No retweets, only original tweets!
        geocode = NULL,
        min_id = min_id,
        parse = TRUE,
        token = NULL,
        retryonratelimit = FALSE,
        verbose = TRUE,
        lang = "fr",
        tweet_mode = "extended" # get 240 character tweets in full
    )  
    return(tweets.df)
}

C2.df=my_search(query,number.of.tweets,min_id)
C.df<-C.df[,-91]
Combined.df=unique(rbind(C2.df,C.df))

save(Combined.df,file = "BF20_24.Rdata")

#query amazon france
query = '"Amazon France"'
number.of.tweets=18000
my_search=function(query, number.of.tweets,max_id=NULL){
    tweets.df<- search_tweets(
        query,
        n = number.of.tweets,
        type = "recent",
        include_rts = FALSE, #No retweets, only original tweets!
        geocode = NULL,
        max_id = max_id,
        parse = TRUE,
        token = NULL,
        retryonratelimit = FALSE,
        verbose = TRUE,
        lang = "fr",
        tweet_mode = "extended" # get 240 character tweets in full
    )  
    return(tweets.df)
}


Amazon.df=my_search(query,number.of.tweets)
save(Amazon.df,file = "amazon_france.Rdata")
#query amazon
query = '"Amazon"'
number.of.tweets=18000
my_search=function(query, number.of.tweets,max_id=NULL){
    tweets.df<- search_tweets(
        query,
        n = number.of.tweets,
        type = "recent",
        include_rts = FALSE, #No retweets, only original tweets!
        geocode = NULL,
        max_id = max_id,
        parse = TRUE,
        token = NULL,
        retryonratelimit = FALSE,
        verbose = TRUE,
        lang = "fr",
        tweet_mode = "extended" # get 240 character tweets in full
    )  
    return(tweets.df)
}


Amazon2.df=my_search(query,number.of.tweets)
max_id=tail(Amazon2.df$status_id,1)
B.df=my_search(query,number.of.tweets,max_id)
Amazon2.df=unique(rbind(Amazon2.df,B.df))


## tweets are gathered
install.packages('proustr')
library(proustr)
install.packages('data.table')
library(data.table)
library(quanteda)
library(ggplot2)
setDT(Combined.df) 

# plot number of tweet each day
ggplot(Combined.df, aes(x=created_at)) +
    geom_histogram(aes(y=..count..), #make histogram
                   binwidth=3600*24, #each bar contains number of tweets during 60 s
                   colour="blue", #colour of frame of bars
                   fill="blue", #fill colour for bars
                   alpha=0.8) + # bars are semi transparant
    ggtitle(paste0("Activity ","17357"," tweets"," written in French"), subtitle = "tweets per day") + #title
    scale_y_continuous(name="Number of Tweets") + 
    scale_x_datetime(name = "Time") +
    theme_minimal(base_family="Times New Roman")

# which # are the most common 


sort(table(unlist(Combined.df$hashtags,use.names=FALSE)),decreasing = TRUE)[1:10]


#tokenise the tweets
library(magrittr)
tok_tweets <- Combined.df$text %>% 
    gsub("#","", . ) %>% 
    corpus %>% 
    tokens(what="word",
           remove_numbers=TRUE,
           remove_punct=TRUE,
           remove_symbols=TRUE,
           remove_separators=TRUE,
           remove_url=TRUE)
head(tok_tweets,n=2)

#stopword
stopwords(language = "fr")[1:10]
tok_tweets <- tokens_remove(tok_tweets,c(dico_vente,proust_stopwords(),stopwords(language='fr'),
                                         "BlackFriday","Friday","Black","Black Friday","c'est",
                                         "va","tout","si","j'ai","fait","fair","ça","a"))
head(tok_tweets,n=2)
#wordcount 1-gram
dico_vente<-c("faire","plus","cadeau","prix","exclusif","promo","dji","select","produits","adhésion","coupon","incroyables","gagner","tirageausort","gratuit","adhesion","coupons","gratuite","selection","inscrivez-vous","promortions")
words.to.remove <- c(dico_vente,proust_stopwords(),stopwords(language='fr'),
                     "BlackFriday","Friday","Black","Black Friday","c'est",
                     "va","tout","si","j'ai","fait","fair","ça","a")
dfm1 <- Combined.df$text %>% corpus() %>% 
    gsub("#","", . )%>%
    dfm(remove = words.to.remove,
        what = "word",
        remove_numbers=TRUE,
        stem = TRUE, 
        remove_punct = TRUE,
        remove_symbols=TRUE,
        remove_separators=TRUE,
        remove_url=TRUE)

dfFreq <- textstat_frequency(dfm1) %>% as.data.table

ggplot(dfFreq[1:30,], aes(x=reorder(feature,-rank), y=frequency)) + 
    geom_col() +
    coord_flip() +
    theme_minimal()


## split combined into 2 subset to see the diff before and after

before<-Combined.df[created_at<"2020-11-18",]

after<-Combined.df[created_at>"2020-11-18",]





#2-gram
dico_vente<-c("faire","plus","cadeau","prix","exclusif","promo","dji","select","produits","adhésion","coupon","incroyables","gagner","tirageausort","gratuit","adhesion","coupons","gratuite","selection","inscrivez-vous","promortions")


tok_tweets <- tokens_remove(tok_tweets,c(proust_stopwords(),stopwords(language='fr'),
                                         "BlackFriday","Friday","Black","Black Friday","c'est",
                                         "va","tout","si","j'ai","fait","fair","ça","a",dico_vente))
dfm2 <- dfm(tokens_ngrams(tok_tweets,n=2))
dfFreq2 <- textstat_frequency(dfm2)

ggplot(dfFreq2[1:40,], aes(x=reorder(feature, frequency), y=frequency)) + 
    geom_col() +
    coord_flip() +
    scale_x_discrete(name = "2 gram") +
    theme(text=element_text(size=12, family="Times New Roman"))





## 3-gram, but not relevant

#tok_tweets <- tokens_remove(tok_tweets,c(proust_stopwords(),stopwords(language='fr'),
  #                                       "BlackFriday","Friday","Black","Black Friday","c'est",
   #                                      "va","tout","si","j'ai","fait","fair","ça","a"))
#dfm3 <- dfm(tokens_ngrams(tok_tweets,n=3))
#dfFreq3 <- textstat_frequency(dfm3)

#ggplot(dfFreq3[1:40,], aes(x=reorder(feature, frequency), y=frequency)) + 
 #   geom_col() +
  #  coord_flip() +
   # scale_x_discrete(name = "3 gram") +
    #theme(text=element_text(size=12, family="Times New Roman"))
################################################################








##topic modelling 2-gram
dtm2 <- convert(dfm2, to = "topicmodels")
lda2 <- LDA(dtm2, k = 4, control=list(seed=12))
terms(lda2, 8) %>% utf8::utf8_print()
#The topic for the first four tweets:
     topics(lda2)[1:4]

topicAssignment2 = 
    data.table(
        index = lda2 %>% 
            topics %>% 
            names %>% 
            gsub("text","", .) 
        %>% as.integer,
        topic = lda2 %>% topics
    )
topicAssignment2 %>% head(4)

#Combined.df[,Topic2gram := NA] # creates a new col ‘topic’, assign it to NA
Combined.df$Topic2gram[topicAssignment2$index] <- topicAssignment2$topic
Combined.df$Topic2gram = Combined.df$Topic2gram %>% as.factor
ggplot(Combined.df, aes(x=created_at, y=Topic2gram, col=Topic2gram)) +
    geom_jitter(aes(size = retweet_count)) +
    ggtitle(paste0("Each dot is a tweet matching '",query,"'")) +
    scale_y_discrete() +
    scale_x_datetime(name = "") + 
    scale_color_discrete(guide = FALSE) + 
    scale_size_continuous(name="Retweets")


#Topic prone to retweets
    
    Combined.df[,list(Total.Retweets = sum(retweet_count)),by=Topic2gram] %>% 
    ggplot(aes(x = Topic2gram, y = Total.Retweets)) + 
    geom_col()



#reaction 
    Combined.df[!is.na(Topic2gram),
              list(
                  TotalTweets = .N, 
                  TotalReactions=sum(retweet_count, na.rm = TRUE) + 
                      sum(favorite_count, na.rm = TRUE)+
                      sum(reply_count, na.rm = TRUE)+
                      sum(quote_count, na.rm = TRUE),
                  Reach = sum(followers_count)/10000
              ), 
              by = Topic2gram] %>% 
        melt(id.vars = "Topic2gram") %>% 
        ggplot(aes(x = Topic2gram, y = value, fill=variable)) +
        geom_bar(position="dodge", stat="identity") + 
        scale_fill_discrete(name= "", breaks=c("TotalTweets","TotalReactions","Reach"), labels = c("Tweets","Reactions","Reach in 10,000s")) + 
        scale_y_continuous(name = "Count")

 # How much does the topic overlap?
        
    noOfTopics1gram = Combined.df$Topic1gram %>% levels %>% length
    noOfTopics2gram = Combined.df$Topic2gram %>% levels %>% length
    topics1gram = matrix(0, nrow = dim(Combined.df)[1], ncol = noOfTopics1gram)
    colnames(topics1gram) = paste("Topic",1:noOfTopics1gram)
    topics2gram = matrix(0, nrow = dim(Combined.df)[1], ncol = noOfTopics2gram)
    colnames(topics2gram) = paste("Topic",1:noOfTopics2gram)
    for (i in 1:noOfTopics1gram) {
        topics1gram[,i] = as.integer(Combined.df$Topic1gram == i)
    }
    for (i in 1:noOfTopics2gram) {   
        topics2gram[,i] = as.integer(Combined.df$Topic2gram == i)
    }
    topics1gram[is.na(topics1gram)] = 0
    topics2gram[is.na(topics2gram)] = 0
    
    diffMatrix = matrix(NA,nrow = noOfTopics1gram, ncol = noOfTopics2gram )
    for (i in 1:noOfTopics1gram) {
        for (j in 1:noOfTopics2gram) {
            diffMatrix[i,j] = 
                sum(topics1gram[,i]!=topics2gram[,j])
        }
    }
    rownames(diffMatrix) = paste("1gram Topic",1:noOfTopics1gram)
    colnames(diffMatrix) = paste("2gram Topic",1:noOfTopics2gram)
   
     diffMatrix

overlap<-arrayInd(which.min(diffMatrix),c(noOfTopics1gram,noOfTopics2gram))
rownames(diffMatrix)[overlap[1,1]]
colnames(diffMatrix)[overlap[1,2]]
paste(rownames(diffMatrix)[overlap[1,1]],"overlap",colnames(diffMatrix)[overlap[1,2]])

# question, the topics seems to be really differents and i dont know which gram is the most relevant

## Create subset of the Combined.df
#topic 1
Combined.df[Topic1gram == 1][1:10,.(text)]
ggplot(Combined.df[Topic1gram == 1], aes(x = followers_count)) + geom_histogram(binwidth = 10) + xlim(c(0,300))
ggplot(Combined.df[Topic1gram == 1], aes(x = account_created_at)) +
    geom_histogram()

dim(Combined.df[Topic1gram==1])[1]
Combined.df[Topic1gram==1, .(hashtags)] %>% 
    unlist %>% table %>% 
    sort(decreasing = T)

#topic 2
Combined.df[Topic1gram==2][1:10,.(text)]
ggplot(Combined.df[Topic1gram == 2], aes(x = followers_count)) + geom_histogram(binwidth = 10) + xlim(c(0,300))
ggplot(Combined.df[Topic1gram == 2], aes(x = account_created_at)) +
    geom_histogram()

dim(Combined.df[Topic1gram==2])[1]
Combined.df[Topic1gram==2, .(hashtags)] %>% 
    unlist %>% table %>% 
    sort(decreasing = T)

install.packages('tidytext')
library(tidytext)
tweet_topics <- tidy(lda1, matrix = "beta") %>% as.data.table

tweet_topics[order(-beta),.SD[1:3],by = topic][order(topic)]

tweet_topics[order(-beta),.SD[1:10],by = topic] %>% 
    ggplot(aes(x = reorder_within(term,beta,topic), y = beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_x_reordered() + 
    coord_flip() + 
    theme_minimal()
########################################################



########sentiment analysis
library('sentimentr')

df <- Combined.df[,.(created_at,text,Topic1gram)]
    #To change the class of the “created_at” column to POSIX clock-time:
    #Finally, this creates cuts every five minutes. Replace “5 min” with “1 hour” etc as needed!
df$roundTime <- as.POSIXct(cut(df$created_at, breaks = "24 hour"))
#The sentiment for each sentence is
df$text[1] %>% get_sentences 
df$text[1] %>% get_sentences %>% sentiment
#the sentiment score for the entire tweet
df$text[1] %>% get_sentences %>% sentiment_by
#table with the sentiment score for each tweet
sentiment_by_tweet = 
    df[,
       list(text %>% get_sentences %>% sentiment_by(),
            Topic1gram)]

#using by=Topic in the third input in data.table we can summarize the sentiment scores by topic.

sentiment_by_Topic = 
    sentiment_by_tweet[, list(Tweets = .N,
                              ave_sentiment = mean(ave_sentiment),
                              sd_sentiment = sd(ave_sentiment),
                              Total_word_count = sum(word_count)),
                       by = Topic1gram]
sentiment_by_Topic

#check if the sentiments in Topic 1 is significantly different than those in Topic 2

t.test(sentiment_by_tweet[Topic1gram ==1,ave_sentiment], sentiment_by_tweet[Topic1gram ==2,ave_sentiment])
#As the p-value is 0,5 > 5%, I say that the difference is not significant.

#The average sentiment score for the entire data set is

mean(sentiment_by_tweet$ave_sentiment)
df$polarity_score = sentiment_by_tweet$ave_sentiment
ggplot(df,aes(x=roundTime, y=polarity_score, fill=roundTime)) + 
    geom_boxplot()

#roundTime is not a factor so the boxplots will not be groped by roundTime. Let us fix this

df$roundTime <- as.factor(df$roundTime)

ggplot(df,aes(x=roundTime, y=polarity_score, fill = roundTime)) + 
    geom_boxplot() +
    guides(fill=FALSE) + 
    theme(axis.text.x = element_text(angle = 45, hjust=1))


#The 10 most frequent negative terms, regardless of topic

Combined.df[,list(text),] %>% 
    get_sentences() %>%              # get sentences
    extract_sentiment_terms(polarity_dt=my_key) %>%    # extract negative terms
    .[,negative] %>%                 # select the negative colum
    unlist %>%                       # unlist
    table  %>%                       # create freq table
    sort(decreasing = TRUE) %>% 
    head(10) %>% 
    as.data.frame.table


#get the word/sentiments from each topic

topics= unique(Combined.df$Topic1gram)
topics
topics = topics[!is.na(topics)]
topics

max_terms = 10

for (i in topics) {
    neg <- Combined.df %>% subset(Topic1gram == i) %>% 
        .[,text] %>% unlist() %>% 
        extract_sentiment_terms() %>% .[,negative] %>% unlist
    
    pos <- Combined.df %>% subset(Topic1gram == i) %>% 
        .[,text] %>% unlist() %>% 
        extract_sentiment_terms() %>% .[,positive] %>% unlist
    
    pos <- sort(table(pos), decreasing = TRUE)
    # this is the same thing if you want to use pipes:
    #pos %>% table %>% sort(decreasing = TRUE)
    
    neg <- sort(table(neg), decreasing = TRUE)
    
    print(paste("Topic",i))
    print(pos[1:min(max_terms,length(pos))])
    
    print(neg[1:min(max_terms,length(neg))])
    print("------------------------------------------------")

}

#Add or remove term from the dictionnary
head(lexicon::hash_sentiment_jockers_rinker)

#I copy this “key”

my_key = lexicon::hash_sentiment_jockers_rinker

my_key["black",]
my_key <- update_key(my_key, drop = c("black"))


###############################################
## Network of users
install.packages("vosonSML")
library(vosonSML)
class(Combined.df)
class(Combined.df) <- c(class(Combined.df),"datasource","twitter")
class(Combined.df)

## actor network - nodes are users who have tweeted
actorGraph <- Combined.df[] %>%      # tweets data table
    Create("actor") %>%             # Actor graph 
    Graph()                         # igraph network graph

source("graphHelpFunctions.R")
get_igraph_attrs(actorGraph)
V(actorGraph)$name %>% head(4)
V(actorGraph)$screen_name %>% head(4)
E(actorGraph)$edge_type %>% as.factor() %>% levels
actorGraph.simplyfied = simplify.actor.network(actorGraph, remove.loops = TRUE, delete.zero.degree = TRUE)
grep("^layout_with_.*[^[sugiyama]]*", ls("package:igraph"), value = TRUE) %>%  print
plot.actor.Graph(actorGraph.simplyfied, 
                 vertex.label = NA, 
                 layout = layout_with_kk)

top.ranked.users(actorGraph.simplyfied)[1:15]
named.users = top.ranked.users(actorGraph.simplyfied)[1:15]

#Add labels on selected

actorGraph.named = label.user.network(actorGraph.simplyfied,
                                      named.users)
plot.actor.Graph(actorGraph.named,layout = layout_with_kk)







sum(is.na(C.df$Topic)==TRUE)/length(C.df$Topic)
C.df[,.(.N, Prop=paste(round(.N/length(C.df$Topic)*100,2),"%")),by="country"]


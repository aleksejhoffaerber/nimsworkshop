# LIBS

# install the packages before loading them into the project
install.packages(c("slam", "skimr", "dplyr", "tidyr", "tibble", "stringr", "marittr", "data.table",
                   "ggplot2", "webshot", "wordcloud", "wordcloud2", "RColorBrewer",
                   "tm", "stm", "topicmodels",
                   "readr"))

# load packages into the project

library(slam)
library(skimr)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(stringr)
library(magrittr)
library(data.table)

library(ggplot2)
library(wordcloud)

library(tm)
library(stm)
library(topicmodels)


# DATA IMPORT ------ 
# read JSON stream file from reddit 

data_selfimprovement <- jsonlite::stream_in(file("submissions_selfimprovement.json"))
data_getdisciplined <- jsonlite::stream_in(file("submissions_getdisciplined.json"))
data_decidingtobebetter <- jsonlite::stream_in(file("submissions_decidingtobebetter.json"))

# data selection and initial cleaning
cleaning <- function(x) {
  x %>% 
    select(title, selftext, created_utc, subreddit, id) %>%
    mutate(created = as.Date(as.POSIXct(created_utc, origin = "1970-01-01")))
}

result <- list(data_selfimprovement, data_getdisciplined, data_decidingtobebetter) %>% 
  lapply(cleaning) # apply cleaning function to 3 subreddits



df <- rbindlist(result, fill = T) # combine sole lists into one data frame

# PREPERATION FOR CORPUSES ------ 

title_list <- df[["title"]] # there is a difference in the data type based on the subset method: 
text_list <- df[["selftext"]] # [[]] returns an atomic vector, [22] a sublist, though

# CLEANING ------ 

for (i in list(text_list, title_list)) {
  i %<>% gsub("(\t.+?,)", " ", .) %>% 
    gsub("[^A-Za-z/// ]+", " ", .) %>% # only including letters from english alphabet
    gsub("\\s+", " ",.) %>% # widespaces
    gsub("\\t+", " ",.) # too many tab-breaks 
}

corpus_title <- DocumentTermMatrix(Corpus(VectorSource(title_list)),
                             control = list(removePunctuation = T,
                                            stopwords = T, 
                                            tolower = T, 
                                            removeNumbers = T, 
                                            stemming = T, 
                                            wordLengths = c(3,25), # not too easy or complex words
                                            bounds = list(global = c(10,250)))) # should not occur more than 250 times

corpus_text <- DocumentTermMatrix(Corpus(VectorSource(text_list)),
                                   control = list(removePunctuation = T, 
                                                  stopwords = T, 
                                                  tolower = T, 
                                                  removeNumbers = T, 
                                                  stemming = T, 
                                                  wordLengths = c(3,25), 
                                                  bounds = list(global = c(200,20000)))) 



# DELETING UNNECESSARY DOCUMENTS / SUBREDDITS ------ 
# deleting threads/submissions with less than 10 words

a <- c(seq(1:nrow(corpus_text))) # storing the rows to be deleted
b <- c(rep(NA,nrow(corpus_text))) # storing amount of terms per submission

to.del <- data.frame(a,b)

for (i in 1:nrow(corpus_text)) {
  to.del[i,2] <- row_sums(corpus_text[i,]) # calculating amount of words
  if (i %% 1000 == 0) {
    print(i)
  }
}

to.del <- to.del[to.del[,2] <= 10,] # deleting entries that do not fulfill minimum term amount requirement


# ADJUSTING THE CORPUS ------ 
corpus_text <- corpus_text[row_sums(corpus_text) >= 10,] # exclude everything from the corpus with less than 10 words 

# TRAINING THE LDA MODEL ------ 
topic_text <- LDA(corpus_text,  # document term matrix
             k = 35, # specifify number of topics
             method = "Gibbs", # staying with Gibbs instead of VEM to reduce possible local minima
             control = list(
               seed = 1234, # eases replication
               burnin = 5,  # how often sampled before estimation recorded
               iter = 100,  # number of iterations
               keep = 1,    # saves additional data per iteration (such as logLik)
               save = F,     # saves logLikelihood of all iterations
               verbose = 5  # report progress
             ))

# ANALYSING LOGLIKELIHOOD CHANGE OVER TRAINING ITERATIONS
topic_text@loglikelihood             
plot(topic_text@logLiks, type = "l") 


data.frame(logLiks = topic_text@logLiks,
           Index = seq(1:105)) %>% 
  ggplot(aes(Index, logLiks)) +
  geom_line() +
  labs(x = "Iteration",
       y = "Loglikehood (the higher, the better)",
       title = "LDA performance over Iterations",
       subtitle = "Stable performance after 100 Iterations") +
  theme_minimal()

# LDA specific control measures ------ 
topic.words <- function () { 
  
  beta <- exp(topic_text@beta) # log of probability reported
  dim(beta)
  
  topic.terms <<- list() # empty list for topic words
  prob.top <<- list() # empty list for respective probabilities
  
  for (i in 1:topic_text@k) {
    
    topic.terms[[i]] <<- head(topic_text@terms[order(beta[i,], decreasing = T)], 60) # generating top words per topic
    prob.top[[i]] <<- head(sort(beta[i,], decreasing = T), 60)
    
  }
}

topic.words() 

topic_gamma <- topic_text@gamma

topic_gamma %<>%  as.data.frame() %>% # create df structure
  mutate(mak = do.call(pmax,.)) # maximizing the gamma values

colnames(topic_gamma) <- c(seq(1:topic_text@k),"mak") 




# TOPIC EXTRACTION ------ 
extracted.topics <- function(v) { # draw the topic wordclouds
  par(mfrow=c(1,1), oma = c(0,0,0,0), mai = c(0,0,0,0)) # reset plotting space 
  for (i in 1:topic_text@k) {
    
    x <- topic.terms[[i]]
    y <- prob.top[[i]]

    
    a <- wordcloud(words = x,
              freq = y,
              scale = c(4, 1),
              random.order = F, 
              rot.per = 0.5, # 90? degree rotation amount
              colors = brewer.pal(8, "Dark2")) # colour palette taking frequency into account
    
    dev.copy2pdf(file=paste0("Topics ",i,".pdf"), width = 7, height = 5)

  }
  
  par(mfrow=c(1,1), oma = c(1,1,1,1), mai = c(1,1,1,1)) # reset plotting space 
  
}
extracted.topics(topic_text)


# TOPIC SPECIFICATION ------ 
extracted_topics <- data.frame(nr = c(35:1),
                               topic = c("35: bad feelings, hate",
                                         "34: productivity and procrastination",
                                         "33: noise",
                                         "32: hope in new month/year ",
                                         "31: depression, anxiety & therapy",
                                         "30: noise",
                                         "29: studies",
                                         "28: noise",
                                         "27: videos, gaming & music",
                                         "26: emotion, mindset, control",
                                         "25: family",
                                         "24: sexual relationships (w/ girls)",
                                         "23: empowerment (power, believe, purpose)",
                                         "22: journal, book & writing (improving)",
                                         "21: emotional intelligence",
                                         "20: opinions and questions",
                                         "19: stress and mental health",
                                         "18: noise",
                                         "17: overcome fear & weaknesses",
                                         "16: goals, progress & achievements",
                                         "15: stopping bad habits (",
                                         "14: job & career",
                                         "13: habits (new, build, reward)",
                                         "12: skills, learning & improvement",
                                         "11: relationships (emotions)",
                                         "10: hardship, failure & mistakes",
                                         "09: phones, internet & media",
                                         "08: health, food & exercise",
                                         "07: noise",
                                         "06: noise",
                                         "05: happines, love & selfworth ",
                                         "04: positive choices",
                                         "03: discipline for next time",
                                         "02: morning, sleep & routine",
                                         "01: money, spending & finances"))


# Hello and welcome to our interactive function! 
# First of all, run this packages
library(twitteR)
library(tidyverse)
library(magrittr)
library(rtweet)
library(openssl)
library(httpuv)
library(tm)
library(stringr)
library(tidytext)
library(dplyr)
library(countrycode)
library(datasets)
library(textclean)

# To create the dictionary can choose betweet 2 ways:
# - manual
# - automatic

# if you want to proceed with the manual way you do not need to upload anything:
# we recoment you to use it if you do not have any existing set of word about the topic you are interested in
# or if it is the first time you create a dictionary

# On the contrary, we recomend you to use the automatic way if you want to update your existing dictionary.
# In this case, you need to upload your dataset, or a sample of it (such as the one you find on GitHub files)
# please call this file "my_data_twitter"
# Remember to clean the corpus of my_data_tweet and call the variable containing your cleaned tweets "textClean"
my_data_twitter
# If you use the automatic way, you can load hashtags and keywords concerning your topic:
# please name them "hashtags_TOPIC" and "keywords_TOPIC"
hashtags_TOPIC
keywords_TOPIC

# Now let's think about OSA list. Do you have a list of tweet talking about your topic of interest?
# If no, you can use our proposed list, in case you are interested about environment, 
# Otherwise, go on Twitter and find some official account which post about the topic of interest.
# If yes plese call it "tweet_from_OSA"
# Keep in mind! If you need to create your OSA tweet list remember that you need Twitter API!

{
  cat("Hello and welcome to the function!\nLet us give you some suggestions:\nIf you need to collect OSA tweets you can use our proposed list composed by environement accounts, otherwise you can choose how many and which profiles you want to include.\nIf you have a list of OSA ready to be used please call it tweet_from_OSA
To create the dictionary you can choose two ways: Manual or Automatic.\nIf you choose the Automatic way please be sure you uploaded your general tweet dataset named as my_data_twitter.\nIf you have a recent list of keywords related to the topic to be applied to the general list of keywords upload it now nameing it as keywords_TOPIC\nIf you want to do it for hashtags too please upload the list as hashtags_TOPIC.")
  proceedWfunct <- readline(prompt = "Are you ready to proceed? (yes/no) ")
  if(proceedWfunct == "yes" | proceedWfunct == "YES" | proceedWfunct == "Yes" ) {
  cl_dic_TW <- function(data_dic) {
    data_dic %<>% filter(is_retweet == FALSE, lang == "en") %>% 
      select(text, screen_name) %>% 
      as_tibble()
    cleanText <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", data_dic$text)
    cleanText <- gsub('[^\x01-\x7F]+', " ", cleanText)
    cleanText <- replace_html(cleanText)
    cleanText <- replace_non_ascii(cleanText, remove.nonconverted = FALSE)
    cleanText <- str_replace_all(cleanText,"@[a-z,A-Z]*"," ") 
    cleanText <- strip(cleanText, char.keep = c("#"))
    data_dic %<>% mutate(textClean = cleanText)
  }
rm(proceedWfunct)
  lookforOSA <- readline(prompt = "Do you need to create OSA list? (yes/no) ")
if(lookforOSA == "yes" | lookforOSA == "YES" | lookforOSA == "Yes" ) {
  cl_dic_TW <- function(data_dic) {
    library(textclean)
    data_dic %<>% filter(is_retweet == FALSE, lang == "en") %>% 
      select(text, screen_name) %>% 
      as_tibble()
    cleanText <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", data_dic$text)
    cleanText <- gsub('[^\x01-\x7F]+', " ", cleanText)
    cleanText <- replace_html(cleanText)
    cleanText <- replace_non_ascii(cleanText, remove.nonconverted = FALSE)
    cleanText <- str_replace_all(cleanText,"@[a-z,A-Z]*"," ") 
    cleanText <- strip(cleanText, char.keep = c("#"))
    data_dic %<>% mutate(textClean = cleanText)
  }
  askforAPI <- readline(prompt = "Do you have Twitter API? (yes/no) ")
  if(askforAPI == "yes" | askforAPI == "YES" | askforAPI == "Yes") {
    consumer_key <- readline(prompt = "Insert your consumer key: ")
    consumer_secret <- readline(prompt = "Insert your consumer secret: ")
    access_token <- readline(prompt = "Insert your consumer access token: ")
    access_secret <- readline(prompt = "Insert your access secret: ")
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  } else { 
    print("Please, get your API on Twitter")
    stop()
  } 
  n_OSA_t <- readline(prompt = "How many OSA do you want to include? ")
  vector_names <- c()
  {for(i in 1:n_OSA_t) { vector_names[i] <- readline("OSA twitter name e.g. @Greenpeace: ") }
  }
  tweet_from_OSA <- NULL
  {for(i in 1:length(vector_names)) { 
    tweet_coll = get_timeline(vector_names[i], n = 1000000000, parse = TRUE)
    tweet_from_OSA %<>% bind_rows(tweet_coll)
  }
  }
  tweet_from_OSA %<>% cl_dic_TW()
  print("YOUR DATAFRAME IS READY, LOOK AT tweet_from_OSA")
  rm(tweet_coll, access_secret, access_token, askforAPI, consumer_key, consumer_secret,
     i, n_OSA_t, vector_names)
  printOSA <- readline(prompt = "Do you want to have a look on the data frame? (yes/no) ")
  if(printOSA == "yes" | printOSA == "YES" | printOSA == "Yes") {
    print(paste("Your OSA dataset is made by", length(tweet_from_OSA$text), 
                "tweets", "here is a sample made by 20 tweets:"))
    print(sample(tweet_from_OSA$text, 20))
  }
  rm(printOSA)
} else if (lookforOSA == "no" | lookforOSA == "NO" | lookforOSA == "No") {
  useourlist <- readline(prompt = "Do you want to use our OSA list? ")
  if(useourlist == "yes" | useourlist == "YES" | useourlist == "Yes") {
    updateourlist <- readline(prompt = "Do you want to update tweets of our OSA list? ")
    if(updateourlist == "yes" | updateourlist == "YES" | updateourlist == "Yes") {
      tweet_from_OSA = NULL
      OSA_names <- c("@climateprogress", "@ClimateReality", "@friends_earth", "@Greenpeace", "@GreenpeaceUK",
                     "@LessPlasticUK", "@PlasticPollutes", "@UNEnvironment", "@UNFCCC", "@World_Wildlife", "@WWF", "@WWFScotland")
      {for(i in 1:length(OSA_names)) { 
        tweet_coll = get_timeline(OSA_names[i], n = 1000000000, parse = TRUE)
        tweet_from_OSA %<>% bind_rows(tweet_coll) %>% as_tibble()}
      }
      tweet_from_OSA %<>% cl_dic_TW()
      printOSA <- readline(prompt = "Do you want to have a look on the data frame? ")
      if(printOSA == "yes" | printOSA == "YES" | printOSA == "Yes") {
        print(paste("Your OSA dataset is made by", length(tweet_from_OSA$text), "tweets", "here is a sample made by 20 tweets:"))
        print(sample(tweet_from_OSA$text, 20))
      }
    }
    else if (updateourlist != "yes" | updateourlist != "YES" | updateourlist != "Yes") {print("Please load the file OSA_tweets.Rdata")}
  }
  else if (useourlist != "yes" | useourlist != "YES" | useourlist != "Yes") {
    print("Please, use your own list of OSA")
    stop()
    }
  rm(useourlist)
}
rm(lookforOSA)
create_dict <- readline(prompt = "Do you need to create the dictionary? (yes/no) ")
if(create_dict == "no" | create_dict == "NO" | create_dict == "No" ) {
  print("Enjoy your dataset ")
} else if (create_dict == "yes" | create_dict == "YES" | create_dict == "Yes" ) {
  auto_manual <- readline(prompt = "Do you want to build it manually or automatically? (Choose: Manual/Automatic) ")
  if (auto_manual == "Manual") {
    cat("Steps you must follow:\n
        1. Visualize bigram list, select the threeshold, remove not pertaining words\n
        2. Visualize trigram list, select the threeshold, remove not pertaining words\n 
        3. Visualize hashtags list, select the threeshold, remove not pertaining words\n
        Finally you get your dictionary")
    data("stop_words")
    most_used_bigrams <- tweet_from_OSA %>% unnest_tokens(bigram, textClean, token = "ngrams", n = 2) %>% 
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(word1 != "NA", word2 != "NA") %>% 
      unite(keywords, c("word1", "word2"), sep = " ") %>%
      count(keywords, sort = TRUE)
    print_bigrams <- readline(prompt = "How many bigrams do you want to print? ")
    print_bigrams <- as.numeric(print_bigrams)
    print(most_used_bigrams, n=print_bigrams)
    rm(print_bigrams)
    bi_thre <- readline(prompt = "Which threeshold do you choose for bigrams? ")
    bi_thre <- as.numeric(bi_thre)
    most_used_bigrams %<>% filter(n >= bi_thre) %>% select(keywords)
    print(most_used_bigrams, n=10000000) 
    rm(bi_thre)
    remove_word <- readline(prompt = "Do you want to remove any word? (yes/no) ")
    if(remove_word == "yes" | remove_word == "YES" | remove_word == "Yes" ) {
      print("If you had done please write EXIT ")
      word_to_remove = NULL
      repeat{
        word_to_remove <- readline(prompt = "Insert the keyword you want to remove: ")
        most_used_bigrams %<>% filter(keywords != word_to_remove)
        if(word_to_remove == "EXIT") {
          break
        }
      }
      rm(word_to_remove)
    }
    rm(remove_word)
    most_used_trigrams <- tweet_from_OSA %>% unnest_tokens(trigram, textClean, token = "ngrams", n = 3) %>% 
      separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(!word3 %in% stop_words$word) %>%
      filter(word1 != "NA", word2 != "NA", word3 != "NA") %>%
      unite(keywords, c("word1", "word2", "word3"), sep = " ") %>%
      count(keywords, sort = TRUE)
    most_used_trigrams %<>% mutate(isin = (str_detect(most_used_trigrams$keywords, paste(most_used_bigrams$keywords, collapse = '|')))) %>%
      filter(isin == FALSE) %>% 
      select(-isin)
    print_trigrams <- readline(prompt = "How many trigrams do you want to print? ")
    print_trigrams <- as.numeric(print_trigrams)
    print(most_used_trigrams, n=print_trigrams)
    rm(print_trigrams)
    tri_thre <- readline(prompt = "Which threeshold do you choose for trigrams? ")
    tri_thre <- as.numeric(tri_thre)
    most_used_trigrams %<>% filter(n >= tri_thre) %>% select(keywords)
    print(most_used_trigrams, n=10000000) 
    rm(tri_thre)
    remove_word <- readline(prompt = "Do you want to remove any word? (yes/no) ")
    if(remove_word == "yes" | remove_word == "YES" | remove_word == "Yes" ) {
      print("If you had done please write EXIT ")
      word_to_remove = NULL
      repeat{
        word_to_remove <- readline(prompt = "Insert the keyword you want to remove: ")
        most_used_trigrams %<>% filter(keywords != word_to_remove)
        if(word_to_remove == "EXIT") {
          break
        }
      }
      rm(word_to_remove)
    }
    rm(remove_word)
    include_hashtags <- readline(prompt = "Do you want to include hashtags in your dictionary? (yes/no) ")
    if(include_hashtags == "yes" | include_hashtags == "YES" | include_hashtags == "Yes" ) {
      hashtag_used_dim <- (str_extract_all(paste(tweet_from_OSA$textClean, collapse = ' '), "#\\S+"))[[1]]
      hashtag_used_uniq_dim <- unique(hashtag_used_dim)
      most_used_hashtags <- hashtag_used_dim %>% as_tibble() %>% 
        mutate(hashtag = as.character(value)) %>% select(-value) %>% 
        group_by(hashtag) %>% summarise(n=n()) %>% 
        arrange(order(match(hashtag_used_uniq_dim, hashtag))) %>% arrange(desc(n)) 
      rm(hashtag_used_dim, hashtag_used_uniq_dim)
      print_hashtags <- readline(prompt = "How many hashtags do you want to print? ")
      print_hashtags <- as.numeric(print_hashtags)
      print(most_used_hashtags, n=print_hashtags)
      rm(print_hashtags)
      has_top <- readline(prompt = "How many top trending hashtags to you want to inclued? ")
      has_top <- as.numeric(has_top)
      most_used_hashtags %<>% top_n(has_top) %>% select(hashtag)
      print(most_used_hashtags, n=1000)
      rm(has_top)
      remove_hash <- readline(prompt = "Do you want to remove any hashtags? (yes/no) ")
      if(remove_hash == "yes" | remove_hash == "YES" | remove_hash == "Yes" ) {
        print("If you had done please write EXIT ")
        hash_to_remove = NULL
        repeat{
          hash_to_remove <- readline(prompt = "Insert the hashtag you want to remove: ")
          most_used_hashtags %<>% filter(hashtag != hash_to_remove)
          if(hash_to_remove == "EXIT") {
            break
          }
        }
        rm(hash_to_remove)
      }
      rm(remove_hash)
      most_used_hashtags %<>% rename(keywords = "hashtag")
      dictionary <- bind_rows(most_used_bigrams, most_used_trigrams, most_used_hashtags)
      rm(most_used_bigrams, most_used_trigrams, most_used_hashtags)
      name <- readline(prompt = "How do you want to call your dictionary? ")
      assign(name[1], dictionary)
      print(paste("Your dictionary is ready, look at: ", name[1]))
      rm(name)
    } else if (include_hashtags == "no" | include_hashtags == "NO" | include_hashtags == "No" ){
      dictionary = bind_rows(most_used_bigrams, most_used_trigrams)
      rm(most_used_bigrams, most_used_trigrams)
      name <- readline(prompt = "How do you want to call your dictionary? ")
      assign(name[1], dictionary)
      print(paste("Your dictionary is ready, look at: ", name[1]))
      rm(name)
    }
    rm(include_hashtags)
  }
  else if (auto_manual == "Automatic") {
    cat("Steps you must follow:\n
        1. Create the general list of keyword.\n
        2. Visualize bigram list, select the threeshold, remove not pertaining words\n
        3. Visualize trigram list, select the threeshold, remove not pertaining words\n 
        4. Visualize hashtags list, select the threeshold, remove not pertaining words\n
        Finally you get your dictionary")
    rm(auto_manual)
    data_uploaded <- readline(prompt = "Did you upload your tweets as my_data_twitter? (yes/no) ")
    if (data_uploaded == "no" | data_uploaded == "NO" | data_uploaded == "No"){
      print("Please upload your data and run the function")
    }
    else if(data_uploaded == "yes" | data_uploaded == "YES" | data_uploaded == "Yes") {
      use_sample <- readline(prompt = "Do you you want to use a sample from your data to create the general keywords list? (yes/no) ")
      if (use_sample == "yes" | use_sample == "YES" | use_sample == "Yes"){
        sample_size <- readline(prompt = "How big do you want your sample to be? ")
        sample_size <- as.numeric(sample_size)
        my_data_twitter = sample_n(my_data_twitter, size = sample_size)
        rm(sample_size)
        }
      else if (use_sample == "no" | use_sample == "NO" | use_sample == "No"){
        my_data_twitter = my_data_twitter
      }
      rm(use_sample)
      have_env_key <- readline(prompt = "Do you have a list of keywords related to environment named keywords_TOPIC? (yes/no) ")
      if (have_env_key == "no" | have_env_key == "NO" | have_env_key == "No") {
        print("Since you don't have any list, do not choose a too low threshold ")
        keywords_TOPIC = c("  ")
      }
      else if (have_env_key == "yes" | have_env_key == "YES" | have_env_key == "Yes") {
        keywords_TOPIC = keywords_TOPIC
      }
      rm(have_env_key)
      data("stop_words")
      most_used_bigrams_tot <- my_data_twitter %>% unnest_tokens(bigram, textClean, token = "ngrams", n = 2) %>% 
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        filter(word1 != "NA", word2 != "NA") %>% 
        unite(keywords, c("word1", "word2"), sep = " ") %>%
        count(keywords, sort = TRUE)
      most_used_bigrams_tot %<>% 
        mutate(isin = (str_detect(most_used_bigrams_tot$keywords, paste(keywords_TOPIC, collapse = '|')))) %>% 
        filter(isin == FALSE) %>% select(-isin)
      print_bigrams <- readline(prompt = "How many general bigrams do you want to print? ")
      print_bigrams <- as.numeric(print_bigrams)
      print(most_used_bigrams_tot, n=print_bigrams)
      rm(print_bigrams)
      bi_thre <- readline(prompt = "Which threeshold do you choose for general bigrams? ")
      bi_thre <- as.numeric(bi_thre)
      most_used_bigrams_tot %<>% filter(n >= bi_thre)
      rm(bi_thre)
      print(most_used_bigrams_tot, n=10000000) 
      remove_word <- readline(prompt = "Do you want to remove any word related to the topic? (yes/no) ")
      if(remove_word == "yes" | remove_word == "YES" | remove_word == "Yes" ) {
        print("If you had done please write EXIT ")
        word_to_remove = NULL
        repeat{
          word_to_remove <- readline(prompt = "Insert the keyword you want to remove: ")
          most_used_bigrams_tot %<>% filter(keywords != word_to_remove)
          if(word_to_remove == "EXIT") {
            break
          }
        }
        rm(word_to_remove)
      }
      rm(remove_word)
      most_used_trigrams_tot <- my_data_twitter %>% unnest_tokens(trigram, textClean, token = "ngrams", n = 3) %>% 
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        filter(!word3 %in% stop_words$word) %>%
        filter(word1 != "NA", word2 != "NA", word3 != "NA") %>%
        unite(keywords, c("word1", "word2", "word3"), sep = " ") %>%
        count(keywords, sort = TRUE)
      most_used_trigrams_tot %<>% 
        mutate(isin = (str_detect(most_used_trigrams_tot$keywords, paste(keywords_TOPIC, collapse = '|')))) %>% 
        filter(isin == FALSE) %>% select(-isin)
      print_trigrams <- readline(prompt = "How many general trigrams do you want to print? ")
      print_trigrams <- as.numeric(print_trigrams)
      print(most_used_trigrams_tot, n=print_trigrams)
      rm(print_trigrams)
      tri_thre <- readline(prompt = "Which threeshold do you choose for general trigrams? ")
      tri_thre <- as.numeric(tri_thre)
      most_used_trigrams_tot %<>% filter(n >= tri_thre)
      rm(tri_thre)
      print(most_used_trigrams_tot, n=10000000) 
      remove_word <- readline(prompt = "Do you want to remove any keyword related to the topic? (yes/no) ")
      if(remove_word == "yes" | remove_word == "YES" | remove_word == "Yes" ) {
        print("If you had done please write EXIT ")
        word_to_remove = NULL
        repeat{
          word_to_remove <- readline(prompt = "Insert the keyword you want to remove: ")
          most_used_trigrams_tot %<>% filter(keywords != word_to_remove)
          if(word_to_remove == "EXIT") {
            break
          }
        }
        rm(word_to_remove)
      }
      rm(remove_word)
      keyword_tot <- bind_rows(most_used_bigrams_tot, most_used_trigrams_tot) %>% 
        arrange(desc(n)) %>% select(keywords)
      cat("Here is your list of general keywords to apply as filter:")
      print(keyword_tot)
      rm(most_used_trigrams_tot, most_used_bigrams_tot)
    }
    rm(data_uploaded)
    country_names <- tolower(codelist$country.name.en)
    state_names <- tolower(state.name)
    bigrams_dim <- tweet_from_OSA %>% unnest_tokens(bigrams_dim, textClean, token = "ngrams", n = 2) %>% 
      separate(bigrams_dim, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(word1 != "NA", word2 != "NA") %>% 
      unite(keywords, c("word1", "word2"), sep = " ") %>% 
      count(keywords, sort = TRUE)
    bigrams_filt <- bigrams_dim$keywords[!bigrams_dim$keywords %in% keyword_tot$keywords] %>% 
      as_tibble() %>% rename(keywords = value)
    bigrams_filt <- bigrams_filt$keywords[!bigrams_filt$keywords %in% country_names] %>% 
      as_tibble() %>% rename(keywords = value)
    bigrams_filt <- bigrams_filt$keywords[!bigrams_filt$keywords %in% state_names] %>% 
      as_tibble() %>% rename(keywords = value)
    bigrams_dim <- inner_join(bigrams_dim, bigrams_filt)
    rm(bigrams_filt)
    trigrams_dim <- tweet_from_OSA %>% unnest_tokens(trigram, textClean, token = "ngrams", n = 3) %>% 
      separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(!word3 %in% stop_words$word) %>%
      filter(word1 != "NA", word2 != "NA", word3 != "NA") %>%
      unite(keywords, c("word1", "word2", "word3"), sep = " ") %>%
      count(keywords, sort = TRUE)
    trigrams_dim %<>% mutate(isin = (str_detect(trigrams_dim$keywords, paste(bigrams_dim$keywords, collapse = '|'))),
                             isin_tot = (str_detect(trigrams_dim$keywords, paste(keyword_tot$keywords, collapse = '|'))),
                             isin_cou = (str_detect(trigrams_dim$keywords, paste(country_names, collapse = '|'))),
                             isin_sta = (str_detect(trigrams_dim$keywords, paste(state_names, collapse = '|')))) %>%
      filter(isin_tot == FALSE, isin == FALSE, isin_cou == FALSE, isin_sta == FALSE) %>% 
      select(-isin, -isin_tot, -isin_cou, -isin_sta)
    rm(keyword_tot)
    print_bigrams <- readline(prompt = "How many bigrams do you want to print? ")
    print_bigrams <- as.numeric(print_bigrams)
    print(bigrams_dim, n=print_bigrams)
    rm(print_bigrams)
    bi_thre <- readline(prompt = "Which threeshold do you choose for bigrams? ")
    bi_thre <- as.numeric(bi_thre)
    bigrams_dim %<>% filter(n >= bi_thre)
    rm(bi_thre)
    print(bigrams_dim, n=10000000) 
    remove_word <- readline(prompt = "Do you want to remove any keyword? (yes/no) ")
    if(remove_word == "yes" | remove_word == "YES" | remove_word == "Yes" ) {
      print("If you had done please write EXIT ")
      word_to_remove = NULL
      repeat{
        word_to_remove <- readline(prompt = "Insert the keyword you want to remove: ")
        bigrams_dim %<>% filter(keywords != word_to_remove)
        if(word_to_remove == "EXIT") {
          break
        }
      }
      rm(word_to_remove)
    }
    rm(remove_word)
    print_trigrams <- readline(prompt = "How many trigrams do you want to print? ")
    print_trigrams <- as.numeric(print_trigrams)
    print(trigrams_dim, n=print_trigrams)
    rm(print_trigrams)
    tri_thre <- readline(prompt = "Which threeshold do you choose for trigrams? ")
    tri_thre <- as.numeric(tri_thre)
    trigrams_dim %<>% filter(n >= tri_thre)
    rm(tri_thre)
    print(trigrams_dim, n=10000000) 
    remove_word <- readline(prompt = "Do you want to remove any trigram keyword? (yes/no) ")
    if(remove_word == "yes" | remove_word == "YES" | remove_word == "Yes" ) {
      print("If you had done please write EXIT ")
      word_to_remove = NULL
      repeat{
        word_to_remove <- readline(prompt = "Insert the keyword you want to remove: ")
        trigrams_dim %<>% filter(keywords != word_to_remove)
        if(word_to_remove == "EXIT") {
          break
        }
      }
      rm(word_to_remove)
    }
    rm(remove_word)
    keyword <- bind_rows(bigrams_dim, trigrams_dim)
    print("Here is your keyword list: ")
    print(keyword, n=100000)
    rm(keyword)
    include_hashtags <- readline(prompt = "Do you want to include hashtags in your dictionary? (yes/no) ")
    if(include_hashtags == "no" | include_hashtags == "NO" | include_hashtags == "No" ){
      dictionary = bind_rows(bigrams_dim, trigrams_dim) %>% select(keywords)
      name <- readline(prompt = "How do you want to call your dictionary? ")
      assign(name[1], dictionary)
      print(paste("Your dictionary is ready, look at: ", name[1]))
      rm(bigrams_dim, trigrams_dim, name)
    }
    else if(include_hashtags == "yes" | include_hashtags == "YES" | include_hashtags == "Yes" ) {
      hashtag_tot <- ((str_extract_all(paste(my_data_twitter$textClean, 
                                             collapse = ' '), "#\\S+"))[[1]]) %>% 
        as_tibble()
      have_env_has <- readline(prompt = "Do you have a list of hashtags related to environment? (yes/no) ")
      if (have_env_has == "no" | have_env_has == "NO" | have_env_has == "No") {
        print("Since you don't have any list, do not choose a too low threshold ")
        hashtags_TOPIC = c("  ") 
      }
      else if (have_env_has == "yes" | have_env_has == "YES" | have_env_has == "Yes") {
        hashtags_TOPIC = hashtags_TOPIC
      }
      rm(have_env_has)
      hashtag_mostused_tot <- hashtag_tot %>% as_tibble() %>% group_by(value) %>% 
        summarise(n=n()) %>% arrange(desc(n)) %>%
        rename(hashtag_tot = value)
      hashtag_mostused_tot_filt <- hashtag_mostused_tot$hashtag_tot[!hashtag_mostused_tot$hashtag_tot %in% hashtags_TOPIC] %>% 
        as_tibble() %>% rename(hashtag_tot = value)
      rm(hashtag_tot)
      hashtag_mostused_tot %<>% inner_join(hashtag_mostused_tot_filt)
      rm(hashtag_mostused_tot_filt)
      print_hash <- readline(prompt = "How many general hashtags do you want to print? ")
      print_hash <- as.numeric(print_hash)
      print(hashtag_mostused_tot, n=print_hash)
      rm(print_hash)
      has_thre <- readline(prompt = "Which threeshold do you choose for general hashtags? ")
      has_thre <- as.numeric(has_thre)
      hashtag_mostused_tot %<>% filter(n >= has_thre)
      rm(has_thre)
      print(hashtag_mostused_tot, n=10000000) 
      remove_word <- readline(prompt = "Do you want to remove any hashtag related to the topic? (yes/no) ")
      if(remove_word == "yes" | remove_word == "YES" | remove_word == "Yes" ) {
        print("If you had done please write EXIT ")
        word_to_remove = NULL
        repeat{
          word_to_remove <- readline(prompt = "Insert the keyword you want to remove: ")
          hashtag_mostused_tot %<>% filter(keywords != word_to_remove)
          if(word_to_remove == "EXIT") {
            break
          }
        }
        rm(word_to_remove)
      }
      rm(remove_word)
      country_names_hash <- paste0("#", country_names)
      state_names_hash <- paste0("#", state_names)
      rm(country_names, state_names)
      hashtag_used_dim <- (str_extract_all(paste(tweet_from_OSA$textClean, collapse = ' '), "#\\S+"))[[1]]
      hashtag_used_uniq_dim <- unique(hashtag_used_dim)
      hashtag_most_used_dim_notclean <- hashtag_used_dim %>% as_tibble() %>% 
        mutate(hashtag = as.character(value)) %>% select(-value) %>% 
        group_by(hashtag) %>% summarise(n=n()) %>% 
        arrange(order(match(hashtag_used_uniq_dim, hashtag)))
      rm(hashtag_used_uniq_dim, hashtag_used_dim)
      hashtag_most_used_dim <- hashtag_most_used_dim_notclean$hashtag[!hashtag_most_used_dim_notclean$hashtag %in% 
                                                                        hashtag_mostused_tot] %>% 
        as_tibble() %>% rename(hashtag = value)
      hashtag_most_used_dim <- hashtag_most_used_dim$hashtag[!hashtag_most_used_dim$hashtag %in% state_names_hash] %>% 
        as_tibble() %>% rename(hashtag = value)
      hashtag_most_used_dim <- hashtag_most_used_dim$hashtag[!hashtag_most_used_dim$hashtag %in% country_names_hash] %>% 
        as_tibble() %>% rename(hashtag = value)
      hashtag_most_used_dim %<>% inner_join(hashtag_most_used_dim_notclean, by="hashtag") %>% arrange(desc(n))
      rm(hashtag_most_used_dim_notclean, hashtag_mostused_tot, state_names_hash, country_names_hash)
      print_hashtags <- readline(prompt = "How many hashtags do you want to print? ")
      print_hashtags <- as.numeric(print_hashtags)
      print(hashtag_most_used_dim, n=print_hashtags)
      rm(print_hashtags)
      has_top <- readline(prompt = "How many top trending hashtags to you want to inclued? ")
      has_top <- as.numeric(has_top)
      hashtag_most_used_dim %<>% top_n(has_top) %>% select(hashtag)
      rm(has_top)
      print(hashtag_most_used_dim, n=1000)
      remove_hash <- readline(prompt = "Do you want to remove any hashtags? (yes/no) ")
      if(remove_hash == "yes" | remove_hash == "YES" | remove_hash == "Yes" ) {
        print("If you had done please write EXIT ")
        hash_to_remove = NULL
        repeat{
          hash_to_remove <- readline(prompt = "Insert the hashtag you want to remove: ")
          hashtag_most_used_dim %<>% filter(hashtag != hash_to_remove)
          if(hash_to_remove == "EXIT") {
            break
          }
        }
        rm(hash_to_remove)
      }
      rm(remove_hash)
      keywords <-  bind_rows(bigrams_dim, trigrams_dim) %>% select(keywords)
      rm(bigrams_dim, trigrams_dim)
      hashtag_most_used_dim %<>% select(hashtag) %>% rename(keywords = hashtag)
      dictionary <- bind_rows(keywords, hashtag_most_used_dim)
      rm(keywords, hashtag_most_used_dim)
      name <- readline(prompt = "How do you want to call your dictionary? ")
      assign(name[1], dictionary)
      print(paste("Your dictionary is ready, look at: ", name[1]))
      rm(name)
    } 
    rm(include_hashtags)
  }
} 
rm(create_dict, stop_words)
  }
  else {
    stop()
  }
}


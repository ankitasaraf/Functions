library(tidytext)
library(tidyverse)
library(stringr)


#-------------------------------------------------------
#Funcion to reticulate python sentence tokenizer 
#-------------------------------------------------------

py.sent_tokenize = function(text) {
  
  require(reticulate)
  require(dplyr)
  
  nltk = import("nltk")
  
  sent_list = vector(mode="list", length=length(text))  
  counter = 0
  
  for (i in 1:length(text)){  
    sents = nltk$tokenize$sent_tokenize(text[i])
    
    sent_list[[i]] = data.frame(docID = i, 
                                sentID = counter + seq(1:length(sents)), 
                                text = sents, 
                                stringsAsFactors=FALSE)
    
    counter = max(sent_list[[i]]$sentID)   }    # end of for loop
  
  sent_df = bind_rows(sent_list)   
  return(sent_df)  }   # end of function




#--------------------------------------------------------
#Function for basic text cleaning
#--------------------------------------------------------

# 
# clean_text <- function(text, lower=FALSE, alphanum=FALSE, drop_num=FALSE){
#   text  =  str_replace_all(text, "<.*?>", " ")   # drop html tags
#   
#   if (lower=="TRUE") 
#   {
#     text = text %>% str_to_lower()
#   }
#   if (alphanum=="TRUE") 
#   {
#     text = text %>% str_replace_all("[^[:alnum:]]", " ")
#   }
#   if (drop_num=="TRUE") 
#   {
#     text = text %>% str_replace_all("[:digit:]", "")
#   }
#   
#   # collapse multiple spaces to a single space
#   text = text %>%   
#     str_replace_all("\\\\s+", " ")  
#   
#   return(text) } # end of function
# 
# 
# 

#-------------------------------------------------------
#Function for basic text cleaning
#-------------------------------------------------------

text_clean = function(x,                    # x=text_corpus
                      remove_numbers=TRUE,        # whether to drop numbers? Default is TRUE  
                      remove_stopwords=TRUE)      # whether to drop stopwords? Default is TRUE
  
{ 
  library(tm)
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  
  if (remove_numbers) 
  { 
    x  =  removeNumbers(x)
  }    # removing numbers
  
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space. Note regex usage
  
  # evlauate condition
  if (remove_stopwords){
    
    # read standard stopwords list from my git
    stpw1 = readLines('https://raw.githubusercontent.com/ankitasaraf/Text-Analytics/master/stopwords.txt')
    
    # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
    stpw2 = tm::stopwords('english')      
    comn  = unique(c(stpw1, stpw2))         # Union of the two lists
    stopwords = unique(gsub("'"," ",comn))  # final stop word list after removing punctuation
    
    # removing stopwords created above
    x  =  removeWords(x,stopwords)           }  # if condn ends
  
  x  =  stripWhitespace(x)                  # removing white space
  # x  =  stemDocument(x)                   # can stem doc if needed. For Later.
  
  return(x)

}  # end of function



#------------------------------------------------------
#Create DTM
#------------------------------------------------------

dtm_build <- function(raw_corpus, tfidf=FALSE)
{                  # func opens
  
  require(tidytext)
  require(tibble)
  require(tidyverse)
  
  # converting raw corpus to tibble to tidy DF
  textdf = data_frame(text = raw_corpus);    textdf  
  
  tidy_df = textdf %>%   
    mutate(doc = row_number()) %>%
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    group_by(doc) %>%
    count(word, sort=TRUE)
  tidy_df
  
  # evaluating IDF wala DTM
  if (tfidf == "TRUE") {
    textdf1 = tidy_df %>% 
      group_by(doc) %>% 
      count(word, sort=TRUE) %>% ungroup() %>%
      bind_tf_idf(word, doc, nn) %>%   # 'nn' is default colm name
      rename(value = tf_idf)} else { textdf1 = tidy_df %>% rename(value = n)  } 
  
  textdf1
  
  dtm = textdf1 %>% cast_sparse(doc, word, value);    dtm[1:9, 1:9]
  
  # order rows and colms putting max mass on the top-left corner of the DTM
  
  colsum = apply(dtm, 2, sum)    
  col.order = order(colsum, decreasing=TRUE)
  row.order = order(rownames(dtm) %>% as.numeric())
  
  dtm1 = dtm[row.order, col.order];    dtm1[1:8,1:8]
  
  return(dtm1)  }   # end of function


#------------------------------------
#Create DTM
#------------------------------------

create_DTM = function(text,
                      docID = NULL,
                      replace_ngrm = T,
                      #rm_stop_words = T,
                      textcleaning = T,
                      lower = T,
                      alphanum = T,
                      drop_num = T,
                      #stop_custom = c('will','was', 'can'),
                      #smart_stop_words = T,
                      tfidf = F,
                      bi_gram_pct = 0.05,
                      min_freq = 5,
                      filter = 'pct',
                      py.sent_tknzr = T)
{
  if(is.null(docID))
  {
    docID = 1:length(text)
  }
  if(replace_ngrm==T)
  {
    replaced_text = replace_ngram(text=text,
                                  bi_gram_pct = bi_gram_pct,
                                  min_freq = min_freq,
                                  filter = filter,
                                  py.sent_tknzr = py.sent_tknzr,
                                  textcleaning = textcleaning,
                                  lower = lower,
                                  alphanum = alphanum,
                                  drop_num = drop_num)
                                  #rm_stop_words = rm_stop_words,
                                  #stop_custom = stop_custom,
                                  #smart_stop_words = smart_stop_words)
    replaced_text$docID = docID
  }
  else{
    if(textcleaning==T)
    {
      text = text_clean(text)
    }
    replaced_text = data_frame(text = text, docID = docID)
  }
  
  
  #tokenizing the corpus
  
  textdf1 = replaced_text %>%
            group_by(docID)%>%
            unnest_tokens(word,text)%>%
            count(word, sort = TRUE) %>% ungroup()
  
  #cast into matrix object
  
  if (tfidf == "TRUE")
  {
    textdf2 = textdf1 %>% group_by(docID) %>%
              count(word, sort = TRUE) %>% ungroup() %>% 
              bind_tf_idf(word, docID, nn) %>%
              rename(value = tf_idf)
  }
  else
  {
    textdf2 = textdf1 %>% rename(value=n)
  }
  
  m <- textdf2 %>% cast_sparse(docID, word, value)
  
  #reorder DTM to have sorted rows by doc_num and cols by colsums
  
  b0 = apply(m, 2, sum) %>% order(decreasing = TRUE)
  dtm = m[,b0]
  return (dtm)  #end of function
}

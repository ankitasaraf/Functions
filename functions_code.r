library(tidyverse)
library(tidytext)
library(stringr)

#--------------------------------------------------------------
# 1 - Reticulate python sentence tokenizer with python NLTK
#--------------------------------------------------------------

py.sent_tokenize = function(text) {
  
  require(reticulate)
  require(dplyr)
  nltk = import("nltk")
  
  sent_list = vector(mode="list", length=length(text))  
  counter = 0
  
  for (i in 1:length(text)){  
    if (!grepl('[:alnum:]',text[i])) {text[i] = "NO sentence"}
    sents = nltk$tokenize$sent_tokenize(text[i])
    sent_list[[i]] = data.frame(docID = i, 
                                sentID = counter + seq(1:length(sents)), 
                                text = sents, 
                                stringsAsFactors=FALSE)
    
    counter = max(sent_list[[i]]$sentID)   }    # i ends
  
  sent_df = bind_rows(sent_list)   
  return(sent_df)  }   # py.sent_tokenize() ends


#---------------------------------------------------
# 2 - Basic text cleaning
#---------------------------------------------------

clean_text <- function(text, lower=T, alphanum=T, drop_num=T){
  text  =  str_replace_all(text, "<.*?>", " ")   # drop html junk
  
  if (lower=="TRUE") {text = text %>% str_to_lower()}
  if (alphanum=="TRUE") {text = text %>% str_replace_all("[^[:alnum:]]", " ")}
  if (drop_num=="TRUE") {text = text %>% str_replace_all("[:digit:]", "")}
  
  text = text %>% str_replace_all("\\\\s+", " ")   # collapse multiple spaces
  return(text) 
  
} # clean_text() ends

#--------------------------------------
# 3 - Removing Stop Words
#--------------------------------------

remove_words =  function (x, words) {
  gsub(sprintf("(*UCP)\\b(%s)\\b", paste(sort(words, decreasing = TRUE), 
                                         collapse = "|")), "", x, perl = TRUE) }


#--------------------------------------
# 4 - Replacing bi grams
#--------------------------------------  
replace_ngram <- function(text, 
                          bi_gram_pct = 0.05,
                          min_freq = 5, 
                          filter = 'pct', 
                          py.sent_tknzr = T, 
                          textcleaning = T,
                          lower=T,
                          alphanum=T, 
                          drop_num=T,
                          rm_stop_words=T,
                          stop_custom = c('will','was','can'),
                          smart_stop_words = T
){
  
  text_orig = text
  # Cleaning text
  if( textcleaning == T) {
    text = clean_text(text,lower = lower, alphanum = alphanum, drop_num = drop_num)
  }
  
  # defining smart stop words
  smart_stop = c("i","me","my","myself","we","our","ours","ourselves","you","your","yours","yourself","yourselves","he","him","his","himself","she","her","hers","herself",
                 "it","its","itself","they","them","their","theirs","themselves","what","which","who","whom","this","that","these","those","am","is","are","was","were","be",
                 "been","being","have","has","had","having","do","does","did","doing","would","should","could","ought","i'm","you're","he's","she's","it's","we're","they're",
                 "i've","you've","we've","they've","i'd","you'd","he'd","she'd","we'd","they'd","i'll","you'll","he'll","she'll","we'll","they'll","isn't","aren't","wasn't",
                 "weren't","hasn't","haven't","hadn't","doesn't","don't","didn't","won't","wouldn't","shan't","shouldn't","can't","cannot","couldn't","mustn't","let's","that's",
                 "who's","what's","here's","there's","when's","where's","why's","how's","a","an","the","and","but","if","or","because","as","until","while","of","at","by","for",
                 "with","about","against","between","into","through","during","before","after","above","below","to","from","up","down","in","out","on","off","over","under",
                 "again","further","then","once","here","there","when","where","why","how","all","any","both","each","few","more","most","other","some","such","no","nor","not",
                 "only","own","same","so","than","too","very","a's","able","according","accordingly","across","actually","afterwards","ain't","allow","allows","almost","alone",
                 "along","already","also","although","always","among","amongst","another","anybody","anyhow","anyone","anything","anyway","anyways","anywhere","apart","appear",
                 "appreciate","appropriate","around","aside","ask","asking","associated","available","away","awfully","b","became","become","becomes","becoming","beforehand",
                 "behind","believe","beside","besides","best","better","beyond","brief","c","c'mon","c's","came","can","cant","cause","causes","certain","certainly","changes",
                 "clearly","co","com","come","comes","concerning","consequently","consider","considering","contain","containing","contains","corresponding","course","currently",
                 "d","definitely","described","despite","different","done","downwards","e","edu","eg","eight","either","else","elsewhere","enough","entirely","especially","et",
                 "etc","even","ever","every","everybody","everyone","everything","everywhere","ex","exactly","example","except","f","far","fifth","first","five","followed",
                 "following","follows","former","formerly","forth","four","furthermore","g","get","gets","getting","given","gives","go","goes","going","gone","got","gotten",
                 "greetings","h","happens","hardly","hello","help","hence","hereafter","hereby","herein","hereupon","hi","hither","hopefully","howbeit","however","ie",
                 "ignored","immediate","inasmuch","inc","indeed","indicate","indicated","indicates","inner","insofar","instead","inward","it'd","it'll","j","just","k",
                 "keep","keeps","kept","know","knows","known","l","last","lately","later","latter","latterly","least","less","lest","let","like","liked","likely",
                 "little","look","looking","looks","ltd","m","mainly","many","may","maybe","mean","meanwhile","merely","might","moreover","mostly","much","must",
                 "n","name","namely","nd","near","nearly","necessary","need","needs","neither","never","nevertheless","new","next","nine","nobody","non","none",
                 "noone","normally","nothing","novel","now","nowhere","o","obviously","often","oh","ok","okay","old","one","ones","onto","others","otherwise",
                 "outside","overall","p","particular","particularly","per","perhaps","placed","please","plus","possible","presumably","probably","provides",
                 "q","que","quite","qv","r","rather","rd","re","really","reasonably","regarding","regardless","regards","relatively","respectively","right",
                 "s","said","saw","say","saying","says","second","secondly","see","seeing","seem","seemed","seeming","seems","seen","self","selves","sensible",
                 "sent","serious","seriously","seven","several","shall","since","six","somebody","somehow","someone","something","sometime","sometimes","somewhat",
                 "somewhere","soon","sorry","specified","specify","specifying","still","sub","sup","sure","t","t's","take","taken","tell","tends","th","thank","thanks",
                 "thanx","thats","thence","thereafter","thereby","therefore","therein","theres","thereupon","think","third","thorough","thoroughly","though","three","throughout",
                 "thru","thus","together","took","toward","towards","tried","tries","truly","try","trying","twice","two","u","un","unfortunately","unless","unlikely","unto","upon",
                 "us","use","used","useful","uses","using","usually","uucp","v","value","various","via","viz","vs","w","want","wants","way","welcome","well","went","whatever","whence",
                 "whenever","whereafter","whereas","whereby","wherein","whereupon","wherever","whether","whither","whoever","whole","whose","will","willing","wish","within","without",
                 "wonder","x","y","yes","yet","z")
  
  # removing stopwords
  
  if (rm_stop_words == T) {
    if(smart_stop_words == T){
      stop = unique(tolower(c(stop_custom,smart_stop)))
    } else {
      stop = unique(tolower(stop_custom))
    }
    text = remove_words(text,stop)
  }
  
  
  textdf = data_frame(text = text, docID = 1:length(text))
  
  # Build All possible bigram list
  bigram_df_orig = textdf %>% group_by(docID) %>% unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    unite(bigram1, word1, word2, sep = " ", remove = FALSE) %>%
    ungroup()
  
  if (py.sent_tknzr == T) {
    # Split corpus into sentences to get valid bigrams
    sentdf = py.sent_tokenize(text_orig)
    if( textcleaning == T) {
      sentdf$text = clean_text(sentdf$text,lower = lower, alphanum = alphanum, drop_num = drop_num)
    }
    
    if (rm_stop_words == T) {
      if(smart_stop_words == T){
        stop = unique(tolower(c(stop_custom,smart_stop)))
      } else {
        stop = unique(tolower(stop_custom))
      }
      sentdf$text = remove_words(sentdf$text,stop)
    }
    
    
  } else {
    sentdf = data_frame(text = text, docID = 1:length(text), sentID = 1:length(text))
  }
  
  # get the valid bi-grams for replacement
  bigram_df <- sentdf %>% unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, word2, sort = T) %>%
    filter(n >= 2) %>%     # valid bi-grams
    unite(bigram1, word1, word2, sep = " ", remove = FALSE) %>%
    unite(bigram2, word1, word2, sep = "_")
  
  # if (filter = 'pct') {
  bigram_df1 = bigram_df[1:round(nrow(bigram_df)*bi_gram_pct),]  
  # } else {
  bigram_df2 = bigram_df %>% filter(n >= min_freq)
  # }
  
  # Print bi-gram count
  print(paste0("Total first ",bi_gram_pct*100, "% bi-grams identified is ", (nrow(bigram_df1))))
  print(paste0(nrow(bigram_df2)," bi-grams identified with minimum frequency ", (min_freq)))
  
  if (filter == 'pct') {
    bigram_df = bigram_df1 }
  else {
    bigram_df = bigram_df2
  }
  
  # merge the two bi-grams
  merged_df1 <- left_join(bigram_df_orig, bigram_df, by=c("bigram1"="bigram1"))
  
  # first, replace NAs with word1
  a0 = (is.na(merged_df1$bigram2))
  merged_df1$bigram2[a0] = merged_df1$word1[a0]
  
  # and NAs count with n = 1
  merged_df1$n[a0] = 1
  
  # create vectors for each column of merged df
  docID = merged_df1$docID
  bigram1 = merged_df1$bigram1
  bigram2 = merged_df1$bigram2
  nvec = merged_df1$n
  word1 = merged_df1$word1
  word2 = merged_df1$word2
  
  # system.time({
  i = 1
  while(i < (nrow(merged_df1))){             
    
    if (docID[i] == docID[i+1]){
      
      if (grepl("_",bigram2[i]) & grepl("_",bigram2[i+1]) ) {
        if (nvec[i] >= nvec[i+1]){ 
          bigram2[i+1] = ""
        } else { bigram2[i] = word1[i]} 
      }
      
      if (grepl("_",bigram2[i])){
        bigram2[i+1] = ""
      }    
      
    } else if (!(docID[i] == docID[i+1]) ) {
      if (grepl("_",bigram2[i-1])) {
        bigram2[i] = word2[i]
      } else if (!grepl("_",bigram2[i])) {
        bigram2[i] = bigram1[i]
      }
    }
    i = i + 1  
  }
  if (!grepl("_",bigram2[i])) {bigram2[i] = bigram1[i]}
  
  # })
  
  merged_df1$bigram2 = bigram2
  
  # Roll-back the tokens into a doc string
  cleaned_corpus = data.frame(docID = numeric(), text = character(), stringsAsFactors=FALSE) # define empty df to populate
  tokens_only = merged_df1 %>% select(docID, bigram2)
  
  for (i1 in 1:nrow(textdf)){
    a100 = tokens_only[tokens_only$docID == i1,]   # for each doc, collect all cleaned tokens
    
    if (nrow(a100) == 0){
      cleaned_corpus[i1, 1] =  i1
      cleaned_corpus[i1, 2] = "NA_BLANK"
    } else {
      cleaned_corpus[i1, 1] =  i1
      cleaned_corpus[i1, 2] = str_c(a100$bigram2, collapse=" ")   # using str_c()
    }
    
    if (i1 %% 100 == 0) {cat(i1, " docs processed\n")}   # counter for docs processed. Run '?%%'
  } # i1 loop ends
  
  return(cleaned_corpus)   }    # func ends  

#--------------------------------
# 5 - Create DTM
#--------------------------------

create_DTM = function(text, 
                      docID = NULL,
                      replace_ngrm = T,
                      rm_stop_words=T,
                      textcleaning = T,
                      lower=T,
                      alphanum=T, 
                      drop_num=T,
                      stop_custom = c('will','was','can'),
                      smart_stop_words = T,
                      tfidf = F,
                      bi_gram_pct = 0.05,
                      min_freq = 5, 
                      filter = 'pct', 
                      py.sent_tknzr = T){
  
  if (is.null(docID)) {docID = 1:length(text)}
  if (replace_ngrm == T) {
    replaced_text = replace_ngram(text = text, 
                                  bi_gram_pct = bi_gram_pct,
                                  min_freq = min_freq, 
                                  filter = filter, 
                                  py.sent_tknzr = py.sent_tknzr, 
                                  textcleaning = textcleaning,
                                  lower=lower,
                                  alphanum=alphanum, 
                                  drop_num=drop_num,
                                  rm_stop_words=rm_stop_words,
                                  stop_custom = stop_custom,
                                  smart_stop_words = smart_stop_words)
    replaced_text$docID = docID
  } else {
    if( textcleaning == T) {
      text = clean_text(text,lower = lower, alphanum = alphanum, drop_num = drop_num)
    }
    
    if (rm_stop_words == T) {
      if(smart_stop_words == T){
        stop = unique(tolower(c(stop_custom,smart_stop)))
      } else {
        stop = unique(tolower(stop_custom))
      }
      text = remove_words(text,stop)
    }
    
    replaced_text = data_frame(text = text, docID = docID)
  }
  
  
  ## tokenizing the corpus
  textdf1 = replaced_text %>% 
    # mutate(docIDs = row_number()) %>%    # row_number() is v useful.
    group_by(docID) %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) %>% ungroup()
  
  ## cast into a Matrix object
  if (tfidf == "TRUE") {
    textdf2 = textdf1 %>% group_by(docID) %>% 
      count(word, sort=TRUE) %>% ungroup() %>%
      bind_tf_idf(word, docID, nn) %>% 
      rename(value = tf_idf)} else { textdf2 = textdf1 %>% rename(value = n)  }
  
  m <- textdf2 %>% cast_sparse(docID, word, value)
  
  # reorder dtm to have sorted rows by doc_num and cols by colsums	
  # m = m[order(as.numeric(rownames(m))),]    # reorder rows	
  b0 = apply(m, 2, sum) %>% order(decreasing = TRUE)
  dtm = m[, b0]
  
  return(dtm)
  
}

#---------------------------------------------------
# 6 - Build a wordcloud
#---------------------------------------------------

build_wordcloud <- function(dtm, 
                            max.words1=150,     # max no. of words to accommodate
                            min.freq=5,       # min.freq of words to consider
                            plot.title="wordcloud"){          # write within double quotes
  
  require(wordcloud)
  if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
    
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
      print(i)      } # i loop ends
    
    tsum = ss.col
    
  } else { tsum = apply(dtm, 2, sum) }
  
  tsum = tsum[order(tsum, decreasing = T)]       # terms in decreasing order of freq
  head(tsum);    tail(tsum)
  
  # windows()  # Opens a new plot window when active
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(3.5, 0.5),     # range of word sizes
            min.freq,                     # min.freq of words to consider
            max.words = max.words1,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = plot.title)     # title for the wordcloud display
  
} # func ends


#-----------------------------------
# 7 - Plot a barchart
#-----------------------------------


plot.barchart <- function(dtm, num_tokens=15, fill_color="Blue")
{
  a0 = apply(dtm, 2, sum)
  a1 = order(a0, decreasing = TRUE)
  tsum = a0[a1]
  
  # plot barchart for top tokens
  test = as.data.frame(round(tsum[1:num_tokens],0))
  
  # windows()  # New plot window
  require(ggplot2)
  p = ggplot(test, aes(x = rownames(test), y = test)) + 
    geom_bar(stat = "identity", fill = fill_color) +
    geom_text(aes(label = test), vjust= -0.20) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x= "New words", y = "Frequency", title = "New words frequency chart")
  
  plot(p) }  # func ends


#----------------------------------
# 8 - COG graph
#----------------------------------

distill.cog = function(dtm, # input dtm
                       title="COG", # title for the graph
                       central.nodes=4,    # no. of central nodes
                       max.connexns = 5){  # max no. of connections  
  
  # first convert dtm to an adjacency matrix
  dtm1 = as.matrix(dtm)   # need it as a regular matrix for matrix ops like %*% to apply
  adj.mat = t(dtm1) %*% dtm1    # making a square symmatric term-term matrix 
  diag(adj.mat) = 0     # no self-references. So diag is 0.
  a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
  mat1 = as.matrix(adj.mat[a0[1:50], a0[1:50]])
  
  # now invoke network plotting lib igraph
  library(igraph)
  
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:central.nodes){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[max.connexns]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc, word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  
  # building and plotting a network object
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:central.nodes] = "green"
  V(graph)$color[(central.nodes+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
  
} # distill.cog func ends

#-----------------------------------------
# 9 - Lemmatizing
#-----------------------------------------

lemmatize.corpus <- function(df)
{
  library(udpipe)
  library(stringr)
  english_model = udpipe_load_model('C:/Users/saraf/Desktop/CBA/Term 1/Text Analytics/Assignments/Group Assignment Data files-20180503/english-ud-2.0-170801.udpipe')
  
  dim(df)
  text = df$bd.text
  fname = sapply(strsplit(df$file,"_"),'[[',1)
  x <- udpipe_annotate(english_model, x = text)
  x <- as.data.frame(x)
  head(x)
  x = x[,c('doc_id', 'lemma')]
  
  lemma_corpus = data.frame(docID = character(), text = character(), stringsAsFactors = FALSE)
  
  for (k in 1:nrow(df))
  {
    a100 = x[x$doc_id == paste0('doc', k),]   #collect all cleaned tokens for each document
    if(nrow(a100)==0)
    {
      lemma_corpus[k,1] = k
      lemma_corpus[k,2] = 'NA_BLANK'
    }
    else
    {
      lemma_corpus[k,1] = k
      lemma_corpus[k,2] = str_c(a100$lemma, collapse = " ")
    }
    if (k %% 100 ==0)
    {
      cat(k," documents processed\n")
    }
  }
  return(lemma_corpus)
} #end of function

text.clean = function(x,                    # x=text_corpus
                      remove_numbers=TRUE,        # whether to drop numbers? Default is TRUE  
                      remove_stopwords=TRUE)      # whether to drop stopwords? Default is TRUE
  
{ 
  library(tm)
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  
  if (remove_numbers) { x  =  removeNumbers(x)}    # removing numbers
  
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space. Note regex usage
  
  # evlauate condn
  if (remove_stopwords){
    
    # read std stopwords list from my git
    stpw1 = readLines('https://raw.githubusercontent.com/ankitasaraf/Text-Analytics/master/stopwords.txt')
    
    # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
    stpw2 = tm::stopwords('english')      
    comn  = unique(c(stpw1, stpw2))         # Union of the two lists
    stopwords = unique(gsub("'"," ",comn))  # final stop word list after removing punctuation
    
    # removing stopwords created above
    x  =  removeWords(x,stopwords)           }  # if condn ends
  
  x  =  stripWhitespace(x)                  # removing white space
  # x  =  stemDocument(x)                   # can stem doc if needed. For Later.
  
  return(x) }  # func ends

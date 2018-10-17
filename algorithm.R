# algorithm.R

library(stringr)
library(data.table)
library(openNLP)
library(NLP)

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

unigram_dt <- readRDS("unigram_dt.rds")
bigram_dt <- readRDS("bigram_dt.rds")
trigram_dt <- readRDS("trigram_dt.rds")
badwords <- readRDS("badwords.rds")

clean_text <- function(text) {
  input_str <- tolower(text)    
  input_str <- str_replace_all(input_str, "([iu]n)-([a-z])", "\\1\\2")
  input_str <- str_replace_all(input_str, "([0-9])(st|nd|rd|th)", "\\1")
  input_str <- str_replace_all(input_str, "[^a-z.' ]", " ")
  input_str <- str_replace_all(input_str, "www\\.[a-z]+\\.[a-z]+", "")
  input_str <- str_replace_all(input_str, "\\.", " ")
  input_str <- str_replace_all(input_str, " ([a-z])\\1+ |^([a-z])\\1+ | ([a-z])\\1+$|^([a-z])\\1+$", " ")
  input_str <- str_replace_all(input_str, "([a-z])\\1{2,}", "\\1\\1")
  input_str <- str_replace_all(input_str, "\\'+([a-z]+)\\'+", "\\1")
  input_str <- str_replace_all(input_str, "\\'+ \\'+", " ")
  input_str <- str_replace_all(input_str, "(\\'+ )+|( \\'+)+|^\\'+|\\'+$", " ")
  input_str <- str_replace_all(input_str, "^[a-z]+$", "")
  input_str <- str_replace_all(input_str, "( [^ai])+ |^([^ai] )+|( [^ai])+$", " ")
  input_str <- str_replace_all(input_str, "^ +| +$|", "")
  input_str <- str_replace_all(input_str, " {2,}", " ")
  input_str <- str_replace_all(input_str, " +$|^ +", "")
  return(input_str)
}

filter_text <- function(text) {
  tmp <- text
  if (length(tmp) > 0) {
    words <- parse_text(tmp)
    num_words <- length(words)
    if (num_words > 0) {
      for (i in 1:num_words) {
        if (words[i] %in% badwords) words[i] <- paste(substring(words[i], 1, 1), "***", sep = "")
      }
      tmp_w <- paste(words[1]) 
      if (num_words > 1) {
        for (i in 2:num_words) tmp_w <- paste(tmp_w, words[i])
      }
      return(tmp_w)
    }
  }
  return(tmp)
}

get_default <- function(text) {
  if (length(text) > 0) {
    a2 <- annotate(as.String(text), list(sent_token_annotator, word_token_annotator))
    a3 <- annotate(as.String(text), pos_tag_annotator, a2)
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, `[[`, "POS")
    if (tags %like% "NN") {
      return("in")
    } else if (tags %like% "VB") {
      return("a")
    } else if (tags %like% "JJ") {
      return("time")
    } else if (tags %like% "PRP") {
      return("first")
    } else if (tags %like% "CC") {
      return("i")
    } else if (text == "the") {
      return("first")
    }
  }
  return("the")
}

parse_text <- function(text) {
  tmp <- unlist(str_split(text, " "))
  tmp <- tmp[tmp != ""]
  return(tmp)
}

get_word <- function(text) {
  if (text != " ") { 
    words <- parse_text(tolower(text))
    num_words <- length(words)
    if (num_words > 0) {
      filter <- paste("^", words[num_words], sep = "")
      tmp_dt <- unigram_dt[n0 %like% filter]
      pred_word <- dim(tmp_dt)[1]
      if (pred_word > 0) {
        tmp_dt <- tmp_dt[order(rank(-freq))]
        pred <- tmp_dt[1]$n0
        if (num_words > 2) {
          tmp_w <- paste(words[1])
          for (i in 2:(num_words - 1)) tmp_w <- paste(tmp_w, words[i])
          return(paste(tmp_w, filter_text(pred)))
        } else if (num_words > 1) {
          tmp_w <- paste(words[1])
          return(paste(tmp_w, filter_text(pred)))
        }
      }
    }
  }
  return(text)
}

get_pred <- function(text) {
  if (text != " ") { 
    input_words <- parse_text(clean_text(text))
    len <- length(input_words)
    
    if (len > 1) {
      w1 <- input_words[len]
      w2 <- input_words[len - 1]
    } else if (len > 0) {
      w1 <- input_words[len]
      w2 <- "NA"
    } else return("the")
    
    l1 <- .95
    l2 <- .04
    l3 <- .01
    
    len3 <- length(trigram_dt[trigram_dt[n2 == w2 & n1 == w1]]$freq)
    len2 <- length(bigram_dt[bigram_dt[n1 == w1]]$freq)
    matches <- matrix(nrow = len3 + len2, ncol = 2)
    matches[,1] <- ""
    matches[,2] <- 0
    
    if (len3 > 0) {
      for (i in 1:len3) {
        matches[i, 1] <- trigram_dt[trigram_dt[n2 == w2 & n1 == w1]]$n0[i]
        cnt2 <- length(bigram_dt[bigram_dt[n1 == w1 & n0 == matches[i, 1]]]$freq)
        cnt1 <- length(unigram_dt[unigram_dt[n0 == matches[i, 1]]]$freq)
        if (cnt2 > 0) freq2 <- bigram_dt[bigram_dt[n1 == w1 & 
                                                     n0 == matches[i, 1]]]$freq else freq2 <- 0
        if (cnt1 > 0) freq1 <- unigram_dt[unigram_dt[n0 == matches[i, 1]]]$freq else freq1 <- 0
        matches[i, 2] <- trigram_dt[trigram_dt[n2 == w2 & n1 == w1]]$freq[i] * 
          l1 + freq2 * l2 + freq1 * l3     
      }
    }
    if (len2 > 0) {
      for (i in sum(len3, 1):sum(len3, len2)) {
        matches[i, 1] <- bigram_dt[bigram_dt[n1 == w1]]$n0[i - len3]
        cnt1 <- length(unigram_dt[unigram_dt[n0 == matches[i, 1]]]$freq)
        if (cnt1 > 0) freq1 <- unigram_dt[unigram_dt[n0 == matches[i, 1]]]$freq else freq1 <- 0
        matches[i, 2] <- bigram_dt[bigram_dt[n1 == w1]]$freq[i - len3] * l2 + freq1 * l3   
      }
    }
    match_len <- length(matches[which.max(matches[,2])])
    if (match_len > 0) return(matches[which.max(matches[,2])])
    return(get_default(w1))
  }
  return(" ")
}
#' @title Scoring a vector of text based on words
#' @param words A list of words you want to search for/score. Use * to use prefixes or suffixes.
#' @param textfile The text file you want to search in, e.g. textfile.
#' @return A vector of length(text) with scores for each word.
#' @examples negative_words <- c("overwhelming", "*ar", "trapped", "frustrating", "a", "co*", "*ate")
#' @examples positive_words <- c("shake", "cooperate", "woke", "diary", "bed", "i", "sle*")
#' @examples scoring_words(negative_words)
#' @export
#'
#'

scoring_words <- function (words, text = textfile) {
  tmp <- matrix(nrow=length(text), ncol=length(words)) #Creating empty matrix
  tmp <- data.frame(tmp)
  colnames(tmp) <- words
  for (i in words) {
    if (substr(i, 1, 1)=="*") { #Looking for words with prefixes
      tmp[,i] <- endsWith(text, gsub("\\*","",i))
    }
    else if (substr(i, nchar(i), nchar(i))=="*"){ #Looking for words with suffixes
      tmp[,i] <- startsWith(text, gsub("\\*","",i))
    }
    else {
      tmp[,i] <- i == text #Looking for exact words
    }
  }
  final <- rowSums(tmp)
  final <- ifelse(final > 1, 1, final) #added this statement to deal with instances where a word is counted twice i.e. if * used for finding suffixes/prefixes
}


##I would change the examples of positive/negative words to not include broad prefixes or suffixes as with this example provided,
# the combination of "co*", "*ate" meant that "cooperate" was incorrectly identified twice, rather than once, as a negative word


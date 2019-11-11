library(wordcloud)
library(tidyverse)

# get words that relate to faculty expertise
words <- read.table(file = "data/faculty_expertise_words.txt", header = FALSE, sep = "\n")
words <- as.character(as.matrix(words))
words <- tolower(words)
words <- unlist(sapply(words, function(y) strsplit(y, " ")[[1]]))

# now draw wordcloud
# dev.new(width = 1000, height = 1000)
wordcloud(words, 
          min.freq = 0, fixed.asp = TRUE, 
          random.order = TRUE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"), random.color = TRUE, 
          scale = c(2, 0.5))

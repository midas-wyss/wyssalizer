# WORDCLOUD ON TITLES

# all titles
all_titles <- filtered_papers %>% 
  dplyr::filter(., year >= 2009) %>%
  dplyr::select(., wyss_scholar, journal, title)

all_words <- all_titles %>%
  tidytext::unnest_tokens(word, title) %>%
  dplyr::anti_join(tidytext::get_stopwords())

wc_words <- all_words %>%
  dplyr::mutate(., word = gsub("^ .", "", word)) %>%
  dplyr::filter(., !grepl("^[0-9]", word)) %>%
  dplyr::filter(., !grepl("^[a-z]$", word)) %>%
  dplyr::filter(., !grepl("^ii$", word)) %>%
  dplyr::filter(., word != "using") %>%
  dplyr::filter(., word != "cells") %>%
  dplyr::count(word) # <--- this generates the data frame for the word cloud function


pdf(file = paste0("res/img/", format(Sys.Date(), "%Y-%m-%d"), "_wordcloud-all_titles-top250words.pdf"), bg = NULL)
pal <- viridis(n = 12)
wordcloud(words = wc_words$word,
          freq = wc_words$n, 
          random.order = FALSE, 
          max.words = 250, 
          rot.per = 0.25, 
          colors = pal, 
          scale = c(2.5, .5))
dev.off()


# google scholar only
scholar_titles <- filtered_papers %>% 
  dplyr::filter(., year >= 2009, search_source == "google scholar") %>%
  dplyr::select(., wyss_scholar, journal, title)

scholar_words <- scholar_titles %>%
  tidytext::unnest_tokens(word, title) %>%
  dplyr::anti_join(tidytext::get_stopwords())

gs_wc_words <- scholar_words %>%
  dplyr::mutate(., word = gsub("^ .", "", word)) %>%
  dplyr::filter(., !grepl("^[0-9]", word)) %>%
  dplyr::filter(., !grepl("^[a-z]$", word)) %>%
  dplyr::filter(., !grepl("^ii$", word)) %>%
  dplyr::filter(., word != "using") %>%
  dplyr::filter(., word != "cells") %>%
  dplyr::count(word) # <--- this generates the data frame for the word cloud function


pdf(file = paste0("res/img/", format(Sys.Date(), "%Y-%m-%d"), "_wordcloud-gscholar_titles-top250words.pdf"), bg = NULL)
pal <- viridis(n = 12)
wordcloud(words = gs_wc_words$word,
          freq = gs_wc_words$n, 
          random.order = FALSE, 
          max.words = 250, 
          rot.per = 0.25, 
          colors = pal, 
          scale = c(2.5, .5))
dev.off()

# pubmed only
pubmed_titles <- filtered_papers %>% 
  dplyr::filter(., year >= 2009, search_source == "pubmed") %>%
  dplyr::select(., wyss_scholar, journal, title)

pubmed_words <- pubmed_titles %>%
  tidytext::unnest_tokens(word, title) %>%
  dplyr::anti_join(tidytext::get_stopwords())

pm_wc_words <- pubmed_words %>%
  dplyr::mutate(., word = gsub("^ .", "", word)) %>%
  dplyr::filter(., !grepl("^[0-9]", word)) %>%
  dplyr::filter(., !grepl("^[a-z]$", word)) %>%
  dplyr::filter(., !grepl("^ii$", word)) %>%
  dplyr::filter(., word != "using") %>%
  dplyr::filter(., word != "cells") %>%
  dplyr::count(word) # <--- this generates the data frame for the word cloud function


pdf(file = paste0("res/img/", format(Sys.Date(), "%Y-%m-%d"), "_wordcloud-pubmed_titles-top250words.pdf"), bg = NULL)
pal <- viridis(n = 12)
wordcloud(words = pm_wc_words$word,
          freq = pm_wc_words$n, 
          random.order = FALSE, 
          max.words = 250, 
          rot.per = 0.25, 
          colors = pal, 
          scale = c(2.5, .5))
dev.off()
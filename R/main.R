# WYSS WORDCLOUD

# load library to query pubmed
library(RISmed)
library(tidyverse)
library(wordcloud)
library(tidytext)
library(ggraph)
library(gganimate)
# library(ggwordcloud)
library(readxl)
library(scholar) # <-- make sure this is the one on github, not on CRAN
library(igraph)
library(plotly)
library(ggwordcloud) # <-- from lepennec github repo
library(RColorBrewer)
library(viridis)
library(wordcloud2)

######################
# FACULTY DATAFRAME
# source("R/faculty_dataframes.R")
load("data/2019-11-10_wyss_scholar_dataframe.RData")

######################
# GOOGLE SCHOLAR NETWORK
# source("R/gs_network.R")





######################
# OLD -- DON'T RUN THIS ----
######################
# PUBMED 
# faculty papers
# all_papers <- vector(mode = "list", length = nrow(wyss_scholar))
# for (i in seq(1, nrow(wyss_faculty))) {
#   x1 <- wyss_scholar[i, ]
#   y1 <- extract_papers(search_term = x1$pubmed_search)
#   y2 <- data_extractor(papers = y1, faculty = x1, start_year = 2007)
#   all_papers[[i]] <- y2
# }
# all_papers <- dplyr::bind_rows(all_papers)


######################
# DATA FROM MARIEL
# load data
# pubs_data <- readxl::read_xlsx(path = "data/Wyss_Publications_2009-2019_DMC.xlsx")

# clean up data
# aa <- gsub("\\.$", "", pubs_data$title)
# pubs_data <- pubs_data %>% 
#   dplyr::mutate(title = aa) %>%
#   dplyr::mutate(title = tolower(title))
# 
# 
# gsub(pattern = "\\\"", replacement = "", x = pubs_data$title[2])

# separate faculty
# sep_data <- vector(mode = "list", length = nrow(pubs_data))

# for (i in seq(1, nrow(pubs_data))) {
#   a1 <- pubs_data[i, ]
#   # a2 <- strsplit(x = a1$faculty, split = "; ")[[1]]
#   a3 <- gsub("^and ", "", strsplit(a1$authors, ", |  | and ")[[1]])
#   a3 <- setdiff(a3, "")
#   a4 <- character(length = length(a3))
#   for (j in seq(1, length(a3))) {
#     a4[j] <- tail(strsplit(a3[j], " ")[[1]], n = 1)
#   }
#   
#   
#   sep_data[[i]] <- tibble::tibble(authors = a4,
#                                   title = a1$title,
#                                   journal = a1$journal,
#                                   date = a1$publication_date,
#                                   doi = a1$DOI)
#   
# }
# sep_data <- dplyr::bind_rows(sep_data)

######################
# GET FACULTY NAMES
# faculty_names <- vector(mode = "list", length = nrow(pubs_data))
# for (i in seq(1, nrow(pubs_data))) {
#   faculty_names[[i]] <- strsplit(x = pubs_data$faculty[i], split = "; ")[[1]]
# }
# faculty_names <- unlist(faculty_names)



######################
# NETWORK            
# doi matrix
# N <- matrix(0, nrow = length(unique(sep_data$authors)), ncol = length(unique(sep_data$doi)))



######################
# WORD CLOUD         #

# all_papers %>% 
#   dplyr::filter(., abstract != "") %>%
#   # dplyr::filter(., faculty == "collins") %>%
#   unnest_tokens(., word, abstract) %>% 
#   anti_join(stop_words) %>% 
#   count(word) %>% 
#   dplyr::filter(., !str_detect(word, "^\\d")) %>%
#   with(wordcloud(word, n,
#                  min.freq = 25,
#                  # scale = c(3, 0.4),
#                  # max.words = 2500,
#                  # random.order = TRUE,
#                  random.color = TRUE,
#                  random.order = TRUE,
#                  rot.per = 0.35,
#                  colors = brewer.pal(8, "Dark2")))
# 
# 
# p <- paper_collection$papers %>% 
#   # dplyr::filter(., faculty == "lewis j") %>% 
#   dplyr::count(., journal) %>% 
#   dplyr::arrange(., desc(n)) %>% 
#   dplyr::slice(1:25) %>%
#   ggplot() + 
#   geom_col(aes(x = forcats::fct_reorder(journal, n), y = n), alpha = 0.8) + 
#   labs(x = "Journal", y = "Number of papers") + 
#   coord_flip() +
#   # facet_wrap(faculty ~ n, scales = "free") +
#   theme_bw() +
#   theme(axis.text = element_text(size = 12, color = "black"),
#         axis.title = element_text(size = 24, color = "black"))

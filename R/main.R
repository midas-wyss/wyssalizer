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
library(scholar) # <-- make sure this is the one on github, not on CRAN (devtools::install_github("jkeirstead/scholar"))
library(igraph)
library(plotly)
library(ggwordcloud) # <-- from lepennec github repo (devtools::install_github("lepennec/ggwordcloud"))
library(RColorBrewer)
library(viridis)
library(wordcloud2)

######################
# FACULTY DATAFRAME
# source("R/faculty_dataframes.R")
load("data/2019-11-10_wyss_scholar_dataframe.RData")

######
# CO-AUTHORSHIP NETWORK
source("R/coauthorship_network.R")

#####
# TITLES WORDCLOUD
source("R/title_wordcloud.R")

#####
# NETWORK ANIMATIONS
source("R/network_animations.R")

#####
# PLOT PAPERS METADATA AND PATENTS
source("R/plot_papertype.R")
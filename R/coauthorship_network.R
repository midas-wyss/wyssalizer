# COMBINE ALL PAPERS

# all_papers <- dplyr::bind_rows(scholar_papers, pubmed_papers)
# save(file = paste0(format(Sys.Date(), "%Y-%m-%d"), "_wyss_papers.RData"), all_papers)

# load pre-collected data
load("data/2019-11-10_wyss_papers.RData")

filtered_papers <- all_papers %>% 
  dplyr::filter(., year >= 2006, !is.na(year), journal != "") %>%
  dplyr::filter(., !grepl("US Patent", tofilter, ignore.case = TRUE)) %>%
  dplyr::filter(., !grepl("Supplement", tofilter, ignore.case = TRUE)) %>%
  dplyr::filter(., !grepl("Conference", tofilter, ignore.case = TRUE)) %>%
  dplyr::filter(., !grepl("Abstract", tofilter, ignore.case = TRUE))
  


# data frame with totals over given time span
year_span <- seq(2006, 2019, by = 1)
num_facs <- nrow(wyss_scholar)
paper_totals <- vector()
for (i in seq(1, num_facs)) {
  for (k in seq(1, length(year_span))) {
    a1 <- filtered_papers$tofilter[which(filtered_papers$wyss_scholar == wyss_scholar$pubmed_search[i] & filtered_papers$year == year_span[k])]
    a4 <- tibble::tibble(faculty = wyss_scholar$id[i],
                         type = wyss_scholar$type[i],
                         total_papers = length(a1),
                         year = year_span[k])
    paper_totals <- dplyr::bind_rows(paper_totals, a4)
  }
}
paper_totals <- paper_totals

paper_totals %>% 
  dplyr::filter(., !is.na(faculty), total_papers < 200) %>% 
  ggplot() + 
  geom_tile(aes(x = as.factor(year), y = faculty, fill = total_papers), color = "black") + 
  scale_fill_viridis_c() + 
  facet_grid(type ~ ., scales = "free_y") +
  # coord_polar() +
  theme_minimal() + 
  theme(panel.grid = element_blank())

# build pairs
# year_span <- seq(2006, 2019, by = 1)
# num_facs <- nrow(wyss_scholar)
# N <- vector()
# for (i in seq(1, num_facs)) {
#   message(paste("Papers from",wyss_scholar$pubmed_search[i]))
#   for (j in seq(i+1, num_facs)) {
#     for (k in seq(1, length(year_span))) {
#       a1 <- tmp$tofilter[which(tmp$wyss_scholar == wyss_scholar$pubmed_search[i] & tmp$year == year_span[k])]
#       a2 <- tmp$tofilter[which(tmp$wyss_scholar == wyss_scholar$pubmed_search[j] & tmp$year == year_span[k])]
#       a3 <- length(intersect(a1, a2))
#       a4 <- tibble::tibble(node1 = wyss_scholar$id[i],
#                            node2 = wyss_scholar$id[j],
#                            node1_type = wyss_scholar$type[i],
#                            node2_type = wyss_scholar$type[j],
#                            node1_papers = length(a1),
#                            node2_papers = length(a2),
#                            year = year_span[k],
#                            number_papers = a3)
#       N <- dplyr::bind_rows(N, a4)
#     }
#   }
# }
# 
# N <- N %>% 
#   dplyr::filter(., !is.na(node1), !is.na(node2)) %>% 
#   dplyr::filter(., node1 != node2)







# redo this more intelligently
# year_span <- seq(2006, 2019, by = 1)
# tmp2 <- tmp %>% 
#   dplyr::filter(., year >= 2006)
upaps <- unique(filtered_papers$tofilter)
N <- vector(mode = "list", length = length(upaps))
tmp2 <- wyss_scholar %>% 
  dplyr::mutate(., vid = seq(1, nrow(wyss_scholar)))

for (i in seq(1, length(upaps))) {
  a1 <- which(filtered_papers$tofilter == upaps[i]) # number of times paper is in db
  a2 <- unique(filtered_papers$wyss_scholar[a1]) # wyss authors in it
  a3 <- tmp2$vid[which(tmp2$pubmed_search %in% a2)] # vertex ids
  if (length(a2) > 1) {
    a4 <- unique(filtered_papers$year[a1])
    a5 <- t(combn(a3, m = 2))
    if (length(a2) > 1) {
      N[[i]] <- tibble::tibble(paper = upaps[i],
                               year = a4,
                               author1_vid = a5[, 1],
                               author2_vid = a5[, 2])
    }
  }
}
N <- dplyr::bind_rows(N)


##### NETWORK WITH OBJECT PERMANENCE #####
# a paper gets carried over from the previous years (a cumulative type of approach)
year_span <- seq(2006, 2019)
tmp2 <- wyss_scholar %>% 
  dplyr::mutate(., vid = seq(1, nrow(wyss_scholar)))
PN <- vector(mode = "list")

for (i in seq(1, length(year_span))) {
  b1 <- filtered_papers[filtered_papers$year == year_span[i], ]
  b2 <- unique(b1$tofilter)
  for (j in seq(1, length(b2))) {
    b3 <- which(b1$tofilter == b2[j])
    b4 <- unique(b1$wyss_scholar[b3])
    b5 <- tmp2$vid[which(tmp2$pubmed_search %in% b4)] # vertex ids
    if (length(b4) > 1) {
      b6 <- t(combn(b5, m = 2))
      PN[[j]] <- tibble::tibble(author1_vid = rep(b6[, 1], length(seq(year_span[i], tail(year_span, n = 1)))),
                     author2_vid = rep(b6[, 2], length(seq(year_span[i], tail(year_span, n = 1)))), 
                     year = as.vector(sapply(seq(year_span[i], tail(year_span, n = 1)), function(x) rep(x, nrow(b6)))))
    }
  }
}
PN <- dplyr::bind_rows(PN) %>% dplyr::distinct()




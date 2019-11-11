
x1 <- which(wyss_scholar$scholar_id == "")

st <- expand.grid(wyss_scholar$pubmed_search[x1], wyss_scholar$pubmed_search) %>%
  dplyr::filter(., Var1 != "", Var2 != "")

pair_terms <- apply(st, 1, function(x) paste0(x[1], "[au] AND ", x[2], "[au]"))

pubmed_papers <- vector(mode = "list", length = length(pair_terms))
for (i in seq(1, length(pubmed_papers))) {
  print(i)
  Sys.sleep(time = 0.5)
  a2 <- gsub(str_split(string = pair_terms[i],
                       pattern = "\\[au\\] AND ")[[1]],
             pattern = "\\[au\\]",
             replacement = "")
  
  a1 <- EUtilsSummary(pair_terms[i], type = "esearch", db = "pubmed")
  a3 <- EUtilsGet(a1)
  num_papers <- RISmed::QueryCount(a1)
  
  if (num_papers != 0) {
    b1 <- RISmed::PMID(a3)
    b2 <- RISmed::ArticleTitle(a3)
    b2 <- gsub(pattern = "\\.$", "", b2)
    b3 <- RISmed::YearPubmed(a3)
    b4 <- RISmed::Title(a3)
    b5 <- RISmed::Volume(a3)
    b6 <- RISmed::Issue(a3)
    
    pubmed_papers[[i]] <- tibble::tibble(wyss_scholar = c(rep(a2[1], length(b1)),
                                                          rep(a2[2], length(b1))),
                                         # pmid = rep(b1, 2),
                                         title = rep(b2, 2),
                                         journal = rep(b4, 2),
                                         # volume = rep(b5, 2),
                                         year = rep(b3, 2),
                                         tofilter = rep(paste(tolower(b2), tolower(as.character(b4)), b5, paste0("(", b6, ")"), b3), 2),
                                         search_source = "pubmed")

  } else {
    pubmed_papers[[i]] <- tibble::tibble(wyss_scholar = a2,
                                         # pmid = c(NA, NA),
                                         title = c(NA, NA),
                                         journal = c(NA, NA),
                                         volume = c(NA, NA),
                                         year = c(NA, NA),
                                         tofilter = c(NA, NA))
                              
  }  
}
pubmed_papers <- dplyr::bind_rows(pubmed_papers)

pubmed_papers <- pubmed_papers %>% 
  dplyr::filter(., !is.na(title)) %>% 
  dplyr::select(., wyss_scholar, title, journal, year, tofilter, search_source)

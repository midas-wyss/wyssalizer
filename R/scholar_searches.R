# library(scholar)

# first on scholars with google scholar ids
scholar_papers <- vector(mode = "list", length = nrow(wyss_scholar))
for (i in seq(1, nrow(wyss_scholar))) {
  a1 <- scholar::get_publications(id = wyss_scholar$scholar_id[i], pagesize = 1000)
  scholar_papers[[i]] <- tibble::tibble(wyss_scholar = wyss_scholar$pubmed_search[i],
                                        # family_name = wyss_scholar$last_name[i],
                                        # display_name = paste(wyss_scholar$initials[i], wyss_scholar$last_name[i]),
                                        # group = wyss_scholar$type[i],
                                        # pubids = a1$pubid,
                                        title = a1$title,
                                        journal = a1$journal,
                                        year = a1$year, 
                                        # citations = a1$cites,
                                        # vols = ,
                                        tofilter = paste(tolower(title), tolower(as.character(journal)), sapply(a1$number, function(x) str_split(x, "\\,")[[1]][1]), year),
                                        search_source = "google scholar")
}
scholar_papers <- dplyr::bind_rows(scholar_papers)


faculty_loop <- function(faculty_list) {
  
  
  all_papers <- vector(mode = "list", length = length(faculty_list))
  all_authors <- vector(mode = "list", length = length(faculty_list))
  
  for (i in seq(1, length(faculty_list))) {
    message(paste("Papers by:", faculty_list[i]))
    name <- faculty_list[i]
    a1 <- extract_papers(name)
    a2 <- pubmed_df(a1)
    tmp2 <- a2[[2]]
    tmp2 <- tmp2 %>% tibble::add_column(., faculty = rep(name, nrow(tmp2)), .before = 1)
    all_papers[[i]] <- tmp2
    all_authors[[i]] <- a2[[1]]
  }
  
  all_papers <- dplyr::bind_rows(all_papers)
  all_authors <- dplyr::bind_rows(all_authors)
  
  # collaborative papers will be counted multiple times, so i need to unique-fy the pmids
  upmid <- all_papers$pmid
  
  return(list(papers = all_papers, authors = all_authors))
}

extract_papers <- function(search_term) {
  author <- EUtilsSummary(paste(search_term, "[author]", sep = ""), type = "esearch", db = "pubmed")
  papers <- EUtilsGet(author)
  return(papers)
}

data_extractor <- function(papers, faculty, start_year) {
  pmids <- PMID(papers)
  authors <- Author(papers)
  abstracts <- AbstractText(papers)
  journals <- Title(papers)
  titles <- ArticleTitle(papers)
  year_pubmed <- as.numeric(YearPubmed(papers))
  
  # find papers that belong to the wyss faculty
  x1 <- lapply(authors, function(x) length(grep(x = x$ForeName, pattern = faculty$first_name, ignore.case = TRUE)))
  # x2 <- lapply(authors, function(x) length(grep(x = x$Initials, pattern = faculty$initials, ignore.case = TRUE)))
  # x3 <- union(which(x1 != 0), which(x2 != 0))
  x3 <- which(x1 != 0 & year_pubmed >= start_year)
  
  paper_data <- tibble::tibble(id = faculty$id,
                               pmid = pmids[x3],
                               year = year_pubmed[x3],
                               title = titles[x3],
                               abstract = abstracts[x3],
                               journal = journals[x3])
}




pubmed_df <- function(author_papers){
  
  # res_search <- EUtilsSummary(query, type='esearch', db='pubmed')
  # res_records<- EUtilsGet(res_search)
  pmids <- PMID(author_papers)
  authors <- Author(author_papers)
  abstracts <- AbstractText(author_papers)
  journals <- Title(author_papers)
  titles <- ArticleTitle(author_papers)
  year_pubmed <- as.numeric(YearPubmed(author_papers))
  # mesh_terms <- RISmed::Mesh(author_papers)
  
  # wyss_papers <- grep("Wyss", Affiliation(author_papers))
  
  # 2 data frames:
  # author to pmid
  x <- vector()
  for(i in seq(1,length(authors)))
  {
    a1 <- authors[[i]]
    b1 <- paste(a1$LastName, a1$Initials, sep="_")
    x <- rbind(x, cbind(rep(pmids[i], length(b1)), b1))
  }
  pmid_authors <- tibble::tibble(pmid = x[, 1],
                                 authors = x[, 2])
  
  # pmid to title, abstract, and journal
  paper_metadata <- tibble::tibble(pmid = pmids,
                                   title = titles,
                                   abstract = abstracts,
                                   journal = journals,
                                   year = year_pubmed)
  
  # results
  return(list(authors = pmid_authors, papers = paper_metadata))     
  
}
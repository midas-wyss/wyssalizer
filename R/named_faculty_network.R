x1 <- wyss_scholar %>%
  dplyr::filter(., type == "core" | type == "associate")

x2 <- filtered_papers %>% 
  dplyr::filter(., wyss_scholar %in% x1$pubmed_search)

upaps <- unique(x2$tofilter)
N <- vector(mode = "list", length = length(upaps))
tmp2 <- wyss_scholar %>% 
  dplyr::mutate(., vid = seq(1, nrow(wyss_scholar)))

for (i in seq(1, length(upaps))) {
  a1 <- which(x2$tofilter == upaps[i]) # number of times paper is in db
  a2 <- unique(x2$wyss_scholar[a1]) # wyss authors in it
  a3 <- tmp2$vid[which(tmp2$pubmed_search %in% a2)] # vertex ids
  if (length(a2) > 1) {
    a4 <- unique(x2$year[a1])
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



N <- N %>% 
  # dplyr::filter(., number_papers != 0, node1_type != "staff", node2_type != "staff") %>%
  # dplyr::filter(., number_papers != 0) %>%
  # dplyr::filter(., year == i) %>% 
  dplyr::select(., author1_vid, author2_vid, year, paper) %>%
  dplyr::filter(., author1_vid %in% tmp2$vid, author2_vid %in% tmp2$vid) %>%
  dplyr::group_by(., author1_vid, author2_vid, year) %>%
  dplyr::count()

nix <- setdiff(seq(1, nrow(tmp2)), union(which(tmp2$vid %in% N$author1_vid),
                                         which(tmp2$vid %in% N$author2_vid)))

tmp3 <- tmp2[-nix, ]
tmp3 <- tmp3 %>% 
  dplyr::select(., vid, id, year_joined, type) %>%
  dplyr::mutate(., trick = case_when(.$type == "core" ~ 1, 
                                     .$type == "associate" ~ 2, 
                                     .$type == "staff" ~ 3)) %>%
  dplyr::mutate(., trick = as.factor(trick)) %>%
  dplyr::mutate(., node_size = case_when(.$type == "core" ~ 24, 
                                         .$type == "associate" ~ 16, 
                                         .$type == "staff" ~ 12)) #%>%
# dplyr::mutate(., year = year_joined)

yy <- integer(length = nrow(tmp3))
for (i in seq(1, nrow(tmp3))) {
  a1 <- N$year[which(N$author1_vid == tmp3$vid[i])]
  a2 <- N$year[which(N$author2_vid == tmp3$vid[i])]
  a3 <- unique(c(a1, a2))
  yy[i] <- min(a3)
}

tmp3$year <- yy


num_factors <- length(unique(tmp3$trick))
igraph::graph_from_data_frame(d = N, 
                              directed = TRUE, 
                              vertices = tmp3) %>%
  ggraph(layout = "kk",
         maxiter = 10000,
         dim = 2,
         epsilon = 0.0001,
         kkconst = 0.1) +
  # geom_edge_arc(start_cap = circle(1.75, "mm"),
  #               end_cap = circle(1.75, "mm"),
  #               color = "gray75") +
  geom_edge_link(aes(width = n),
                 start_cap = circle(1.75, "mm"),
                 end_cap = circle(1.75, "mm"),
                 color = "gray75", show.legend = FALSE) +
  # geom_node_point(aes(fill = trick), 
  #                 alpha = 1, 
  #                 size = tmp3$node_size,
  #                 # size = 14,
  #                 color = "gray75", 
  #                 shape = 21, 
  #                 stroke = 2) +
  geom_node_text(aes(fill = trick, label = as.vector(sapply(tmp3$id, function(x) strsplit(x, "_")[[1]][1]))), 
                  alpha = 1, 
                  color = "black") +
  scale_color_viridis_d(labels = c("core faculty", "associate faculty", "staff")) +
  # scale_fill_manual(values = viridis::viridis_pal()(num_factors),
  #                   labels = c("core", "associate", "staff"),
  #                   name = NULL) +
  # labs(title = "Year: {current_frame}") +
  theme_minimal() + 
  guides(fill = guide_legend(override.aes = list(size = 12))) +
  theme(title = element_text(size = 32, color = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size = 24, color = "black"),
        legend.position = "right") 
ggsave(filename = paste0("Camacho.faculty-network-names.", format(Sys.Date(),"%Y-%m-%d"), ".pdf"),
       dpi = 600,
       height = 8,
       width = 11)
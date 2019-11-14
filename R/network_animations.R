# PLOTTING

##### GIFS! #####
# gganimate bit for everyone
# first organize the data
N <- N %>% 
  # dplyr::filter(., number_papers != 0, node1_type != "staff", node2_type != "staff") %>%
  # dplyr::filter(., number_papers != 0) %>%
  # dplyr::filter(., year == i) %>% 
  dplyr::select(., author1_vid, author2_vid, year, paper) %>%
  dplyr::filter(., author1_vid %in% tmp2$vid, author2_vid %in% tmp2$vid)

nix <- setdiff(seq(1, nrow(tmp2)), union(which(tmp2$vid %in% N$author1_vid),
             which(tmp2$vid %in% N$author2_vid)))

tmp3 <- tmp2[-nix, ]
tmp3 <- tmp3 %>% 
  dplyr::select(., vid, id, year_joined, type)

# tmp5 <- tibble::tibble(vid = as.vector(t(sapply(tmp3$vid, rep, 14))),
#                        year = rep(seq(2006, 2019), nrow(tmp3)),
#                        name = as.vector(t(sapply(tmp3$id, rep, 14))),
#                        type = as.vector(t(sapply(tmp3$type, rep, 14))),
#                        color = "gray75") %>% 
#   dplyr::arrange(vid, year)
# 
# uvid <- unique(tmp5$vid)
# for (i in seq(1, length(uvid))) {
#   a1 <- which(tmp5$vid == uvid[i])
#   a2 <- tmp3$year_joined[which(tmp3$vid == uvid[i])]
#   a3 <- tmp3$type[which(tmp3$vid == uvid[i])]
#   if (a3 == "core") {
#     tmp5$color[tmp5$year[a1] >= a2] <- "gold"
#   } else if (a3 == "associate") {
#     tmp5$color[tmp5$year[a1] >= a2] <- "purple"
#   } else if (a3 == "staff") {
#     tmp5$color[tmp5$year[a1] >= a2] <- "lightblue"
#   }
#   
# }



# tmp3$year[tmp3$year == 2010] <- 2006
tmp3 <- tmp3 %>% 
  dplyr::mutate(., trick = case_when(.$type == "core" ~ 1, 
                                     .$type == "associate" ~ 2, 
                                     .$type == "staff" ~ 3)) %>%
  dplyr::mutate(., trick = as.factor(trick))

num_factors <- length(unique(tmp3$trick))
p <- igraph::graph_from_data_frame(d = N, 
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
  geom_edge_link(start_cap = circle(1.75, "mm"),
                end_cap = circle(1.75, "mm"),
                color = "gray75",
                width = 1) +
  geom_node_point(aes(fill = trick), 
                  alpha = 1, 
                  size = 14, 
                  color = "gray75", 
                  shape = 21, 
                  stroke = 2) +
  # scale_color_viridis_d(labels = c("core faculty", "associate faculty", "staff")) +
  scale_fill_manual(values = viridis::viridis_pal()(num_factors),
                    labels = c("core", "associate", "staff"),
                    name = NULL) +
  labs(title = "Year: {current_frame}") +
  theme_minimal() + 
  theme(title = element_text(size = 32, color = "white"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.text = element_text(size = 24, color = "white"),
    legend.position = "right") +
  gganimate::transition_manual(year, cumulative = TRUE)

options(gganimate.dev_args = list(width = 1600, height = 900))

dur <- 3 # <-- number of seconds each frame takes!
frs <- length(year_span) # <-- number of frames
p_ani <- animate(p, fps = frs / dur, detail = 1)
gganimate::anim_save(filename = paste0("res/gifs/", format(Sys.Date(), "%Y-%m-%d"), "_network-coauthorship.gif"), animation = p_ani)
# gganimate::anim_save(filename = paste0("res/",format(Sys.Date(),"%Y-%m-%d"), "_animation-all-members.gif"), animation = p, ani.width = 1000, ani.height = 1000)
  

# faculty only gif
N %>% 
  dplyr::filter(., number_papers != 0, node1_type != "staff", node2_type != "staff") %>%
  # dplyr::filter(., number_papers != 0) %>%
  # dplyr::filter(., year == i) %>% 
  dplyr::filter(., node1 %in% tmp$id, node2 %in% tmp$id) %>%
  igraph::graph_from_data_frame(directed = FALSE, vertices = tmp) %>%
  ggraph(layout = "linear", circular = TRUE) +
  # ggraph(layout = "kk") +
  geom_edge_arc(start_cap = circle(1.75, "mm"),
                end_cap = circle(1.75, "mm"), 
                color = "gray75",
                aes(width = number_papers)) +
  # geom_edge_link(start_cap = circle(1.75, "mm"),
  #               end_cap = circle(1.75, "mm")) +
  geom_node_point(aes(color = type), alpha = 0.5, size = 4) +
  scale_color_viridis_d() + 
  labs(title = "Co-authorship network", subtitle = "Year: {current_frame}") +
  theme_minimal() + 
  theme(# title = element_text(size = 24, color = "black"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(color = "black", fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.position = "none") +
  gganimate::transition_manual(year)
  # gganimate::transition_reveal(year)
gganimate::anim_save(filename = paste0("res/gifs/",format(Sys.Date(),"%Y-%m-%d"), "_animation-only-faculty.gif"))


# count patents per year
all_papers %>% 
    dplyr::filter(., year >= 2006, journal != "") %>% 
    dplyr::filter(., grepl("US Patent", tofilter, ignore.case = TRUE)) %>%
    dplyr::filter(., !grepl("Supplement", tofilter, ignore.case = TRUE)) %>%
    dplyr::filter(., !grepl("Conference", tofilter, ignore.case = TRUE)) %>%
    dplyr::mutate(., journal = tolower(journal)) %>% 
    dplyr::mutate(., journal = "patents/patent applications") %>%
    dplyr::group_by(.,journal, year) %>% 
    dplyr::count() %>% 
    ggplot() + 
    geom_col(aes(x = as.factor(year), y = n), color = "white", fill = "yellow") + 
    scale_fill_viridis_d() +
    # ylim(c(0, 1000)) + 
    theme_minimal() + 
    theme(axis.title = element_blank(),
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black"),
          axis.text = element_text(color = "white", size = 14),
          axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 0.8),
          axis.line = element_line(color = "white"),
          axis.ticks = element_line(color = "white"),
          axis.ticks.length = unit(2, "mm"),
          panel.grid = element_blank(),
          legend.position = "none") +
    gganimate::transition_reveal(year)
gganimate::anim_save(filename = paste0("res/gifs/",format(Sys.Date(),"%Y-%m-%d"), "_animation-patent-explosion.gif"))
ggsave(filename = paste0("res/img/", format(Sys.Date(), "%Y-%m-%d"), "_patents-per-year.pdf"),
         height = 8,
         width = 11,
         dpi = 600)


##### WHITE BACKGROUNDS #####
# co-authorship network
igraph::graph_from_data_frame(d = N, 
                              directed = FALSE, 
                              vertices = tmp3) %>%
  ggraph(layout = "kk", maxiter = 10000) +
  # ggraph(layout = "linear", circular = TRUE) +
  # ggraph(layout = "fr") +
  # geom_edge_arc(start_cap = circle(1.75, "mm"),
  #               end_cap = circle(1.75, "mm"),
  #               color = "gray75") +
  geom_edge_link(start_cap = circle(1.75, "mm"),
                 end_cap = circle(1.75, "mm"),
                 color = "black") +
  geom_node_point(aes(fill = type), 
                  alpha = 0.75, 
                  size = 4, 
                  color = "black", 
                  shape = 21, 
                  stroke = 2) +
  # scale_color_viridis_d() + 
  scale_fill_manual(values = c("orange", "red", "gray75")) +
  labs(title = "Co-authorship network", subtitle = "Year: {current_frame}") +
  theme_minimal() + 
  theme(title = element_text(size = 12, color = "black"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    # panel.background = element_rect(color = "black", fill = "black"),
    # plot.background = element_rect(fill = "black"),
    legend.position = "right") +
  gganimate::transition_manual(year, cumulative = TRUE)
gganimate::anim_save(filename = paste0("res/",format(Sys.Date(),"%Y-%m-%d"), "_coauthorship-whitebg.gif"))



  


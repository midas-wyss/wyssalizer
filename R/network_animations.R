# PLOTTING

##### GIFS! #####
# gganimate bit for everyone
# first organize the data
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
  geom_edge_link(aes(width = n),
                 start_cap = circle(1.75, "mm"),
                end_cap = circle(1.75, "mm"),
                color = "gray75", show.legend = FALSE) +
  geom_node_point(aes(fill = trick), 
                  alpha = 1, 
                  size = tmp3$node_size,
                  # size = 14,
                  color = "gray75", 
                  shape = 21, 
                  stroke = 2) +
  # scale_color_viridis_d(labels = c("core faculty", "associate faculty", "staff")) +
  scale_fill_manual(values = viridis::viridis_pal()(num_factors),
                    labels = c("core", "associate", "staff"),
                    name = NULL) +
  labs(title = "Year: {current_frame}") +
  theme_minimal() + 
  guides(fill = guide_legend(override.aes = list(size = 12))) +
  theme(title = element_text(size = 32, color = "white"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.text = element_text(size = 24, color = "white"),
    legend.position = "right") +
  gganimate::transition_manual(year, cumulative = TRUE) +
  ease_aes("linear") + 
  enter_fade() + 
  exit_fade()

options(gganimate.dev_args = list(width = 1600, height = 900))


## save as gif
altrend <- gifski_renderer(loop = FALSE)
p_ani <- animate(p, fps = 14, duration = 20, renderer = altrend)
gganimate::anim_save(filename = paste0("res/gifs/", format(Sys.Date(), "%Y-%m-%d"), "_network-coauthorship.gif"), animation = p_ani)

## save as mp4
altrend <- ffmpeg_renderer(format = ".mp4")
p_ani <- animate(p, duration  = 20, renderer = altrend)
gganimate::anim_save(filename = paste0("res/gifs/", format(Sys.Date(), "%Y-%m-%d"), "_network-coauthorship.mp4"), animation = p_ani)
p_ani
system(paste("ffmpeg -i", paste0("./res/gifs/", format(Sys.Date(), "%Y-%m-%d"), "_network-coauthorship.mp4"), "-vf 'setpts=24*PTS' ./res/gifs/slow-network-animation.mp4"))


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



  


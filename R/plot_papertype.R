# PAPER TYPE PLOTTING

# patents
# animation
p <- all_papers %>% 
  dplyr::filter(., year >= 2006, journal != "") %>% 
  dplyr::filter(., grepl("US Patent", tofilter, ignore.case = TRUE) | grepl("wo patent", journal, ignore.case = TRUE)) %>%
  dplyr::filter(., !grepl("Supplement", tofilter, ignore.case = TRUE)) %>%
  dplyr::filter(., !grepl("Conference", tofilter, ignore.case = TRUE)) %>%
  dplyr::filter(., !grepl("patent analyst", tofilter, ignore.case = TRUE)) %>%
  dplyr::mutate(., journal = tolower(journal)) %>% 
  dplyr::mutate(., journal = "patents/patent applications") %>%
  dplyr::group_by(.,journal, year) %>% 
  dplyr::count() %>% 
  ggplot() + 
  geom_col(aes(x = as.factor(year), y = n, fill = n), color = "black") + 
  scale_fill_viridis_c() +
  # ylim(c(0, 1000)) + 
  labs(title = "Patent explosion at the Wyss") +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white", size = 32),
        axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 0.8),
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.ticks.length = unit(2, "mm"),
        panel.grid = element_blank(),
        legend.position = "none") +
  gganimate::transition_reveal(year)

options(gganimate.dev_args = list(width = 1600, height = 900))
dur <- 5 # <-- number of seconds each frame takes!
frs <- length(year_span) # <-- number of frames

## save as gif
altrend <- gifski_renderer(loop = FALSE)
# p_ani <- animate(p, fps = frs / dur, detail = 1, renderer = altrend)
p_ani <- animate(p, fps = 1, duration = 20, renderer = altrend)
gganimate::anim_save(filename = paste0("res/gifs/",format(Sys.Date(),"%Y-%m-%d"), "_animation-patent-explosion-blackbg.gif"))

## save as mp4
altrend <- ffmpeg_renderer(format = ".mp4")
p_ani <- animate(p, fps = 14, duration = 20, renderer = altrend)
gganimate::anim_save(filename = paste0("res/gifs/",format(Sys.Date(),"%Y-%m-%d"), "_animation-patent-explosion.mp4"))




# static
all_papers %>% 
  dplyr::filter(., year >= 2006, journal != "") %>% 
  dplyr::filter(., grepl("US Patent", tofilter, ignore.case = TRUE) | grepl("wo patent", journal, ignore.case = TRUE)) %>%
  dplyr::filter(., !grepl("Supplement", tofilter, ignore.case = TRUE)) %>%
  dplyr::filter(., !grepl("Conference", tofilter, ignore.case = TRUE)) %>%
  dplyr::filter(., !grepl("patent analyst", tofilter, ignore.case = TRUE)) %>%
  dplyr::mutate(., journal = tolower(journal)) %>% 
  dplyr::mutate(., journal = "patents/patent applications") %>%
  dplyr::group_by(.,journal, year) %>% 
  dplyr::count() %>% 
  ggplot() + 
  geom_col(aes(x = as.factor(year), y = n), color = "white", fill = "orange") + 
  # ylim(c(0, 1000)) + 
  labs(title = "Patent explosion at the Wyss") +
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
        legend.position = "none")
ggsave(filename = paste0("res/img/", format(Sys.Date(), "%Y-%m-%d"), "_patents-per-year.pdf"),
       height = 8,
       width = 11,
       dpi = 600)


# heatmap of where papers are published
a1 <- filtered_papers[which(filtered_papers$wyss_scholar %in% wyss_scholar$pubmed_search[wyss_scholar$type == "core"]), ]

a1 <- a1 %>% dplyr::filter(., year >= 2006, journal != "", search_source == "google scholar") %>%
  dplyr::mutate(., journal = tolower(journal)) %>% 
  # dplyr::group_by(., wyss_scholar, journal, year) %>% 
  dplyr::group_by(., journal, year) %>%
  dplyr::count()

plot_ly(a1, 
        x = ~year, 
        # y = ~wyss_scholar, 
        # z = ~journal,
        y = ~journal,
        size = ~n, 
        color = ~n,
        colors = viridis_pal()(7),
        hoverinfo = "text",
        # text = ~paste('Faculty:', wyss_scholar, '<br>Journal:', journal, '<br>Year:', year, "<br>Papers:", n),
        text = ~paste("Journal:", journal, "<br>Year:", year, "<br>Papers:", n)) %>%
  add_markers()


# publication trends
a1 <- filtered_papers[which(filtered_papers$wyss_scholar %in% wyss_scholar$pubmed_search[wyss_scholar$type == "core"]), ]

a1 <- a1 %>% dplyr::filter(., year >= 2006, journal != "", search_source == "google scholar") %>%
  dplyr::mutate(., journal = tolower(journal)) %>% 
  # dplyr::group_by(., wyss_scholar, journal, year) %>% 
  dplyr::group_by(., journal, year) %>%
  dplyr::count() %>%
  dplyr::filter(., n >= 5) %>%
  ggplot() +
  geom_point(aes(x = forcats::fct_reorder(journal, desc(n)), y = n, size = n, color = as.factor(year))) +
  # geom_bar(aes(x = forcats::fct_reorder(journal, desc(n)), y = n, fill = year), stat = "identity", color = "black") +
  scale_color_viridis_d() +
  # scale_fill_viridis_d() + 
  # scale_fill_viridis_c() + 
  labs(title = "Year: {current_frame}") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, size = 18, hjust = 1, vjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  gganimate::transition_manual(year, cumulative = TRUE)

options(gganimate.dev_args = list(width = 1600, height = 900))

dur <- 3 # <-- number of seconds each frame takes!
frs <- length(year_span) # <-- number of frames
altrend <- gifski_renderer(loop = FALSE)
p_ani <- animate(p, fps = frs / dur, detail = 1, renderer = altrend)
gganimate::anim_save(filename = paste0("res/gifs/", format(Sys.Date(), "%Y-%m-%d"), "_publication-trends.gif"), animation = p_ani)

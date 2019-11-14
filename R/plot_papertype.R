# PAPER TYPE PLOTTING

# patents
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
  geom_col(aes(x = as.factor(year), y = n), color = "black", fill = "orange") + 
  # ylim(c(0, 1000)) + 
  labs(title = "Patent explosion at the Wyss") +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        # panel.background = element_rect(fill = "black", color = "black"),
        # plot.background = element_rect(fill = "black"),
        axis.text = element_text(color = "black", size = 14),
        axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 0.8),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(2, "mm"),
        panel.grid = element_blank(),
        legend.position = "none") +
  gganimate::transition_reveal(year)
gganimate::anim_save(filename = paste0("res/gifs/",format(Sys.Date(),"%Y-%m-%d"), "_animation-patent-explosion-whitebg.gif"))
ggsave(filename = paste0("res/img/", format(Sys.Date(), "%Y-%m-%d"), "_patents-per-year.pdf"),
       height = 8,
       width = 11,
       dpi = 600)

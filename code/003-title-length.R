
# relative title length + clearly defined, substantive research questions [52,58], total number of words, divided by total number of pages.

items_df_title <- 
  items_df %>% 
  filter(!is.na(pages_n)) %>%  
  filter(!is.na(title_n)) %>% 
  mutate(relative_title_length = log(title_n / pages_n))

# all on one
items_df_title %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             relative_title_length)) +
  geom_boxplot(colour = "red", 
               size = 1)  +
  scale_y_continuous(limits = c(-5, 5),
                     breaks =  seq(-5, 5, 1),
                     labels = seq(-5, 5, 1)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 5)

ggsave("figures/box_title_length.svg")

# over time
items_df_title %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             relative_title_length,
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01)+
  theme_minimal() +
  ylab("Relative title length")

ggsave("figures/box_title_length_over_time.png")

# by journal
items_df_title %>% 
  mutate(journal = str_wrap(journal, 20)) %>% 
  ggplot(aes(y = reorder(journal,
                     relative_title_length), 
             x = relative_title_length,
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  guides(fill = 'none') +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("") +
  ylab("Relative title length")

ggsave("figures/box_title_length_over_time.png")


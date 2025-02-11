#  ------------------------------------------------------------------
# length of article - less need to introduce, justify and explain study [47,52]

# all on one
library(tidyverse)

items_df %>% 
  ggplot(aes(year,
             log(pages_n))) +
  geom_boxplot(colour = "red", 
               size = 1)  +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 6)

ggsave("figures/box_num_pages.svg")


# over time
items_df %>% 
  ggplot(aes(year,
             log(pages_n),
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01) +
  theme_minimal() +
  ylab("Number of pages (ln)") 

ggsave("figures/box_num_pages_over_time.png")

# by journal, wow yes!
library(ggforce)
items_df %>% 
  mutate(journal = str_wrap(journal, 20)) %>% 
  ggplot(aes(y = reorder(journal,
                     pages_n), 
             x = log(pages_n),
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  guides(fill = 'none') +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("Number of pages (ln)") +
  ylab("")

ggsave("figures/box_num_pages_by_journal.png")


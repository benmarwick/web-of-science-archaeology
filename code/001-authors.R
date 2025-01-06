library(tidyverse)

items_df <- read_rds("data/wos-data-df.rds")

# number of authors + greater scope and need for collaboration [9,44]

# all on one

items_df %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             log(authors_n))) +
  geom_boxplot(colour = "red", 
               size = 1)  +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 6)

ggsave("figures/box_num_authors.svg")

# over time
items_df %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             log(authors_n),
             group = year)) +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  ylab("Number of authors (ln)")

ggsave("figures/box_num_authors_over_time.png")

# by journal, wow yes!
library(ggridges)
items_df %>% 
  mutate(journal = str_wrap(journal, 20)) %>% 
  ggplot(aes(y = reorder(journal,
                     authors_n), 
             x = log(authors_n),
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  guides(fill = 'none') +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6)) +
  ylab("") + 
  xlab("Number of authors (ln)")


ggsave("figures/box_num_authors_by_journal.png")

# histogram

tally_authors_n <- 
  items_df %>% 
  group_by(authors_n) %>% 
  tally() %>% 
  mutate(perc = n / sum(n) * 100) %>% 
  mutate(cum_per = cumsum(perc)) %>% 
  arrange(cum_per) # 90% of papers have 4 or fewer authors

items_df %>% 
  ggplot(aes(authors_n)) +
  geom_histogram()  +
  theme_minimal() +
  scale_x_log10() +
  xlab("Number of authors") + 
  ylab("Count") 

ggsave("figures/box_num_histogram.png")

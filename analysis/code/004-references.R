#  ------------------------------------------------------------------
# number of references - less need to justify, explain and support study [47]


# all on one

items_df %>% 
  ggplot(aes(year,
             sqrt(refs_n))) +
  geom_boxplot(colour = "red", 
               size = 1)  +
  scale_y_continuous(limits = c(0, 20)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 6)

ggsave("figures/box_num_refs.svg")


# over time
items_df %>% 
  ggplot(aes(year,
             sqrt(refs_n),
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01)+
  theme_minimal() +
  ylab("Number of references (ln)")

ggsave("figures/box_num_refs_over_time.png")

# by journal, wow yes!
items_df %>% 
  mutate(journal = str_wrap(journal, 20)) %>% 
  ggplot(aes(y = reorder(journal,
                     refs_n), 
             x = sqrt(refs_n),
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  guides(fill = 'none') +
  theme_minimal()  +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("Number of references (ln)") +
  ylab("")

ggsave("figures/box_num_refs_by_journal.png")

#  ------------------------------------------------------------------
#  references to monographs - focus on simpler questions; less need to justify, explain and support study [53,54]

# not easy...
#  ------------------------------------------------------------------
# age of references - faster settling of disagreements; greater potential to build research upon previous findings [44,56]

# Derek de Solla Price proposed an index, which measures the proportion of cited references published in the five years preceding the citing paper


# years are ", YYYY, "
library(stringr)

# output storage
prices_index <- vector("list", length = nrow(items_df))

# loop, 
for(i in seq_len(nrow(items_df))){
  
  refs <-  items_df$refs[i]
  year <-  items_df$year[i]
  
  ref_years <- 
    as.numeric(str_match(str_extract_all(refs, ", [0-9]{4}, ")[[1]], "\\d{4}"))
  
  preceeding_five_years <-  
    seq(year - 5, year, 1)
  
  refs_n_in_preceeding_five_years <- 
    ref_years[ref_years %in% preceeding_five_years]
  
  prices_index[[i]] <- 
    length(refs_n_in_preceeding_five_years) / length(ref_years)
  
  # for debugging
  # print(i)
  
}

prices_index <- flatten_dbl(prices_index)

# how many are zero?
length(prices_index[prices_index < 0.00000001])

# add to data frame
items_df$prices_index <-  prices_index

# saveRDS(items_wth_refs, "data/items_wth_refs_price_index.rds")

# plot

# all on one

items_df %>% 
  ggplot(aes(year,
             prices_index)) +
  geom_boxplot(colour = "red", 
               size = 1)  +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 30)

ggsave("figures/box_age_refs.svg")


# over time
items_df %>% 
  ggplot(aes(year,
             prices_index,
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01)+
  theme_minimal() +
  ylab("Price's Index")

ggsave("figures/box_age_refs_over_time.png")

# by journal
items_df %>% 
  mutate(journal = str_wrap(journal, 20)) %>% 
  group_by(journal) %>% 
  filter(n() > 500) %>% 
  ggplot(aes(reorder(journal,
                     prices_index), 
             prices_index,
             group = journal)) +
  geom_boxplot()  +
  coord_flip()+
  theme_minimal() +
  ylab("Price's Index") +
  xlab("")

ggsave("figures/box_age_refs_by_journal.png")

#  ------------------------------------------------------------------
# diversity of sources - fewer research topics, which are of more general interest [47,57]

# journal name as species
# article as habitat

# simplify the refs, since they are a bit inconsistent
ref_list1 <- map(items_df$refs, ~tolower(.x))
ref_list2 <- map(ref_list1, ~str_replace_all(.x, "\\.|,| ", ""))
ref_list3 <- map(ref_list2, ~str_split(.x, "\n"))
ref_list4 <- map(ref_list3, ~data_frame(x = .x))
ref_list5 <- bind_rows(ref_list4, .id = "id") # 8042
ref_list6 <- unnest(ref_list5)
# one long vec of all refs

# get the journal names out of the refs
ref_list7 <- 
  ref_list6 %>% 
  mutate(journal_name = gsub("\\-", "", x)) %>% 
  mutate(journal_name = gsub("\\:", "", journal_name)) %>% 
  mutate(journal_name = gsub("^[a-z'\\(\\)\\:]+[0-9]{4}", "", journal_name)) %>% 
  mutate(journal_name = gsub("v[0-9]+.*", "", journal_name)) %>% 
  mutate(journal_name = gsub("p[0-9]+$", "", journal_name))

# prepare to compute shannon and join with other variables
items_df$id <- 1:nrow(items_df)

# tally of all referenced items
all_cited_items <- 
  ref_list7 %>% 
  select(x) %>% 
  group_by(x) %>% 
  tally() %>% 
  arrange(desc(n)) 

# get a list of the top journals
top_journals <- 
  ref_list7 %>% 
  select(journal_name) %>% 
  group_by(journal_name) %>% 
  tally() %>% 
  filter(n > 50) %>% 
  arrange(desc(n)) 

# In the Shannon index, p_i is the proportion (n/N) of individuals of one particular species (journal) found (n) divided by the total number of individuals found (N), ln is the natural log, Σ is the sum of the calculations, and s is the number of species. 


# compute diversity of all citations
# for each article (habitat)
shannon_per_item <- 
  ref_list7 %>% 
  group_by(id, x) %>% 
  tally() %>% 
  mutate(n_in_article = n) %>% 
  select(-n) %>% 
  left_join(all_cited_items) %>% 
  mutate(p_i = n / sum(n, na.rm = TRUE)) %>% 
  mutate(p_i_ln = log(p_i)) %>% 
  group_by(id) %>% 
  summarise(shannon = -sum(p_i * p_i_ln, na.rm = TRUE)) %>% 
  mutate(id = as.numeric(id)) %>% 
  arrange(id)  %>% 
  left_join(items_df)

# all on one
shannon_per_item %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             shannon)) +
  geom_boxplot(colour = "red",
               size = 1)  +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal()  +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 4.5)

#ggsave("figures/box_div_refs.svg")

# over time
shannon_per_item %>% 
  ggplot(aes(year,
             shannon,
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01)+
  theme_minimal() +
  ylab("Shannon Index")

ggsave("figures/box_div_refs_over_time.png")

# by journal
shannon_per_item %>%  
  mutate(journal = str_wrap(journal, 20)) %>% 
  group_by(journal) %>% 
  filter(n() > 500) %>% 
  ggplot(aes(reorder(journal,
                     shannon), 
             shannon,
             group = journal)) +
  geom_boxplot()  +
  coord_flip()+
  theme_minimal() +
  ylab("Shannon Index") +
  xlab("")

ggsave("figures/box_div_refs_by_journal.png")

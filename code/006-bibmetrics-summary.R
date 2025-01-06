
#--------------------------------
# Six variables over time

# https://github.com/benmarwick/March-2019-Cambridge-Big-Data-Archaeology/blob/master/code/002-fanelli-hierarchy-of-sciences.R

over_time <- 
  items_df %>% 
  left_join(items_df_title) %>% 
  left_join(shannon_per_item) %>% 
  mutate(log_authors_n = log(authors_n),
         log_pages_n = log(pages_n),
         sqrt_refs_n = sqrt(refs_n),
         journal_wrp = str_wrap(journal, 30)) %>% 
  select(year,
         log_authors_n,
         log_pages_n,
         prices_index, 
         shannon,
         sqrt_refs_n, 
         relative_title_length)

over_time_long <- 
  over_time %>% 
  ungroup() %>% 
  select(-journal) %>% 
  gather(variable, 
         value,
         -year) %>% 
  filter(value != -Inf,
         value !=  Inf) %>% 
  mutate(variable = case_when(
    variable == "log_authors_n" ~ "N. of authors (ln)",
    variable == "log_pages_n"   ~ "N. of pages (ln)",
    variable == "prices_index"  ~ "Price's index",
    variable == "shannon"  ~ "Shannon div. of sources",
    variable == "sqrt_refs_n"  ~ "N. of refs (sqrt)",
    variable == "relative_title_length"  ~ "Relative title length (ln)"
  ))

# compute beta estimates so we can colour lines to indicate more or
# less scientific
library(broom)
over_time_long_models <- 
  over_time_long %>% 
  group_nest(variable) %>% 
  mutate(model = map(data, ~tidy(lm(value ~ year, data = .)))) %>% 
  unnest(model) %>% 
  filter(term == 'year') %>% 
  mutate(becoming_more_scientific = case_when(
    variable == "N. of authors (ln)" & estimate > 0 ~ "TRUE",
    variable == "N. of pages (ln)"   & estimate < 0 ~ "TRUE",
    variable == "N. of refs (sqrt)"  & estimate < 0 ~ "TRUE",
    variable == "Price's index"     & estimate > 0 ~ "TRUE",
    variable == " Relative title length (ln)"     & estimate > 0 ~ "TRUE",
    variable == "Shannon div. of sources"     & estimate < 0 ~ "TRUE",
    TRUE ~ "FALSE"
  )) 

# join with data
over_time_long_colour <- 
  over_time_long %>% 
  left_join(over_time_long_models)

ggplot(over_time_long_colour,
       aes(year, 
           value,
           colour = becoming_more_scientific)) +
  geom_point(alpha = 0.005) +
  geom_smooth(method = "lm", 
              size = 3) +
  facet_wrap( ~ variable,
              scales = "free_y") +
  theme_minimal(base_size = 12) +
  scale_color_manual(values = c("red", "green")) +
  guides(colour = "none") +
  ylab("")



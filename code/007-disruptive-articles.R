
# https://github.com/benmarwick/March-2019-Cambridge-Big-Data-Archaeology/blob/master/code/003-disruptiveness-index.R
# https://pubsonline.informs.org/doi/10.1287/mnsc.2015.2366
# https://www.nature.com/articles/s41586-019-0941-9

library(tidyverse)
library(here)

# get articles with DOIs and reference lists
items_wth_dois_and_refs <- 
  items_df %>% 
  filter(str_detect(doi, "^10.")) %>% 
  mutate(doi = str_remove_all(doi, "\\[|\\]")) %>% 
  mutate(doi_article = doi) %>% 
  select(-doi)

# get items in the reference lists with DOIs
items_df_one_row_per_ref_with_doi <- 
  items_wth_dois_and_refs %>% 
  separate_rows(refs, sep = "\n  ") %>% 
  ungroup %>% 
  mutate(refs = tolower(str_trim(refs))) %>% 
  mutate(refs = str_extract(refs, "doi.*")) %>% 
  mutate(refs = str_remove_all(refs, "\\[|\\]")) %>% 
  mutate(doi = str_remove_all(refs, "doi"))  %>% 
  mutate(doi_cited = str_trim(doi)) %>%  
  filter(!is.na(doi_cited)) %>% 
  select(-doi)

# ------- compute Disruption  ----
# for a focal article:
# - find the articles that cite the focal article
# - find what articles that citing article cites 
# - find the ratio of citing articles that cite focal articles refs vs those that don't

disruption_output <- list(length = length(items_wth_dois_and_refs$doi_article))

for(i in seq_along(items_wth_dois_and_refs$doi_article)){
  
  this_article_doi <- items_wth_dois_and_refs$doi_article[i] # 1000
  
  # what articles are cited by the focal paper
  cited_by_focal_paper <- 
    items_df_one_row_per_ref_with_doi %>% 
    filter(doi_article %in% this_article_doi) 
  
  # what articles cite the focal paper, that are in our list
  citing_the_focal_paper <- 
    items_df_one_row_per_ref_with_doi %>% 
    filter(doi_cited %in% this_article_doi)
  
  # what articles are cited by the citing papers
  cited_by_citing_papers <- 
    items_df_one_row_per_ref_with_doi %>% 
    filter(doi_article %in% citing_the_focal_paper$doi_article)
  
  # articles that cite the focal paper's citations, but not the focal paper itself
  n_k <- 
    items_df_one_row_per_ref_with_doi %>% 
    group_nest(doi_article) %>% 
    mutate(cites_focal_paper = map_lgl(data, ~(this_article_doi %in% .x$doi_cited))) %>% 
    filter(!cites_focal_paper) %>% 
    mutate(cites_focal_papers_citations = map_int(data, ~sum(.x$doi_cited %in% cited_by_focal_paper$doi_cited))) %>% 
    filter(cites_focal_papers_citations >= 1) %>% 
    nrow()
  
  # do the citing papers cite any of the references in the focal paper?
  common_cites <- 
    cited_by_citing_papers %>% 
    group_by(doi_article) %>% 
    summarise(common_cites = sum(doi_cited %in% cited_by_focal_paper$doi_cited ))  %>% 
    count(common_cites) 
  
  n_i <- # cite focal article, but none of its references
    common_cites[common_cites$common_cites == 0,]$n
  
  n_j <- # cite focal article, and at least one of its references
    common_cites[common_cites$common_cites == 1,]$n
  
  disruption <- (n_i - n_j) / (n_i + n_j + n_k)
  
  disruption_output[[i]] <- tibble(doi = this_article_doi,
                                   n_i = n_i,
                                   n_j = n_j,
                                   n_k = n_k,
                                   disruption = disruption)
  print(i)
  
}

disruption_df <- 
  bind_rows(disruption_output) %>% 
  arrange(desc(disruption)) %>% 
  left_join(items_wth_dois_and_refs, 
            by = c('doi' = 'doi_article')) %>% 
  mutate(first_author = str_remove_all(authors, '\\"')) %>% 
  mutate(first_author = gsub("^(.*?),.*", "\\1", first_author)) %>% 
  mutate(short_ref = as.character(str_glue('{first_author} {year}, {str_trunc(title, 50)}')))

saveRDS(disruption_df, here("data",  "disruption_df.rds"))

ggplot() +
  geom_histogram(data = disruption_df,
                 aes(disruption)) +
  theme_minimal(base_size = 12) +
  xlab("Disruption index") +
  scale_x_continuous(limits = c(-1, 1)) +
  xlab(str_glue("Disruption index values for {nrow(disruption_df)} articles from Web of Science (1998-2018)")) 

ggsave(here("figures", "disruption-index-plain.png"),
       h = 5, w = 7)


disruption_df_top_10 <-  
  disruption_df %>% 
  arrange(desc(disruption)) %>% 
  slice(1:10) 

disruption_df_bottom_10 <-  
  disruption_df %>% 
  arrange((disruption)) %>% 
  slice(1:10) 

y_pos <-  seq.int(from = 2000, 
                  to = 1000, 
                  length.out = length(disruption_df_top_10$disruption))

y_pos_bottom <-  seq.int(from = 1000, 
                         to = 500, 
                         length.out = length(disruption_df_bottom_10$disruption))

main <- 
  ggplot() +
  geom_histogram(data = disruption_df,
                 aes(disruption),
                 alpha = 0.4) +
  theme_minimal(base_size = 12) +
  xlab(str_glue("Disruption index values for {nrow(disruption_df)} articles from Web of Science (1998-2018)")) +
  scale_x_continuous(limits = c(-1, 1)) +
  geom_segment(data = disruption_df_top_10,
               aes(x = disruption,
                   xend = disruption,
                   y = rep(0, length(disruption_df_top_10$disruption)),
                   yend = y_pos),
               colour = "darkgreen",
               alpha = 1,
               size = 0.5) +
  geom_segment(data = disruption_df_bottom_10,
               aes(x = disruption,
                   xend = disruption,
                   y = rep(0, length(disruption_df_bottom_10$disruption)),
                   yend = y_pos_bottom),
               colour = "darkred",
               alpha = 1, 
               size = 0.5) +
  annotate("text", 
           x = disruption_df_top_10$disruption - 0,
           y = y_pos,
           label = disruption_df_top_10$short_ref, 
           colour = "darkgreen",
           size = disruption_df_top_10$disruption * 5,
           hjust = 1) +
  annotate("text", 
           x = disruption_df_bottom_10$disruption - 0,
           y = y_pos_bottom,
           label = disruption_df_bottom_10$short_ref, 
           colour = "darkred",
           size = -disruption_df_bottom_10$disruption * 10,
           hjust = 1) +
  # white mask for the subplot
  geom_rect(data=disruption_df,
            xmin= -1.05, 
            xmax= -0.5, 
            ymin= 150, 
            ymax= 250,
            fill="white", alpha=0.5)

# we don't see the team size-disruptiveness relationship here!

disruption_df %>% 
  # filter( disruption != 0) %>% 
  lm(authors_n ~ disruption, data = .) %>% 
  broom::glance()


team_size <- 
  ggplot(disruption_df, 
         aes(disruption, authors_n)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(alpha = 0.2, method = "lm") +
  xlab("Disruption index values") +
  ylab(str_glue("Number of authors\n(total = {sum(disruption_df$authors_n)})")) +
  theme_bw(base_size = 6)

main + 
  annotation_custom(ggplotGrob(team_size), 
                    xmin= -1.05, 
                    xmax= -0.5, 
                    ymin= 150, 
                    ymax= 550) 

ggsave(here("figures", "disruption-index-papers.png"),
       h = 5, w = 7)

# which journals are publishing the most disruptive work?

journals_more_than_five <- 
  disruption_df %>%  
  group_by(journal) %>% 
  tally(sort = TRUE) %>% 
  filter(n > 5) %>% 
  pull(journal)

# what proportion of articles have +ve values?
journals_with_high_disruption <- 
  disruption_df %>% 
  filter(journal %in% journals_more_than_five) %>% 
  group_by(journal) %>% 
  summarise(n_pos = sum(disruption > 0.5 ),
            p_pos = n_pos / n()) %>% 
  arrange(desc(p_pos) ) %>% 
  filter(p_pos > 0) %>% 
  pull(journal)

disruption_df <- 
  disruption_df %>% 
  mutate(high_disrupt = ifelse(journal %in% journals_with_high_disruption, TRUE, FALSE))


library(ggbeeswarm)
disruption_df %>% 
  filter(journal %in% journals_more_than_five) %>% 
  mutate(journal = str_wrap(journal, 20)) %>% 
  group_by(journal) %>% 
  mutate(mean = mean(disruption)) %>% 
  arrange(desc(mean)) %>% 
  ggplot(aes(reorder(journal,
                     mean),
             disruption)) +
  geom_boxplot()  +
  geom_quasirandom(alpha = 0.4,
                   size = 3,
                   aes(colour = high_disrupt)) +
  coord_flip() +
  xlab("") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) +
  guides(colour = "none")

ggsave(here("figures", "disruption-index-journals.png"),
       h = 5, w = 8)








# use of first person (singular vs. plural) in abstracts, - universal validity of claims; less scope for argumentation; fewer appeals to opinion and authority [59], the proportion of first person pronouns, both singular and plural (i.e. ‘‘I’’, ‘‘me’’, ‘‘mine’’, ‘‘we’’, ‘‘our’’ etc.) among all words in the abstract.

i <- c(" i'd ", " i'll ", " i'm ", " i've ", " i ")
me <- c(" me ", " my ", " mine ")
us <- c(" us ", " our ", " ours ")
first_persons <- c(i, me, us)

library(stringi)

# function to compute proportion of first person pronouns
first_person_prop <- function(x){
  x <- tolower(x)
  firsts <- sum(str_count(x, first_persons))
  abstract_n_words <- stri_count_words(x)
  firsts / abstract_n_words
}


# compute proportions, single author papers only
items_df_abstract <- 
  items_df %>% 
  mutate(abstract_first_person = first_person_prop(abstract)) 

# plots
# all on one
items_df_abstract %>% 
  ggplot(aes(year,
             abstract_first_person)) +
  geom_boxplot()  +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal()

# over time
items_df_abstract %>% 
  ggplot(aes(year,
             abstract_first_person,
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01) +
 # scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme_minimal()

# by journal
items_df_abstract %>% 
  mutate(journal = str_wrap(journal, 20)) %>% 
  ggplot(aes(y = reorder(journal,
                     abstract_first_person), 
             x = abstract_first_person,
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  guides(fill = 'none') +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))  +
  ylab("") +
  xlab("Proportion of first person pronouns (singular vs. plural) in abstracts")


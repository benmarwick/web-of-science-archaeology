# read in the data, Jan 2012 data for comparison with Fanelli?
wos_files <- dir("analysis/data",  full.names = TRUE)

library(tidyverse)
text <- map(wos_files, 
            ~read_file(.x, 
                       locale = locale(encoding = "latin1")))

# split on article delimiter provided by WOS
items <- unlist(str_split(text, 
                          pattern = "\nPT J\n"))

# get rid of the advertising
items <- str_replace_all(items, 
                         "FN Clarivate Analytics Web of Science\nVR 1.0", "")
items <- items[items != ""] 

#  length(items) # each item is one article

# function to automate getting the variables out of each item
extractor <- function(i){
  
  # debug with 
  # i <- items[[3]]
  
  authors =     gsub(".*AU *(.*?) *\nAF .*", "\\1", i)
  authors_n =   str_count(authors, "\n") + 1
  title =       gsub(".*\nTI *(.*?) *\nSO .*", "\\1", i)
  title_n =     str_count(title, "\\w+") 
  journal =     gsub(".*\nSO *(.*?) *\nLA .*", "\\1", i)
  abstract =    gsub(".*\nAB *(.*?) *\nC1 .*", "\\1", i)
  refs =        gsub(".*\nCR *(.*?) *\nNR .*", "\\1", i)
  refs_n =      as.numeric(gsub(".*\nNR *(.*?) *\nTC .*", "\\1", i))
  pages_n =     as.numeric(gsub(".*\nPG *(.*?) *\nWC .*", "\\1", i))
  year =        as.numeric(gsub(".*\nPY *(.*?) *\nVL .*", "\\1", i))
  doi =         gsub(".*\nDI *(.*?) *\nPG .*", "\\1", i)
  
  dplyr::data_frame(
    authors =         authors,
    authors_n =       authors_n,
    title =           title ,
    title_n =         title_n,
    journal =         journal,
    abstract  =       abstract,
    refs =            refs    ,
    refs_n =          refs_n ,
    pages_n =         pages_n,
    year =            year,
    doi =             doi
  )
}


# # for debugging, to find the items that break the fn
# for(i in seq_len(length(items))){
#   extractor(items[i])
#   print(i)
# }


# this will take a few mins
items_df <- map_df(items, ~extractor(.x)) %>% 
  # uniques only 
  group_by(authors, title, journal) %>% 
  filter(row_number() == 1)  %>% 
  # keep only those with refs
  filter(!is.na(refs)) %>% 
  filter(!is.na(year)) 

# keep only the top 25 journals by IF

# https://jcr.clarivate.com/jcr/browse-journals?query=ewAiAGMAYQB0AGUAZwBvAHIAaQBlAHMAIgA6AHsAIgBmAGkAbAB0AGUAcgBGAGkAZQBsAGQAIgA6ACIAYwBhAHQAZQBnAG8AcgBpAGUAcwAiACwAIgBmAGkAbAB0AGUAcgBOAGEAbQBlACIAOgAiAEMAYQB0AGUAZwBvAHIAaQBlAHMAIgAsACIAcwBlAGwAZQBjAHQAZQBkAEYAaQBsAHQAZQByAHMATABpAHMAdAAiADoAWwB7ACIAbgBhAG0AZQAiADoAIgBBAFIAQwBIAEEARQBPAEwATwBHAFkAIgAsACIAYwBhAHQAZQBnAG8AcgB5AEkAZAAiADoAIgBCAEkAIgAsACIAZQBkAGkAdABpAG8AbgBzACIAOgAiAEEASABDAEkAIgAsACIAagBjAHIAWQBlAGEAcgAiADoAMgAwADIAMgB9AF0AfQAsACIAYwBpAHQAYQB0AGkAbwBuAEkAbgBkAGUAeAAiADoAewAiAGYAaQBsAHQAZQByAEYAaQBlAGwAZAAiADoAIgBjAGkAdABhAHQAaQBvAG4ASQBuAGQAZQB4ACIALAAiAGYAaQBsAHQAZQByAE4AYQBtAGUAIgA6ACIAQwBpAHQAYQB0AGkAbwBuACAASQBuAGQAZQB4AGUAcwAiACwAIgBzAGUAbABlAGMAdABlAGQARgBpAGwAdABlAHIAcwBMAGkAcwB0ACIAOgBbACIAQQBIAEMASQAiAF0AfQAsACIAagBjAHIAWQBlAGEAcgAiADoAewAiAGYAaQBsAHQAZQByAEYAaQBlAGwAZAAiADoAIgBqAGMAcgBZAGUAYQByACIALAAiAGYAaQBsAHQAZQByAE4AYQBtAGUAIgA6ACIASgBDAFIAIABZAGUAYQByACIALAAiAHMAZQBsAGUAYwB0AGUAZABGAGkAbAB0AGUAcgBzAEwAaQBzAHQAIgA6ADIAMAAyADIAfQB9AA%3D%3D

# JCI top 25 journals by 2022 JCI

# header row that I deleted to make it a valid CSV
# Journal Data Filtered By:  Selected Categories: ARCHAEOLOGY Selected Editions: AHCI Selected JCR Year: 2022 Selected Category Schema: WOS Selected Open Access: N Indicator: Default											

library(tidyverse)

jci <- read_csv("analysis/data/BenMarwick_JCR_JournalResults_12_2024.csv")

jci_top_25 <- 
  jci %>% 
  arrange(desc(`2022 JIF`)) %>% 
  mutate(journal = toupper(`Journal name`)) %>% 
  slice(1:25)

items_df <- 
items_df %>% 
  filter(journal %in% jci_top_25$journal)

# how many journals? how many articles per journals
items_df_journal_tally <- 
  items_df %>% 
  group_by(journal) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

journals_with_100_plus_articles <- 
  items_df_journal_tally %>% 
  filter(n >= 100) %>% 
  pull(journal)

# keep only those with 100 or more articles 
items_df <- 
items_df %>% 
  filter(journal %in% journals_with_100_plus_articles)

# save to disk so we can re-use it for the next steps to save time
saveRDS(items_df, "data/wos-data-df.rds")



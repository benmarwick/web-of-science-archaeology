
over_time <-  
  items_df %>%  
  left_join(items_df_title) %>%  
  left_join(shannon_per_item) %>%  
  filter(relative_title_length != -Inf, 
         relative_title_length !=  Inf,
         prices_index != "NaN" 
  ) %>% 
  mutate(log_authors_n = log(authors_n), 
         log_pages_n = log(pages_n), 
         journal_wrp = str_wrap(journal, 30)) %>%  
  select(year, 
         log_authors_n, 
         log_pages_n, 
         prices_index,  
         shannon, 
         relative_title_length,
         authors_n,
         pages_n) 

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
    variable == "prices_index"  ~ "Recency of references", 
    variable == "shannon"  ~ "Diversity of references", 
    variable == "relative_title_length"  ~ "Relative title length (ln)", 
    variable == "authors_n" ~ "N. of authors",
    variable == "pages_n" ~ "N. of pages"
  )) %>%  
  filter(!is.na(variable)) %>%  
  filter(!is.nan(value)) %>%  
  filter(!is.na(value)) %>%  
  filter(value != "NaN") %>%  
  mutate(value = parse_number(value))

# compute beta estimates so we can colour lines to indicate more or
# less hard

library(broom)

over_time_long_models <- 
  over_time_long %>%  
  group_nest(variable) %>%  
  mutate(model = map(data, ~tidy(lm(value ~ year, data = .)))) %>%  
  unnest(model) %>%  
  filter(term == 'year') %>%  
  mutate(becoming_more_scientific = case_when( 
    variable == "N. of authors"         & estimate > 0 ~ "TRUE", 
    variable == "N. of pages"           & estimate < 0 ~ "TRUE", 
    variable == "N. of refs (sqrt)"          & estimate < 0 ~ "TRUE", 
    variable == "Recency of references"      & estimate > 0 ~ "TRUE", 
    variable == "Relative title length (ln)" & estimate > 0 ~ "TRUE", 
    variable == "Diversity of references"    & estimate < 0 ~ "TRUE",
    TRUE ~ "FALSE"
  )) 

# join with data
over_time_long_colour <- 
  over_time_long %>% 
  left_join(over_time_long_models)

over_time_long_colour_gams_testing_nested_df <- 
  over_time_long_colour %>% 
  nest(.by = variable) 

# check the distributions of each variable
each_variable <- 
map(over_time_long_colour_gams_testing_nested_df$data, 
    ~.x$value)

names(each_variable) <- over_time_long_colour_gams_testing_nested_df$variable

stack(each_variable) %>% 
  ggplot() +
  aes(values) +
  geom_histogram() +
  facet_wrap(~ind, scales = "free")


over_time_long_colour_gams_testing_nested_df %>% 
  mutate(mod_gam = case_when(
    variable == "N. of authors" ~ lapply(data, 
                            function(df) gam(value ~ s(year, bs = "cr"), 
                                           family = nb(),
                                           method = "REML",
                                           data = df)),
    variable == "N. of pages" ~ lapply(data, 
                             function(df) gam(value ~ s(year, bs = "cr"), 
                                          family = gaussian(),
                                           method = "REML",
                                           data = df)),
    variable == "Recency of references" ~ lapply(data, 
                              function(df) gam(value ~ s(year, bs = "cr"), 
                                           family = betar(link = "logit"),
                                           method = "REML",
                                           data = df)),
    variable == "Diversity of references" ~ lapply(data, 
                              function(df) gam(value ~ s(year, bs = "cr"), 
                                           family = gaussian(),
                                           method = "REML",
                                           data = df)),
    variable == "Relative title length (ln)" ~ lapply(data, 
                              function(df) gam(value ~ s(year, bs = "cr"), 
                                           family = gaussian(),
                                           method = "REML",
                                           data = df)),
    .default = NULL
    ))

# inspect diagnostics
map(over_time_long_colour_gams_testing$mod_gam,
    gam.check)

library(gratia)
library(patchwork)

plots_with_titles <- 
over_time_long_colour_gams_testing %>%
  mutate(appraise_plot = map2(mod_gam, variable, ~ {
    p <- appraise(.x)  
    p + plot_annotation(title = .y)
  }))

plots_with_titles$appraise_plot[2]


# the nested df is 'over_time_long_colour_gams_testing_nested_df'
# with list-column 'data' and character column 'variable'

# Ensure purrr is loaded
library(purrr)
library(dplyr)
library(mgcv)

# the nested df is 'over_time_long_colour_gams_testing_nested_df'
# with list-column 'data' and character column 'variable'

over_time_long_colour_gams_testing_nested_df_out <- 
  over_time_long_colour_gams_testing_nested_df %>%
  mutate(
    mod_gam = map2(data, variable, ~{ # Map over data (.x) and variable (.y)
      current_df <- .x # The dataframe for the current row
      current_var <- .y # The variable name for the current row
      
      # --- Use if/else if/else to choose the family ---
      if (current_var == "N. of authors") {
        chosen_family <- nb() # ok 
      } else if (current_var == "N. of pages") {
        chosen_family <- nb() # ok
      } else if (current_var == "Recency of references") {
        chosen_family <- betar(link = "logit") # needs work
      } else if (current_var == "Diversity of references") {
        chosen_family <- gaussian() # ok
      } else if (current_var == "Relative title length (ln)") {
        chosen_family <- gaussian() # ok
      } else {
        # Default fallback, adjust if needed (e.g., stop with an error)
        chosen_family <- gaussian()
        # Or: stop("Unknown variable encountered: ", current_var)
      }
      # --- End of family selection ---
      
      # Define the value column name 
      response_col <- "value" 
      
      # Construct the formula dynamically
      gam_formula <- as.formula(paste(response_col, "~ s(year, bs = 'cr')"))
      
      # Fit the GAM using the data and family for THIS row
      gam(gam_formula,
          family = chosen_family,
          method = "REML",
          data = current_df)
    })
  )

# Now check the results or proceed with diagnostics
print(over_time_long_colour_gams_testing_nested_df_out)
map(over_time_long_colour_gams_testing_nested_df_out$mod_gam, summary)
map2(over_time_long_colour_gams_testing_nested_df_out$mod_gam, 
    over_time_long_colour_gams_testing_nested_df_out$variable,
    ~gratia::appraise(.x) + plot_annotation(title = .y))
map(over_time_long_colour_gams_testing_nested_df_out$mod_gam, gam.check)


# ---  switch to brms ---------------------------------
library(brms)
library(dplyr)
library(purrr)

# --- Set options for brms (optional, can help speed up) ---
options(mc.cores = parallel::detectCores()) # Use all available CPU cores

# --- the nested df is 'over_time_long_colour_gams_testing_nested_df' 
# with list-column 'data' and character column 'variable'

results_brms <- over_time_long_colour_gams_testing_nested_df %>%
  mutate(
    # Create a new column for brms models to avoid overwriting gam models if needed
    mod_brms = map2(data, variable, ~{
      current_df <- .x       # The dataframe for the current row
      current_var <- .y      # The variable name for the current row
      response_col <- "value" #  this matches the response column name in nested dfs
      
      message(paste("Fitting brms model for:", current_var)) # Progress message
      
      # --- 1. Choose the brms family based on the variable name ---
      chosen_family <- if (current_var == "N. of authors") {
        negbinomial() # For count data
      } else if (current_var == "N. of pages") {
        negbinomial() # For count data
      } else if (current_var == "Recency of references") {
        zero_one_inflated_beta() # For 0/1 inflated proportions
      } else if (current_var == "Diversity of references") {
        gaussian() # For continuous, approx normal data
      } else if (current_var == "Relative title length (ln)") {
        gaussian() # For continuous, normal data
      } else {
        warning(paste("Unknown variable:", current_var, "- using Gaussian default."))
        gaussian() # Default fallback
      }
      
      # --- 2. Construct the formula using brms::bf() ---
      # Handle the special structure for Zero-One Inflated Beta
      if (inherits(chosen_family, "zero_one_inflated_beta")) {
        # Formula for the mean (between 0 and 1), plus formulas for
        # zoi (zero-inflation probability) and coi (one-inflation probability).
        # zoi and coi are modelled simply as intercepts.
        brm_formula <- bf(
          paste(response_col, "~ s(year, bs = 'cr')"), 
          # Smooth term for mean
          zoi ~ 1,                                    
          # Intercept for prob of 0
          coi ~ 1                                     
          # Intercept for prob of 1
        )
      } else {
        # Standard formula for other families
        brm_formula <- bf(
          paste(response_col, "~ s(year, bs = 'cr')")
        )
      }
      
      # --- 3. Fit the brms model ---
      # Using tryCatch to prevent one failure from stopping everything
      tryCatch({
        brm(
          formula = brm_formula,
          family = chosen_family,
          data = current_df,
          # --- Stan control arguments (adjust as needed) ---
          cores = 4,  # Number of cores for parallel chains 
          chains = 4, # Number of Markov chains (4 is standard for final runs)
          iter = 2500, # Total iterations per chain (includes warmup)
          warmup = 1000,# Number of warmup iterations (discarded)
          control = list(adapt_delta = 0.999), # for helping convergence for complex models
          seed = 123, # For reproducibility
          silent = 2, # Suppress compilation messages from Stan
          refresh = 0 # Suppress iteration progress updates (can set to e.g., 500)
          
        )
      }, error = function(e) {
        # Print a warning if a model fails to fit
        warning(paste("brms fitting failed for variable:", 
                      current_var, "\nError:", e$message))
        return(NULL) # Return NULL for failed models
      })
    }) # End of map2
  ) # End of mutate

# --- Inspect the results ---
# Check to confirm all models fitted successfully
# results_brms

# Look at the summaries
library(broom.mixed)
map_df(results_brms$mod_brms,
   ~tidy(.x, 
         effects = "fixed",
         conf.int = TRUE))

results_brms$mod_brms %>% 
  map(~ posterior_summary(.x) %>% 
        as.data.frame() %>%
        tibble::rownames_to_column("term"))

# Posterior predictive check 
pp_check_outputs <- 
  map2(results_brms$mod_brms, 
       results_brms$variable,
      ~pp_check(.x, ndraws = 100) +
        ggtitle(.y))
plot_grid(plotlist = pp_check_outputs)
#  looks generally very good across all five models

# Plot the smooth term
conditional_smooths_outputs <- 
  map2(results_brms$mod_brms,
       results_brms$variable,
      ~plot(conditional_smooths(.x))[[1]] +
        ggtitle(.y) )
plot_grid(plotlist = conditional_smooths_outputs)

 # nice table of Model Evaluation metrics
model_list <- results_brms$mod_brms
names(model_list) <- results_brms$variable

model_eval_df <- map_dfr(names(model_list), function(name) {
  model <- model_list[[name]]
  
  summ <- summary(model)
  loo_result <- loo(model)
  
  tibble(
    model = name,
    `Max R-hat` = max(summ$rhats, na.rm = TRUE),
    `Min Bulk ESS` = min(summ$neff$bulk, na.rm = TRUE),
    LOOIC = loo_result$estimates["looic", "Estimate"],
    `ELPD (LOO)` = loo_result$estimates["elpd_loo", "Estimate"],
    `Effective Params` = loo_result$estimates["p_loo", "Estimate"]
  )
})

sampler_diagnostics <- function(model) {
  post_warmup <- as_draws_df(model)
  tibble(
    `Divergent Transitions` = sum(post_warmup$.divergent__, na.rm = TRUE),
    `Max Tree Depth` = max(post_warmup$.treedepth__, na.rm = TRUE)
  )
}

model_eval_df <- model_eval_df %>%
  bind_cols(
    map_dfr(model_list, sampler_diagnostics)
  )




#
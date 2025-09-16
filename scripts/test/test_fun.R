library(tidyverse)


#First, we’ll use list.files() to get a vector of filenames. 
#Then, we’ll pass that to map

contribution_files <- list.files(here::here("data/original/"), pattern = "17_26.csv", full.names = TRUE)

contribution_data <- map(contribution_files,
                         function(x) read_delim(x, delim=","))

#More Compact Way
contribution_data <- list.files(here::here("data/original/"), 
                                pattern = "17_26.csv", 
                                full.names = TRUE) %>%  
  map(.,
      function(x) read_delim(x, delim=","))

# or with a .csv
contribution_data <- list.files(here::here("data/original/"), 
                                pattern = "17_26.csv", 
                                full.names = TRUE) %>%  
  map(., read_csv)

#Track data w/ Function in map
contribution_data <- list.files(here::here("data/original/"), 
                                pattern = "17_26.csv", 
                                full.names = TRUE) %>%  
  map(~ read_csv(.x) %>%
        mutate(representative = str_extract(basename(.x), "^[^_]+(?=_fec_)"))
  )


#Narrow using formula for "is donor type individual"
select_ind_donors <- function(df, vars, donor_type) {
  df %>%
    select(all_of(vars)) %>%
    filter(is_individual == donor_type)
}

#Select Variables
select_vars <- c("is_individual", "contributor_name",  "contribution_receipt_amount", "committee_name", "report_year",
                 "representative")

ind_donors <- map(contribution_data, 
                  \(x) select_ind_donors(df = x,
                                         vars = select_vars,
                                         donor_type = TRUE))

#Individual Donors + Bind Rows
ind_donors <- map(contribution_data, 
                  \(x) select_ind_donors(df = x,
                                         vars = select_vars,
                                         donor_type = TRUE)) %>% 
  bind_rows()
###
find_top_n <- function(df, group_col, rank_col, value_col, n) {
  df %>%
    group_by({{ group_col }}, {{ rank_col }}) %>%   # Group by the primary grouping column
    summarise(total = sum({{ value_col }}, na.rm = TRUE), .groups = "drop") %>%  # Summarize the numeric column you're ranking by
    group_by({{ group_col }}) %>%   # Regroup only by the primary group for ranking
    slice_max(order_by = total, n = n) %>%   # Take the top N rows within each group based on the total value
    ungroup()    # Remove grouping so future operations treat the data as ungrouped
}

plot_top_donors_by_rep <- function(top_donors_df) {
  reps <- unique(top_donors_df$representative)   # Get list of unique representatives
  plot_list <- list()   # Create an empty list to store plots
  
  # Loop through each representative
  for (rep in reps) {
    df_rep <- filter(top_donors_df, representative == rep)  # Filter for that representative
    
    # Plot (no fct_reorder, just raw x labels)
    p <- ggplot(df_rep, aes(x = contributor_name, y = total))
    labs(
      title = paste("Top 5 Donors for", str_to_title(rep)),
      x = "Donor",
      y = "Total Contributions ($)"
    ) 
    coord_flip() +
      theme_minimal()

    plot_list[[rep]] <- p
  }
  
  return(plot_list)
}


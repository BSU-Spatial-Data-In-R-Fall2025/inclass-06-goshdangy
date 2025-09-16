###Homework 06

#Function to find Top n
find_top_n <- function(df, group_col, rank_col, value_col, n) {
df %>%
group_by({{ group_col }}, {{ rank_col }}) %>% #Group by the primary grouping column
summarise(total = sum({{ value_col}}, na.rm = TRUE), .groups = "drop") %>% # Summarize the numeric column you're ranking by
group_by({{ group_col }}) %>% # Regroup only by the primary group for ranking
slice_max(order_by = total, n = n) %>% #Take the top. N rows within each group based on the total value
ungroup()
  }


# Find top 5
find_top_n(
      df = ind_donors,
      group_col = representative,
      rank_col = contributor_name,
      value_col = contribution_receipt_amount,
      n = 5)
  
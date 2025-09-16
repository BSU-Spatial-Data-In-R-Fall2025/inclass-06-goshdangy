##ggplot for top 5 Donors

#Function for Top 5 Donors
plot_top_donors <- function(df) {
  donor_split <- split(df, df$representative)
  
  map(donor_split, function(rep_df) {
    ggplot(rep_df, aes(x = reorder(contributor_name, total), y = total)) +
      geom_col(fill = "red") +
      coord_flip() +
      labs(
        title = paste("Top 5 Donors -", str_to_title(rep_df$representative[1])),
        x = "Donor Name",
        y = "Amount Given ($)"
      ) +
      theme_minimal()
  })
}

#plug our data
rep_plots <- plot_top_donors(top5_donors)

# View Fulcer's plot
# Can use following Names to Check ("Simpson", "Fulcher", "Risch", "Crapo")
rep_plots[["fulcher"]]

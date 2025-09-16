##ggplot for top 5 Donors

#Function for Top 5 Donors

plot_top_donors <- function(df, fill_color = "red", plot_theme = theme_linedraw()) {
  df %>%
    split(.$representative) %>%  # Splitting the data into a list of tibbles, one per representative
   
    # For each representative's data frame, create a plot 
    map(~ {
      max_val <- max(df$total, na.rm = TRUE) + 1000
      ggplot(.x, aes(
        x = reorder(contributor_name, total), y = total)) +  # Reorder bars by total amount
        geom_col(fill = fill_color) +  #User can define Color or default red
        scale_y_continuous(limits = c(0, max_val))+
        labs(
          title = paste("Top 5 Donors -", str_to_title(unique(.x$representative))),
          x = "Donor Name",
          y = "Amount Given ($)"
        ) +
        plot_theme
    })
}


#plug our data
rep_plots <- plot_top_donors(top5_donors, "violet")

# View Fulcer's plot; Can use following Names to Check ("simpson", "fulcher", "risch", "crapo")
rep_plots[["risch"]]

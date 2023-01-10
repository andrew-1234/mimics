#--------------------------------------------
# Plotting to help visualise overlaps and how they are handled
remove_repeated_df1 <- read.csv("tests/testthat/testdata/remove_repeated_df1.csv")
# convert practice dataset to long format based on the start and end columns
practice_plot <- practice %>%
  pivot_longer(
    cols = c(Start, End),
    names_to = "Start_End",
    values_to = "Value"
  )

# plot the long format data
ggplot2::ggplot(data = practice_plot, ggplot2::aes(x = Value, y = instance, color = as.character(id), group = instance)) +
  ggplot2::geom_line(size = 30) +
  # ggplot2::facet_wrap(~Name, ncol = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
  ggplot2::labs(x = "asdf", y = "asdf", color = "")

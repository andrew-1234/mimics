# This should be broken down into smaller functions
# It does two things: combines data, and generates plots
# It takes data_indices_all as input, which is the data frame returned by the
# function time_series

# Function 1: complete_time_series
# This function returns the complete time series per site per month

# Function 2: plot_time_series
# This function plots the time series per site per month

# Function 3: hime_series / hime_indices
# This function combines the indices time series with the hime motif output
# This function needs to be broken down into smaller functions

####################################################################

# Function 1: complete_time_series
# Need 1 dataframe, or 1 plot, per point per month
# I think each plot will have all three indices
# Visualise with a flow chart

# Step 1: split the data into sites and months
split_data <- split(
  my_indices_data,
  interaction(my_indices_data$site, my_indices_data$month),
  drop = TRUE
)

names(my_indices_data)

# Step 2: for each data frame, add position column
split_data_ts <- lapply(
  split_data,
  function(x) {
    x %>%
      dplyr::mutate(., position = seq_len(nrow(.)))
  }
)

####################################################################

# Function 2: plot_time_series using the split_data_ts
test_df <- split_data_ts$`Bon-Bon-Station-Dry-A.Dec`

# Step 1: prepare the dataframe for plotting
# define the indices
total_indices <-
  c("AcousticComplexity", "EventsPerSecond", "TemporalEntropy")

# test: pivot the data (on single df)
plot_ts <- test_df %>%
  dplyr::select(position, dplyr::all_of(total_indices)) %>%
  tidyr::pivot_longer(.,
    cols = 2:4,
    names_to = "index",
    values_to = "value"
  )

# real: pivot the data (on list)
split_data_ts_pivot <- lapply(split_data_ts, function(x) {
  x %>%
    dplyr::select(position, dplyr::all_of(total_indices)) %>%
    tidyr::pivot_longer(.,
      cols = 2:4,
      names_to = "index",
      values_to = "value"
    )
})

# test that value column is numeric
test_that("Value is numeric", {
  expect_true(is.numeric(plot_ts$value))
})

test_that("Value is numeric", {
  lapply(split_data_ts_pivot, function(x) {
    expect_true(is.numeric(x$value))
  })
})

# test that index column contains three indices
test_that("index column contains three indices", {
  expect_equal(length(unique(plot_ts$index)), 3)
})

test_that("index column contains three indices", {
  lapply(split_data_ts_pivot, function(x) {
    expect_equal(length(unique(x$index)), 3)
  })
})

# define a function that plots the time series
plot_ts <- function(x, name) {
  ggplot2::ggplot(x, ggplot2::aes(x = position, y = value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(. ~ index) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(name)
}
# use lapply to plot each of the time series dfs
lapply(split_data_ts_pivot,
  plot_ts,
  name = names(split_data_ts_pivot)
)
# get element names
ele_names <- names(split_data_ts_pivot)

# make names suitable for file naming use
ele_names <- gsub("\\.", "_", ele_names)

# add the suffix now
ele_names <- paste(ele_names, "indicespertime.jpg", sep = "_")

# loop through each element and plot
# using a loop doesn't return plots, but when lapply is used it does
# need to specify print(plot) to return a plot to environment
# set the pathid
pathid <- "output/figures"
# add a setting to return plots to environment
# add a setting to turn plot title on or off

# working loop
for (i in seq_along(split_data_ts_pivot)) {
  plot <- plot_ts(split_data_ts_pivot[[i]], ele_names[i])
  ggplot2::ggsave(file.path(pathid, ele_names[i]))
}

# test that output/figures contains the right number of plots
# not a good test because output/figures can contain lots of things
test_that("output/figures contains the right number of plots", {
  expect_equal(length(list.files(pathid)), length(split_data_ts_pivot))
})

# Function 3: hime_series / hime_indices
# This function combines the indices time series with the hime motif output
# This function needs to be broken down into smaller functions

# Step 1: add the columns needed
# split_data_ts
test <- split_data_ts$`Bon-Bon-Station-Dry-A.Dec`

names(test)

# ✖ Column `AcousticComplexity` doesn't exist.
# why not??
# test that data in split_data_ts contains all indices columns
test_that("data in split_data_ts contains all indices columns", {
  lapply(split_data_ts, function(x) {
    expect_true(all(total_indices %in% names(x)))
  })
})

names(split_data_ts$`Bon-Bon-Station-Dry-A.Dec`)

for (i in split_data_ts) {
  print("hello")
  print(names(i))
}

for(index in total_indices) {
  print(index)
  test2 <- test %>%
  dplyr::select(index, 4:ncol(test))
  print(names(test2))
}

  test <- test %>%
    dplyr::mutate(., motif = NA) %>%
    dplyr::mutate(., distance = NA) %>%
    dplyr::mutate(., length = NA) %>%
    dplyr::mutate(., reference = "0_ts") %>%
    dplyr::mutate(., id = 0) %>%
    dplyr::rename(., Index = index)
}
test %>%
  dplyr::mutate(., motif = NA) %>%
  dplyr::mutate(., distance = NA) %>%
  dplyr::mutate(., length = NA) %>%
  dplyr::mutate(., reference = "0_ts") %>%
  dplyr::mutate(., id = 0) %>%
  dplyr::rename(., Index = index)

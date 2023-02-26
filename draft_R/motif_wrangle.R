motif_wrangle <- function(motif_list) {
  motif_list <- motif_results_rename_cols(motif_list)
  motif_list <- motif_results_add_id_column_run(motif_list)
  motif_list <- motif_pivot_longer(motif_list)
  motif_list <- rename_motif_match(motif_list)
  motif_list <- separate_instance_column(motif_list)
  motif_list <- motif_pivot_wider(motif_list)
  motif_list <- add_instance_id(motif_list)
  return(motif_list)
}

motif_results_rename_cols <- function(motif_results) {
  # for each dataframe, rename the columns
  motif_results_2 <- list()
  motif_results_2 <- lapply(motif_results, function(x) {
    x <- x %>%
      dplyr::rename(
        .,
        FirstInstance_Start = V1,
        FirstInstance_End = V2,
        SecondInstance_Start = V3,
        SecondInstance_End = V4,
        Length = V5,
        Distance = V6
      )
  })
  return(motif_results_2)
}

motif_results_add_id_column_run <- function(motif_results) {
  motif_results_new <- list()
  motif_results_new <- lapply(motif_results, motif_results_add_id_column)
  return(motif_results_new)
}

motif_results_add_id_column <- function(df) {
  df_2 <- df %>%
    dplyr::mutate(., id = 1:as.numeric(dplyr::count(.)))

  df_3 <- df_2 %>%
    dplyr::filter(., Distance <= 5)

  # test that distance column is <= 5
  test_that("distance column is <= 5", {
    expect_true(all(df_3$Distance <= 5))
  })
  return(df_3)
}

motif_pivot_longer <- function(motif_list) {
  lapply(motif_list, function(x) {
    x <- x %>%
      tidyr::pivot_longer(.,
        cols = 1:4,
        names_to = "Instance",
        values_to = "position"
      )
  })
}

rename_motif_match <- function(motif_list) {
  lapply(motif_list, function(x) {
    x <- x %>%
      dplyr::mutate(.,
        Instance = gsub(
          pattern = "FirstInstance",
          replacement = "motif",
          x = Instance
        )
      ) %>%
      dplyr::mutate(.,
        Instance = gsub(
          pattern = "SecondInstance",
          replacement = "match",
          x = Instance
        )
      )
  })
}

separate_instance_column <- function(motif_list) {
  lapply(motif_list, function(x) {
    x <- x %>%
      tidyr::separate(.,
        Instance,
        into = c("instance", "moment"),
        sep = "_"
      )
  })
}

motif_pivot_wider <- function(motif_list) {
  lapply(motif_list, function(x) {
    x <- x %>%
      tidyr::pivot_wider(.,
        names_from = moment,
        values_from = position
      )
  })
}

add_instance_id <- function(motif_list) {
  lapply(motif_list, function(x) {
    x <- x %>%
      dplyr::mutate(.,
        instance = paste(id, instance, sep = "_")
      )
    if (nrow(x) > 0) {
      x <- x %>%
        dplyr::arrange(., Start, End) %>%
        dplyr::mutate(., overlap = NA)
    }
  })
}

save_final_ts <- function(dfs_motif, himepath) {
  invisible(
    lapply(dfs_motif,
      create_file_name_motif_csv,
      himepath = himepath
    )
  )
}

create_file_name_motif_csv <- function(df, himepath) {
  month_id <- unique(df$month)
  site_id <- unique(df$site)
  index_id <- names(df)[1]
  file_id <- paste0(
    "Motif_",
    index_id, "_",
    month_id, "_",
    site_id,
    ".csv"
  )

  write.csv(
    x = df,
    file = file.path(
      get_data_path(himepath),
      file_id
    ),
    row.names = F
  )
}

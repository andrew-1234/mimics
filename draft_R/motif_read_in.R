# function to create a list of matching hime file names
# and return a list of hime files
# calls hime_file_is

read_hime_files <- function(df) {
  hime_files <- list()
  hime_files <- lapply(df, hime_file_is)
  motif_results <- list()
  motif_results <- lapply(hime_files, function(x) {
    utils::read.table(x, row.names = NULL)
  })
  return(motif_results)
}

# called in read_hime_files
# create a function that generates a path to matching hime file for each data
# frame in dfs_motif
hime_file_is <- function(df) {
  # unique months
  month_id <- unique(df$month)

  # TODO unique site id (from name, from column, or from metadata table?)
  site_id <- unique(df$site)

  # Index ID, index should always be in position 1
  index_id <- names(df)[1]

  # generate the file ID
  file_id <- paste0("Res_TS_", index_id, "_", month_id, "_", site_id, ".txt")

  # where is the hime data sotred?
  # TODO: double check that hime-clean will always be the right folder
  himepath <- "output/hime-clean"

  # use full path to hime file
  full_path <- get_data_path(himepath, file_id)

  # check file exists
  tryCatch(test_that("hime file exists", {
    expect_true(file.exists(full_path))
  }), error = function(e) {
    message(paste0("🚨 hime file does not exist: \n", full_path))
  })

  return(full_path)
}

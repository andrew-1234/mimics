output <- system2("pwsh", "scripts/summary.ps1", stdout = TRUE)

# TODO this is in progress

output2 <- system2("pwsh", "scripts/test.ps1", stdout = TRUE)

# Store the output in a table
table <- read.table(text = output, header = FALSE)
a_test <- tibble(output) %>%
    separate(
        col = 1,
        into = c("duration", "start_time"),
        sep = ","
    )

a_test$start_time <- a_test$start_time %>% trimws()

# Split to date time etc.
a_test_2 <- a_test %>%
    separate(start_time, into = c("date", "time"), sep = "T") %>%
    separate(time, into = c("hour", "offset"), sep = "\\+")

# calcualte summary statistics for duration
a_test_2$duration <- as.numeric(a_test_2$duration)

output_1 <- a_test_2$duration %>% summary()

output_1[1]

# Function to explain the minimum, maximum and mean duration of the files
explain_file_data_summary <- function(summary) {
    # Extract the number of distinct files and directories

    # Create the explanation
    # i could store these in separate strings to format better with new lines
    # without having to use cat. this would be better for the shiny app.

    cat(paste0(
        "The minimum duration was ", summary[1], " seconds, ",
        "while the maximum duration was ", summary[6], " seconds.", "\n",
        "The mean file duration was ", summary[4], " seconds"
    ))
}

explain_file_data_summary(output_1)

# Function to explain how many files in how many directories
length(a_test_2$duration)

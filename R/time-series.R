site_id <- NULL
point_id <- NULL
date_time_id <- NULL
month_id <- NULL
geo_id <- NULL

# set the folder with indices
folder <- "data/indices-output/"

# get a list the files with indices results
# there should be one file per audio recording

files <- list_myfiles(search_pattern = ".Indices.csv")

# subset to change the for loop
#file <- files[1]

# generate metadata based on folder structure
# generate metadata from api call to a2O??
# support using a metadata table as input
# point id can be 1 if only one sensor per site
# TODO: need point id or later parts of scripts might be annoying to configure



# isntead of writing to csvs, just read each iteration, rbind into a new dataframe?
# TODO: this is working, but need to integrate ability to specify a lookup table, else attempt to create metadata from file names (if A2O data or similar formatting)
# TODO: refactor this code
data_indices_all <- NULL
for (file in files) {

        file_name_only_3 <- basename(file)
        # get geographic ID
        u <- strsplit(file_name_only_3, split = "_")
        u2 <- unlist(u)
        u2[1]
        geo_id <- u2[2]
        geo_id <- unique(geo_id)

        # get date_time_ID
        date_time_id <- u2[1]

        # get month ID
        library(lubridate)
        month_1 <- strsplit(date_time_id, split = "[+]") #strsplit is regex so need []
        month_2 <- unlist(month_1)
        month_3 <- month_2[1]
        month_4 <- ymd_hms(month_3)
        month_5 <- month(month_4, label = TRUE, abbr = TRUE)

        library(tidyr)
        library(dplyr)
        indices <- c("AcousticComplexity", "EventsPerSecond", "TemporalEntropy")

        data_indices <- read.csv(file) %>%
                mutate(FID = paste(basename(file), ResultMinute, sep = "_")) %>%
                separate(
                        .,
                        FID,
                        into = c("date_time", "site", "audioID"),
                        sep = "_",
                        remove = F
                ) %>%
                separate(
                        .,
                        date_time,
                        into = c("date", "time"),
                        sep = "T",
                        remove = F
                ) %>%
                separate(
                        .,
                        time,
                        into = c("time", "offset"),
                        sep = "[+]",
                        remove = T
                ) %>%
                #mutate(., site = t[[1]][3]) %>%
                #mutate(., point = t[[1]][4]) %>%
                mutate(., filepath = file) %>%
                # TODO: add the original audio file path?
                mutate(., month = month_5) %>%
                select(
                        .,
                        AcousticComplexity,
                        EventsPerSecond,
                        TemporalEntropy,
                        FileName,
                        date_time,
                        month,
                        date,
                        time,
                        ResultMinute,
                        FID,
                        site,
                        #point,
                        filepath
                ) %>%
                # TODO: select based on names instead of index
                mutate_at(vars(all_of(indices)), scale) %>%
                with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),])
        data_indices_all <- rbind(data_indices_all, data_indices)
}
# unique(data_indices_all$FID)
# TODO maybe remove the __Towsey.Acoustic.Indices suffix to clean up names

# next we run the motif analysis based on a geographic and month subset

# loop and make big df with all
# subset into geo and month id
# drop columns and get 1 df per index
# run hime per month, per geo, per index - keep DF as is and when doing hime, subset what you want and then run (but it has to be ordered - ordering files function - date, time, result minutes)
# hime takes .txt files
# hime runs on pwsh from R
# have to save the results files from hime
# can output everything in same directory
# set seed - when randomizing the labels etc for reproducible example important to set seed
# if they want to try follow they should get exactly same result, for RF and everything.

# ---- subset the DF
# The output here should be: subset by geo/site ID (each sensor location), by month, by indice
# total DFs = 3 * months * sensor locations
# so I should have 6 data frames

# subset the data

# prepare the grouping values
total_sites <- unique(data_indices_all$site)
total_months <- unique(data_indices_all$month)
length_total_months <- length(total_months)

# i think i could do this better
# separate columns into indices and other. and then for x in indices add to other and output into a list

cols_all <- colnames(data_indices_all)
AC <- cols_all[!cols_all %in% c('EventsPerSecond', 'TemporalEntropy')]
EV <- cols_all[!cols_all %in% c('AcousticComplexity', 'TemporalEntropy')]
TE <- cols_all[!cols_all %in% c('EventsPerSecond', 'AcousticComplexity')]
indices_subset_list <- list(AC, EV, TE)
# get it working for sites and indices first. then add month functionality

# update the output folder location to save the TS data
timeseries_input <- get_data_path("data/timeseries-input/")

# create the directory if it doesn't exist
if (!dir.exists(timeseries_input)) {
        dir.create(timeseries_input)
}

# this subsets into each index / site / month
# TODO: need a unique ID row?? result minute and FID is not unique I think
# TODO: fix this loop to match time_processing_cont (start with the for calls one after another and check that it works)

for (x in indices_subset_list) {
        temp <- NULL
        temp2 <- NULL
        # subset by indices
        temp <- data_indices_all %>% select(all_of(unlist(x)))
                # subset by site
                for (sites in total_sites) {
                     temp2 <- temp %>% filter(site == sites)
                     # TODO: subset by site (UNTESTED WITH MULTIPLE MONTHS )
                     for (month_id in total_months) {
                             temp3 <- temp2 %>% filter(month == month_id) %>%
                             glimpse()

                             # TODO: sorting untested on larger date and time ranges
                             temp4 <- temp3 %>% dplyr::arrange(date_time, ResultMinute)

                             # create file ID
                             file_id_new <- paste(c("TS_",
                                     x[1], # gets the indices name
                                     "_",
                                     as.character(month_id), # get month name
                                     "_",
                                     sites), # get site name
                                   collapse = "")
                             print(file_id_new)

                             # write a csv (just in case for now)
                             # TODO: maybe put them in a different directory
                             write.csv(
                                        x = temp4,
                                        file = paste(c(
                                               timeseries_input,
                                               file_id_new,
                                               ".csv"),
                                               collapse = ""))

                             write.table(
                                     x = temp4[1], # just need the indices values for hime
                                     file = paste(c(
                                             timeseries_input,
                                             file_id_new, ".txt"),
                                             collapse = ""),
                                     row.names = F,
                                     col.names = F
                             )
                     }
                }
}







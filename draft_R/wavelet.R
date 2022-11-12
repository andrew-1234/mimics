library(magick)

total_sites <- unique(data_indices_all$site)

#subset to test loop
#total_sites <- total_sites[1]
#site <- total_sites
# file <- files[1]
# loop over sites
# from now on all months will be joined, only sites are separated
setwd("Documents/Data Science/Projects/Ecoacoustics/motifR/")
# this will call at the start of the function, followed by the for loop
motif_complete <- NULL
file_result <- NULL
img_prep <- NULL
ts_data <- NULL
ts_list <- NULL

# construct the data frame----
motif_complete <- data.frame(
        position =	integer(),
        index_value = numeric(),
        FileName = factor(),
        date = integer(),
        time = integer(),
        ResultMinute = integer(),
        FID = character(),
        distance = numeric(),
        length = integer(),
        reference = character(),
        id	= character(),
        fid_what = factor(),
        site = factor(),
        point = factor(),
        date_time = factor()
)

# import and bind together motif csv files ----
for (site in total_sites) {
# list the files in motif csv files in output/hime for each site
        files <-
                list.files(
                        get_data_path("output/hime"),
                        pattern = glob2rx(paste("Motif*", site, ".csv", sep = "")),
                        recursive = T,
                        full.names = T)
        for (file in files) { # for each file (one for each index / month)
                file_result <- read.csv(file) %>%
                        dplyr::filter(., motif != is.na(T)) %>%
                        dplyr::rename(., fid_what = motif) %>%
                        dplyr::rename(., index_value = Index) %>%
                        dplyr::mutate(.,
                                      id = paste(
                                              basename(file) %>%
                                                      gsub(pattern = "Motif_", replacement = "") %>%
                                                      gsub(pattern = "*.csv*", replacement = "") %>%
                                                      gsub(pattern = "TemporalEntropy", replacement = "ENT") %>%
                                                      gsub(pattern = "AcousticComplexity", replacement = "ACI") %>%
                                                      gsub(pattern = "EventsPerSecond", replacement = "EVN"),
                                              fid_what,
                                              sep = "_"
                                      )) %>%
                        dplyr::select(
                                .,
                                position,
                                index_value,
                                FileName,
                                date,
                                time,
                                ResultMinute,
                                distance,
                                length,
                                reference,
                                id,
                                fid_what,
                                site,
                                #point,
                                date_time
                        )

        motif_complete <- rbind(motif_complete, file_result) %>%
                filter(.$index_value != "NA")
        # running code up to here should generate a motif complete file with all sites and all 3 indices?
        # after that, we shouldn't need the site loop?
        # then we can have another loop that creates the output folders needed
        }
}

# prepare the img data table----
img_prep <- separate(
                motif_complete,
                id,
                into = c("index_name",
                         #"point",
                         "month",
                         "site",
                         "motif_number",
                         "what"),
                sep = "_",
                remove = F
        ) %>%
                group_by(., id) %>%
                mutate(., new_position = order(order(position))) %>%
                ungroup(.) %>%
                select(everything(),-c(position)) %>%
                group_by(id) %>%
                filter(ResultMinute == min(ResultMinute)) # what does this do? makes the df small

# TODO: think of a name for this folder
# create output/rename folder if doesn't exist
output_rename <- get_data_path("output/rename")
if (!dir.exists(output_rename)) {
        dir.create(output_rename)
}

# define unique site folders for output
# create a directory for each site if doesn't exist


# incorporate site/point sorting above if necessary in future. ATM just using site IDs.


# paste(img_prep$FileName[row], "__", img_prep$index_name[row], ".png", sep = "")
# for some reason this didn't work
# _AAO_-27.3888+152.8808 was an error (pre-emu) so added to string for allowing to read in
# supply path to AP outputs
# when saving these images keep one folder per site. makes labeling images
# ideas get file list and search for X in file list pattern match
# TODO: atm this only reads in __ENT files? should loop for index in indices
# row <- 1

siteIDs <- unique(img_prep$site)
indices_string <- c("__ENT.png", "__ACI.png", "__EVN.png")
siteID <- siteIDs[2]
index_string <- indices_string[2]
for (siteID in siteIDs) {
        site_folder <- paste(c(output_rename, "/", siteID), collapse = "")
        print(site_folder)
        if (!dir.exists(site_folder)) {
                dir.create(site_folder)
        }

        img_prep_site <- img_prep %>% filter(site == siteID)

        for (index_string in indices_string) {
                image_list <- list.files(get_data_path(path = "data/indices-output/"),
                                         pattern = index_string, recursive = T, full.names = T)

                #the problem i think was the img_prep_site had all rows with different indices and
                # image would read for each row and probably give wrong label

                indices_name_only <- gsub("__|.png", "", index_string)

                # need to filter for each index, and then run through each row and save images.
                img_prep_site_index <- img_prep_site %>% filter(index_name == indices_name_only)
                # read images row by row for img_prep dataframe
                for (row in 1:nrow(img_prep_site_index)) {

                        pattern_match_image_list <-
                                paste(img_prep_site_index$date_time[row],
                                      "_",
                                      img_prep_site_index$site[row],
                                      sep = "")

                        pattern_match_image_list <- gsub("\\+", "\\\\+", x = pattern_match_image_list)

                        # this should match ONE image only - exact date, time, site etc.
                        path_to_image <- grep(pattern = pattern_match_image_list, x = image_list, value = TRUE)

                        #stringr::str_detect(string = image_list, pattern = pattern_match_image_list)
                        #stringr::str_detect(string = image_list, pattern = "20210102T000000\\+1000")

                        # read the image and crop
                        image_read(path_to_image) %>%
                                image_crop(
                                        .,
                                        geometry_area(
                                                height = 256,
                                                width = img_prep_site_index$length[row] - (1 - img_prep_site$ResultMinute[row]),
                                                y_off = 20, # cropping offset don't touch
                                                x_off = img_prep_site_index$ResultMinute[row]
                                        )
                                ) %>%
                                # create a path to save the cropped images
                                magick::image_write(.,
                                            path = paste(
                                                    site_folder,
                                                    "/",
                                                    img_prep_site_index$site[row], # TODO: is the extra site tag redundant?
                                                    "_",
                                                    img_prep_site_index$id[row],
                                                    sep = ""
                                            ))

                }
        }

}




# subset by site, and for loop for site
sites <- unique(motif_complete$site)
siteID <- sites[1]
for (siteID in sites) {
        motif_complete_site <- motif_complete %>% filter(site == siteID)

        ts_data <- select(motif_complete_site, index_value, position, id) %>%
                group_by(., id) %>%
                mutate(., new_position = order(order(position))) %>%
                ungroup(.) %>%
                select(., everything(), -position) %>%
                tidyr::pivot_wider(., names_from = new_position, values_from = index_value) %>%
                as.data.frame(.)



        rownames(ts_data) <- ts_data$id
        ts_data <- ts_data[, 2:length(ts_data)]


        ts_list <- dtwclust::tslist(ts_data) %>%
                purrr::map(., na.omit)


        wtData <- NULL


        for (i in ts_list) {
                wt <- wavelets::dwt(i, filter = "haar", boundary = "periodic")

                un <- as.data.frame(t(unlist(c(wt@W, wt@V[[wt@level]]))))

                wtData <- plyr::rbind.fill(wtData, un)

        }

        wtData <- randomForest::na.roughfix(wtData)

        wtData$id <- rownames(ts_data)

        wtData <- mutate(wtData, class = NA) %>%
                mutate(., geophony = NA) %>%
                mutate(., technophony = NA) %>% # this had a pipe operator missing
        select(., id, class, geophony, technophony, everything())

        # pick 30 percent of the samples to label, without replacement
        samples <-
                sample(wtData$id, size = ceiling(nrow(wtData) * 0.30), replace = F)

        write.csv(wtData, get_data_path("output/step8",
                                        paste(siteID, "_", "_wavelet.csv", sep = "")), row.names = F)

        write.csv(samples, get_data_path("output/step8",
                                         paste(siteID, "_", "_LabelsSample.csv", sep = "")), row.names = F)

        write.csv(motif_complete_site,
                  get_data_path("output/step8",
                              paste(siteID, "motif_complete.csv", sep = "_")),
                  row.names = F)

}

# open labels sample
# then open wavelet
# TODO: in this example above, with a small sample size we only get ENT and  EVN images to label and no ACI. We should implement stratified samples with index class. This only effects small datasets. around line 259 (sample function).
# images when they are saved should appear the SAME as the output into labelsSample so it is easy.
# so open the image. at the start probably have to listen to the recordings as well until you can understand the patterns and visually ID based on the spectrogram. This example was geophony, then you add the labels in the wavelet csv. class: wind, geophony: yes, leave technophony as na. label in the right row.
# we are going to use the wavelet csv for the random forest. and it will use the labelled rows as training etc. you will run one random forest per site. because bg noise etc impacts the wavelets. not good to use different sites.
# sometimes you have to go back and label a few more of a class that you don't have as many. this part needs a lot of researcher input. so thats a no to the idea of generating a sheet that just has the rows to label. maybe have a row that has "label = yes" because then you open in excel and can filter by the rows you need to label. but keeping everything in the same file is easiest because if you have to go back after the random forest step you can clearly see what you labelled, what you didn't, and just do a bit more if you need to.
# the set seed is impportant because if you run the above code again it will tell you a different set of images that you need to label. this could stuff up your workflow.
# if you want to see the full image or listen to the sound, should have an easy way to do that

# TODO: there is a problem reading the images in. images are the same when they should be of different index.

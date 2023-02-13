for (geo in geo_id) {
        motif_complete <- NULL
        file_result <- NULL
        img_prep <- NULL
        ts_data <- NULL
        ts_list <- NULL


        motif_complete <- data.frame(
                position = integer(),
                index_value = numeric(),
                FileName = factor(),
                date = integer(),
                time = integer(),
                ResultMinute = integer(),
                FID = character(),
                distance = numeric(),
                length = integer(),
                reference = character(),
                id = character(),
                fid_what = factor(),
                site = factor(),
                point = factor(),
                date_time = factor()
        )

        files <-
                list_myfiles(step5,
                        search_pattern = glob2rx(paste(geo, "*motif.csv", sep = "_"))
                )

        for (file in files) {
                file_result <- read.csv(file) %>%
                        dplyr::filter(., motif != is.na(T)) %>%
                        dplyr::rename(., fid_what = motif) %>%
                        dplyr::rename(., index_value = Index) %>%
                        dplyr::mutate(.,
                                id = paste(
                                        basename(file) %>%
                                                gsub(pattern = "*_motif.csv", replacement = "") %>%
                                                gsub(pattern = "TemporalEntropy", replacement = "ENT") %>%
                                                gsub(pattern = "AcousticComplexity", replacement = "ACI") %>%
                                                gsub(pattern = "EventsPerSecond", replacement = "EVN"),
                                        fid_what,
                                        sep = "_"
                                )
                        ) %>%
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
                                point,
                                date_time
                        )
                motif_complete <- rbind(motif_complete, file_result) %>%
                        filter(.$index_value != "NA")
                ##### up to here

                img_prep <- separate(motif_complete,
                        id,
                        into = c("site", "point", "month", "index_name", "motif_number", "what"),
                        remove = F
                ) %>%
                        group_by(., id) %>%
                        mutate(., new_position = order(order(position))) %>%
                        ungroup(.) %>%
                        select(everything(), -c(position)) %>%
                        group_by(id) %>%
                        filter(ResultMinute == min(ResultMinute))

                dir.create(getDataPath(folder, step7, unique(img_prep$site)))
                dir.create(getDataPath(folder, step7, unique(img_prep$site), unique(img_prep$point)))

                # this is reading in images from the output of AP, one image per index per recording
                for (row in 1:nrow(img_prep)) {
                        image_read(list.files(getDataPath(folder, img_prep$site[row], img_prep$point[row], paste(img_prep$date[row], "_AAO_-27.3888+152.8808", sep = ""), paste(img_prep$date[row], "T", img_prep$time[row], "_REC_-27.3888+152.8808.flac", sep = "")), pattern = "__ENT.png", full.names = T, recursive = T)) %>%
                                image_crop(
                                        .,
                                        geometry_area(
                                                height = 256,
                                                width = img_prep$length[row] - (1 - img_prep$ResultMinute[row]),
                                                y_off = 20,
                                                x_off = img_prep$ResultMinute[row]
                                        )
                                ) %>%
                                image_write(., getDataPath(
                                        folder,
                                        step7,
                                        img_prep$site[row],
                                        img_prep$point[row],
                                        paste(img_prep$id[row],
                                                ".png",
                                                sep = ""
                                        )
                                ))
                }
        }

        ts_data <- select(motif_complete, index_value, position, id) %>%
                group_by(., id) %>%
                mutate(., new_position = order(order(position))) %>%
                ungroup(.) %>%
                select(., everything(), -position) %>%
                pivot_wider(., names_from = new_position, values_from = index_value) %>%
                as.data.frame(.)



        rownames(ts_data) <- ts_data$id
        ts_data <- ts_data[, 2:length(ts_data)]


        ts_list <- tslist(ts_data) %>%
                map(., na.omit)


        wtData <- NULL


        for (i in ts_list) {
                wt <- dwt(i, filter = "haar", boundary = "periodic")

                un <- as.data.frame(t(unlist(c(wt@W, wt@V[[wt@level]]))))

                wtData <- plyr::rbind.fill(wtData, un)
        }

        wtData <- na.roughfix(wtData)

        wtData$id <- rownames(ts_data)

        wtData <- mutate(wtData, class = NA) %>%
                mutate(., geo = NA) %>%
                mutate(., techno = NA)
        select(., id, class, geo, techno, everything())


        samples <- sample(wtData$id, size = ceiling(nrow(wtData) * 0.30), replace = F)

        write.csv(wtData, getDataPath(folder, step8, paste(geo, "_", "_wavelet.csv", sep = "")), row.names = F)
        write.csv(samples, getDataPath(folder, step8, paste(geo, "_", "_LabelsSample.csv", sep = "")), row.names = F)

        write.csv(motif_complete,
                getDataPath(
                        folder,
                        step6,
                        paste(geo, "motif_complete.csv", sep = "_")
                ),
                row.names = F
        )
}

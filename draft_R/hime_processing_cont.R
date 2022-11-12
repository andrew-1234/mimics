library(ggplot2)

# TODO: add a function to check for output/figures or else create that directory

# check past files, i already created an indices vector i think
total_indices <-
        c("AcousticComplexity", "EventsPerSecond", "TemporalEntropy")

# plot some data ----
# TODO: i think this should be one plot per subset (for geo and for month)

# subset by geo and by month

total_sites <- unique(data_indices_all$site)
total_months <- unique(data_indices_all$month)
#length_total_months <- length(total_months)

# create subsets to run practice loop
# total_sites <- total_sites[1]
# total_months <- total_months[1]
# total_indices <- total_indices[1]

# modify to use subset dataframes instead of reading in a new dataframes at the start of each loop
# this is the main and complete function ---- in PROGRESS ----
# try to get working so that one time series data frame is matched against one motif file
# remember that within each time series data frame there is data from multiple files i think, so the unique ids are not unique. i'm not sure how this affects the analysis. need to see the full workflow working to better understand.

# the first part of this loop subsets the indices dataframe and generates one plot per site, per month. total plots = points*months. 2 sites 1 month = 2 plots.
for (sites in total_sites) {
        for (month_id in total_months) {
                skip_to_next <- FALSE

                tryCatch({
                        complete_ts <- data_indices_all %>%
                                filter(site == sites) %>%
                                filter(month == month_id) %>%
                                mutate(., position = seq_len(nrow(.)))

                },

                error = function(e) {
                        skip_to_next <<- TRUE
                })

                if (skip_to_next) {
                        next

                }

                plot_ts <- complete_ts %>% select(position, all_of(total_indices)) %>%
                        pivot_longer(.,
                                     cols = 2:4,
                                     names_to = "index",
                                     values_to = "value")

                ggplot(plot_ts, aes(x = position, y = value)) +
                        geom_line() +
                        facet_wrap(. ~ index) +
                        theme_classic() +
                        theme(axis.text.x = element_blank())
                ggsave(get_data_path("output/figures", #TODO: update to allow custom path
                        paste(sites, month_id, "indicespertime.jpg", sep = "_")
                ))

# second part: joining hime data to indices data

                for (index in total_indices) {
                        complete_inter <-
                                select(complete_ts, all_of(index), 4:ncol(complete_ts)) %>%
                                mutate(., motif = NA) %>%
                                mutate(., distance = NA) %>%
                                mutate(., length = NA) %>%
                                mutate(., reference = "0_ts") %>%
                                mutate(., id = 0) %>%
                                rename(., Index = index)



                        # Processing results
                        hime_file_import_path <- get_data_path(... = "data/hime-cleaned",
                                                 paste("Res_TS_",
                                                       index, "_",
                                                       month_id, "_",
                                                       sites,
                                                       ".txt",
                                                       sep = ""))

                        motif_results <-
                                read.table(hime_file_import_path) %>%
                                rename(
                                        .,
                                        FirstInstance_Start = V1,
                                        FirstInstance_End = V2,
                                        SecondInstance_Start = V3,
                                        SecondInstance_End = V4,
                                        Length = V5,
                                        Distance = V6
                                ) %>%
                                mutate(., id = 1:as.numeric(count(.))) %>%
                                filter(., Distance <= 5) %>%
                                select(., id, everything()) %>%
                                pivot_longer(.,
                                             cols = 2:5,
                                             names_to = "Instance",
                                             values_to = "position") %>%
                                mutate(.,
                                       Instance = gsub(
                                               pattern = "FirstInstance",
                                               replacement = "motif",
                                               x = Instance
                                       )) %>%
                                mutate(.,
                                       Instance = gsub(
                                               pattern = "SecondInstance",
                                               replacement = "match",
                                               x = Instance
                                       )) %>%
                                separate(.,
                                         Instance,
                                         into = c("instance", "moment"),
                                         sep = "_") %>%
                                pivot_wider(., names_from = moment, values_from = position) %>%
                                mutate(., instance = paste(id, instance, sep = "_")) %>%
                                with(., .[order(Start), ]) %>%
                                mutate(., overlap = NA) %>%
                                remove_repeated(.)



                        for (row in 1:nrow(complete_inter)) {


                                skip_to_next <- FALSE

                                tryCatch({
                                        complete_inter[motif_results$Start[row]:motif_results$End[row], c("motif", "distance", "length")] <-
                                                motif_results[row, c("instance", "Distance", "Length")]
                                },

                                error = function(e) {
                                        skip_to_next <<- TRUE
                                })

                                if (skip_to_next) {
                                        next

                                }
                        }

                        complete_inter <-  group_by(complete_inter, motif) %>%
                                add_count(.) %>%
                                filter(n>=30) # TODO: check message here storing counts in nn

                        motif_csv <- get_data_path("output/hime", paste("Motif_",
                                                            index, "_",
                                                            month_id, "_",
                                                            sites,
                                                            ".csv",
                                                            sep = ""))
                        write.csv(complete_inter,
                                  motif_csv,
                                  row.names = F)

                        plot_ts <- complete_inter %>% #ungroup %>% # NOTE: i added ungroup, because motif is no longer being selected here and we don't want it added back in?
                                select(reference, position, Index, date, time) %>%
                                separate(.,
                                         reference,
                                         into = c("number", "what"),
                                         remove = F)

                        plot_motif <-
                                select(complete_inter, motif, position, Index) %>%
                                rename(., reference = motif) %>%
                                filter(reference != "NA") %>%
                                separate(.,
                                         reference,
                                         into = c("number", "what"),
                                         remove = F)


                        #6 in BNE = +10
                        # just for visual purposes adds a line at a time you want
                        # line_intercept1 <- filter(plot_ts, grepl("160000*", time)) %>%
                        #         .[!duplicated(.$date), ] %>%
                        #         mutate(time = 060000) %>%
                        #         select(time, position)

                        #18 in BNE = +10
                        # line_intercept2 <- filter(plot_ts, grepl("040000*", time)) %>%
                        #         .[!duplicated(.$date), ] %>%
                        #         mutate(time = 180000) %>%
                        #         select(time, position)

                        # missing what column
                        ggplot(plot_ts, aes(x = position, y = Index)) +
                                geom_line(aes(colour = reference, linetype = reference), colour = "grey") +
                                geom_vline(xintercept = line_intercept1$position, linetype = "dotted") +
                                # geom_text(data = line_intercept1,
                                #           aes(label = time, y = 10, size = 1),
                                #           check_overlap = T) +
                                # geom_vline(xintercept = line_intercept2$position, linetype = "dotted") +
                                # geom_text(data = line_intercept2,
                                #           aes(label = time, y = 10, size = 1),
                                #           check_overlap = T) +
                                scale_linetype_manual(values = "dotted") +
                                geom_line(data = plot_motif, aes(
                                        x = position,
                                        y = Index,
                                        colour = reference
                                )) +
                                scale_color_manual(values = c(replicate(nrow(
                                        motif_results
                                ), "#2ca25f"))) +
                                theme_classic() +
                                labs(title = paste(index, sep = " ")) +
                                theme(
                                        legend.title = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(),
                                        axis.ticks = element_blank(),
                                        legend.position = "none"
                                )

                        ggsave(get_data_path(
                                "output/figures", #TODO: update to allow custom path
                                paste(sites, month_id, index, "indicespertime.jpg", sep = "_")
                        ))


                }

        }
}



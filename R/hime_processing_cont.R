library(ggplot2)
# check past files, i already created an indices vector i think
indices <-
        c("AcousticComplexity", "EventsPerSecond", "TemporalEntropy")

# plot some data ----
# TODO: i think this should be one plot per subset (for geo and for month)

# subset by geo and by month

total_sites <- unique(data_indices_all$site)
total_months <- unique(data_indices_all$month)
length_total_months <- length(total_months)

# create subsets to run practice loop
total_sites <- total_sites[1]
total_months <- total_months[1]

# modify to use subset dataframes instead of reading in a new dataframes at the start of each loop
# this is the main and complete function ---- in PROGRESS ----
# try to get working so that one time series data frame is matched against one motif file
# remember that within each time series data frame there is data from multiple files i think, so the unique ids are not unique. i'm not sure how this affects the analysis. need to see the full workflow working to better understand.

for (sites in total_sites) {
        for (month_id in total_months) {
                skip_to_next <- FALSE

                tryCatch({
                        complete_ts <-
                                #TODO: stopped here. subset dataframe instead of reading %>%
                                mutate(., position = seq_len(nrow(.)))

                },

                error = function(e) {
                        skip_to_next <<- TRUE
                })

                if (skip_to_next) {
                        next

                }

                plot_ts <-
                        select(complete_ts, position, all_of(indices)) %>%
                        pivot_longer(.,
                                     cols = 2:4,
                                     names_to = "index",
                                     values_to = "value")

                ggplot(plot_ts, aes(x = position, y = value)) +
                        geom_line() +
                        facet_wrap(. ~ index) +
                        theme_classic() +
                        theme(axis.text.x = element_blank())
                ggsave(getDataPath(
                        folder,
                        "Figures",
                        paste(geo, month, "indicespertime.jpg", sep = "_")
                ))


                for (index in indices) {
                        complete_inter <-
                                select(complete_ts, all_of(index), 4:ncol(complete_ts)) %>%
                                mutate(., motif = NA) %>%
                                mutate(., distance = NA) %>%
                                mutate(., length = NA) %>%
                                mutate(., reference = "0_ts") %>%
                                mutate(., id = 0) %>%
                                rename(., Index = index)



                        # Processing results

                        motif_results <-
                                read.table(getDataPath(
                                        folder,
                                        step4,
                                        paste(geo, "_", month, "_", index, ".txt", sep = "")
                                )) %>%
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
                                filter(n>=30)


                        write.csv(complete_inter,
                                  getDataPath(
                                          folder,
                                          step5,
                                          paste(geo, month, index, "motif.csv", sep = "_")
                                  ),
                                  row.names = F)

                        plot_ts <-
                                select(complete_inter, reference, position, Index, date, time) %>%
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
                        line_intercept1 <- filter(plot_ts, grepl("160000*", time)) %>%
                                .[!duplicated(.$date), ] %>%
                                mutate(time = 060000) %>%
                                select(time, position)

                        #18 in BNE = +10
                        line_intercept2 <- filter(plot_ts, grepl("040000*", time)) %>%
                                .[!duplicated(.$date), ] %>%
                                mutate(time = 180000) %>%
                                select(time, position)

                        ggplot(plot_ts, aes(x = position, y = Index)) +
                                geom_line(aes(colour = what, linetype = what), colour = "grey") +
                                geom_vline(xintercept = line_intercept1$position, linetype = "dotted") +
                                geom_text(data = line_intercept1,
                                          aes(label = time, y = 10, size = 1),
                                          check_overlap = T) +
                                geom_vline(xintercept = line_intercept2$position, linetype = "dotted") +
                                geom_text(data = line_intercept2,
                                          aes(label = time, y = 10, size = 1),
                                          check_overlap = T) +
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
                        ggsave(getDataPath(
                                folder,
                                "Figures",
                                paste(geo, "_", month, "_", index, "ts_motifs.jpg", sep = "")
                        ))

                }

        }
}

# this is the code i started adapting from source ----

# add position # ID
complete_ts <- data_indices_all %>% mutate(position = seq_len(nrow(.)))

# prepare a plot df
# TODO: this is unchanged and I haven't checked what it does but its working
plot_ts <-
        select(complete_ts, position, all_of(indices)) %>%
        pivot_longer(.,
                     cols = 2:4,
                     names_to = "index",
                     values_to = "value")

# create a plot
ggplot(plot_ts, aes(x = position, y = value)) +
        geom_line() +
        facet_wrap(. ~ index) +
        theme_classic() +
        theme(axis.text.x = element_blank())

# save the plot ----
# ggsave(getDataPath(
#         folder,
#         "Figures",
#         paste(geo, month, "indicespertime.jpg", sep = "_")
# ))

# adds some columns ----
# complete_inter will be one dataframe per index and should have other nested loops
for (index in indices) {
        complete_inter <-
                select(complete_ts, all_of(index), 4:ncol(complete_ts)) %>%
                mutate(., motif = NA) %>%
                mutate(., distance = NA) %>%
                mutate(., length = NA) %>%
                mutate(., reference = "0_ts") %>%
                mutate(., id = 0) %>%
                rename(., Index = index) # this renames for example "temporal ent" to "Index"
}

# this will be tested on my zsh files but shouldn't change with pwsh workflow since files were cleaned in previous step (hime_processing) ----
files <- list.files(get_data_path(... = "data/hime-cleaned/"), pattern = ".txt", full.names = TRUE)
files <- files[1]
# TODO: store in df called motif_results
# read in the hime-cleaned files that were processed in hime_processing
for (file in files) {
        motif_results <- read.table(file) %>%
                rename( .,
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
}

# this doesn't work because complete_inter should be by geo and month i think
# the loop has to work on the same csv and the same motif result file at the same time
# that means six data frames to match with six motif files. see start of file - solution in progress

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
        filter(n>=30)




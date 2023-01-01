#' Motif plots
#'
#' This function will use your indices data, and hime data, to generate plots
#' and combined motif/index data frames (.csv files)
#'
#' @param data_indices_all the data frame returned by motifR::time_series
#' @param outputfigures a relative folder path where you want to store figure output
#' @param himedatapath the relative path to your hime data cleaned folder
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom rlang .data
motif_plots <- function(data_indices_all, outputfigures, himedatapath) {
        # define the indices
        total_indices <-
                c("AcousticComplexity", "EventsPerSecond", "TemporalEntropy")

        # where is the hime data to read in (full path)
        himedatapath_full <- get_data_path(himedatapath)

        # where figures will be stored
        outputfigures_full <- get_data_path(outputfigures)

        # create the directory if it doesn't exist
        if (!dir.exists(outputfigures_full)) {
                dir.create(outputfigures_full)
        }

        # going to create one plot per site (AKA sensor point) per month
        # subset the data:
        total_sites <- unique(data_indices_all$site)
        total_months <- unique(data_indices_all$month)

        # the first part of this loop subsets the dataframe and
        # generates one "indices over time" df per site, per month
        # total df = points*months. 2 sites 1 month = 2 plots
        # these df will each get passed to the plot and himejoin functions
        for (sites in total_sites) {
                for (month_id in total_months) {
                        # if there is no data for a site/month, skip to the next
                        # why would there be no data? if looping over the sites,
                        # there could be a site that doesn't have data for a
                        # month for example.
                        skip_to_next <- FALSE

                        tryCatch(
                                {
                                        complete_ts <- data_indices_all %>%
                                                dplyr::filter(site == sites) %>%
                                                dplyr::filter(month == month_id) %>%
                                                dplyr::mutate(., position = seq_len(nrow(.)))
                                },
                                error = function(e) {
                                        skip_to_next <<- TRUE
                                }
                        )

                        if (skip_to_next) {
                                next
                        }

                        # get indices per time plots exported with ggsave
                        indicestimeplots(
                                complete_ts = complete_ts,
                                pathid = outputfigures_full,
                                siteID = sites,
                                total_indices = total_indices,
                                Y.month_id = month_id
                        )

                        # second part: joining hime data to indices data
                        # creates motif.csv tables and creates plots
                        hime_indices(
                                total_indices = total_indices,
                                complete_ts = complete_ts,
                                himedatapath = himedatapath_full,
                                X.sites = sites,
                                X.month_id = month_id,
                                figures = outputfigures_full
                        )
                }
        }
}

#' indicestimeplots function
#'
#' plots time series of all 3 indices
#'
#' @param complete_ts the timeseries data frame
#' @param pathid where output figures are stored
#' @param siteID the study site ID
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom ggplot2 ggplot aes facet_wrap theme_classic theme element_blank
indicestimeplots <- function(complete_ts, pathid, siteID, total_indices, Y.month_id) {
        # prepare the dataframe
        plot_ts <- complete_ts %>%
                dplyr::select(position, dplyr::all_of(total_indices)) %>%
                tidyr::pivot_longer(.,
                        cols = 2:4,
                        names_to = "index",
                        values_to = "value"
                )

        # plot and ggsave in outputfigures
        ggplot2::ggplot(plot_ts, ggplot2::aes(x = position, y = value)) +
                ggplot2::geom_line() +
                ggplot2::facet_wrap(. ~ index) +
                ggplot2::theme_classic() +
                ggplot2::theme(axis.text.x = ggplot2::element_blank())

        ggplot2::ggsave(file.path(
                pathid,
                paste(siteID, Y.month_id, "indicespertime.jpg", sep = "_")
        ))
}

#' hime_indices function
#'
#' matches hime data to time series indices data. generates plots (one per index per site per month)
#'
#' @param total_indices the three indices
#' @param complete_ts the timeseries data frame
#' @param himedatapath the path to hime data
#' @param X.sites site ID
#' @param X.month_id month ID
#' @param figures where the figures will be stored
#'
#' @return
#' @export
#'
#' @examples
hime_indices <- function(total_indices, complete_ts, himedatapath, X.sites, X.month_id, figures) {
        for (index in total_indices) {
                complete_inter <-
                        dplyr::select(complete_ts, dplyr::all_of(index), 4:ncol(complete_ts)) %>%
                        dplyr::mutate(., motif = NA) %>%
                        dplyr::mutate(., distance = NA) %>%
                        dplyr::mutate(., length = NA) %>%
                        dplyr::mutate(., reference = "0_ts") %>%
                        dplyr::mutate(., id = 0) %>%
                        dplyr::rename(., Index = index)

                # Processing results - import the hime file for this specific site/month/index
                hime_file_import_path <- file.path(
                        himedatapath,
                        paste("Res_TS_",
                                index, "_",
                                X.month_id, "_",
                                X.sites,
                                ".txt",
                                sep = ""
                        )
                )
                motif_results1 <-
                        utils::read.table(hime_file_import_path) %>%
                        dplyr::rename(
                                .,
                                FirstInstance_Start = V1,
                                FirstInstance_End = V2,
                                SecondInstance_Start = V3,
                                SecondInstance_End = V4,
                                Length = V5,
                                Distance = V6
                        ) %>%
                        dplyr::mutate(., id = 1:as.numeric(dplyr::count(.))) %>%
                        dplyr::filter(., Distance <= 5) %>%
                        dplyr::select(., id, dplyr::everything()) %>%
                        tidyr::pivot_longer(.,
                                cols = 2:5,
                                names_to = "Instance",
                                values_to = "position"
                        ) %>%
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
                        ) %>%
                        tidyr::separate(.,
                                Instance,
                                into = c("instance", "moment"),
                                sep = "_"
                        ) %>%
                        tidyr::pivot_wider(., names_from = moment, values_from = position) %>%
                        dplyr::mutate(., instance = paste(id, instance, sep = "_"))
                motif_results <- motif_results1 %>%
                        # with(., .[order(Start), ]) %>%
                        # TODO problem with arrange here
                        dplyr::arrange(., Start) %>%
                        dplyr::mutate(., overlap = NA) %>%
                        MIMiCS::remove_repeated(.) # remove repeated seems to be working

                for (row in 1:nrow(complete_inter)) {
                        skip_to_next <- FALSE

                        tryCatch(
                                {
                                        complete_inter[motif_results$Start[row]:motif_results$End[row], c("motif", "distance", "length")] <-
                                                motif_results[row, c("instance", "Distance", "Length")]
                                },
                                error = function(e) {
                                        skip_to_next <<- TRUE
                                }
                        )

                        if (skip_to_next) {
                                next
                        }
                }

                complete_inter <- dplyr::group_by(complete_inter, motif) %>%
                        dplyr::add_count(.) %>%
                        dplyr::filter(n >= 30) # TODO: check message here storing counts in nn

                # file name to give
                motif_csv <- file.path(himedatapath, paste("Motif_",
                        index, "_",
                        X.month_id, "_", ## check
                        X.sites, ## check
                        ".csv",
                        sep = ""
                ))
                write.csv(
                        x = complete_inter,
                        file = motif_csv,
                        row.names = F
                )

                # prepare df for plot
                plot_ts <- complete_inter %>%
                        dplyr::select(reference, position, Index, date, time) %>%
                        tidyr::separate(.,
                                reference,
                                into = c("number", "what"),
                                remove = F
                        )

                plot_motif <-
                        dplyr::select(complete_inter, motif, position, Index) %>%
                        dplyr::rename(., reference = motif) %>%
                        dplyr::filter(reference != "NA") %>%
                        tidyr::separate(.,
                                reference,
                                into = c("number", "what"),
                                remove = F
                        )


                # 6 in BNE = +10
                # just for visual purposes adds a line at a time you want
                # line_intercept1 <- filter(plot_ts, grepl("160000*", time)) %>%
                #         .[!duplicated(.$date), ] %>%
                #         mutate(time = 060000) %>%
                #         select(time, position)

                # 18 in BNE = +10
                # line_intercept2 <- filter(plot_ts, grepl("040000*", time)) %>%
                #         .[!duplicated(.$date), ] %>%
                #         mutate(time = 180000) %>%
                #         select(time, position)

                # missing what column -- what did i mean by this
                ggplot2::ggplot(plot_ts, ggplot2::aes(x = position, y = Index)) +
                        ggplot2::geom_line(ggplot2::aes(colour = reference, linetype = reference), colour = "grey") +
                        # geom_vline(xintercept = line_intercept1$position, linetype = "dotted") +
                        # geom_text(data = line_intercept1,
                        #           aes(label = time, y = 10, size = 1),
                        #           check_overlap = T) +
                        # geom_vline(xintercept = line_intercept2$position, linetype = "dotted") +
                        # geom_text(data = line_intercept2,
                        #           aes(label = time, y = 10, size = 1),
                        #           check_overlap = T) +
                        ggplot2::scale_linetype_manual(values = "dotted") +
                        ggplot2::geom_line(data = plot_motif, ggplot2::aes(
                                x = position,
                                y = Index,
                                colour = reference
                        )) +
                        ggplot2::scale_color_manual(values = c(replicate(nrow(
                                motif_results
                        ), "#2ca25f"))) +
                        ggplot2::theme_classic() +
                        ggplot2::labs(title = paste(index, sep = " ")) +
                        ggplot2::theme(
                                legend.title = ggplot2::element_blank(),
                                axis.title.x = ggplot2::element_blank(),
                                axis.text = ggplot2::element_blank(),
                                axis.ticks = ggplot2::element_blank(),
                                legend.position = "none"
                        )
                ggplot2::ggsave(file.path(figures, paste(X.sites, X.month_id, index, "indicespertime.jpg", sep = "_")))
        }
}

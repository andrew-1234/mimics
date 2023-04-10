#' Image classification shiny gadget
#' @import shiny
#' @export mimics_classify
mimics_classify <- function(
    input_list, threshold = 0.3, auto_backup = TRUE,
    seed = 1234) {
  shiny::shinyApp(
    ui <- fluidPage(
      theme = shinythemes::shinytheme("simplex"),
      titlePanel("Image Classifier"),
      sidebarLayout(
        sidebarPanel(
          h3("Current Image"),
          textOutput("image_num"),
          textOutput("current_file"),
          br(),
          h3("Current Tag"),
          textOutput("current_tag"),
          br(),
          h3("Tag Status"),
          textOutput("tag_status"),
          br(),
          actionButton("back", "Previous Image"),
          actionButton("next", "Next Image"),
          br(),
          h3("Assign tag"),
          textInput("tag", "Enter tag here"),
          actionButton("submit", "Submit"),
          br(),
          # actionButton("play_sound", "Play sound"),
          h3("Audio contols"),
          actionButton("open_audio", "Play Sound"),
          actionButton("open_audio_audacity", "Open Sound in Audacity"),
          br(),
          h3("Download results"),
          downloadButton("download_table", "Download Table")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Image",
              fluidRow(
                h3("Image Display"),
                imageOutput("image"),
              )
            ),
            tabPanel(
              "Table",
              fluidRow(
                h3("Tag Table"),
                dataTableOutput("tags_table")
              )
            )
          )
        )
      )
    ),
    server <- function(input, output, session) {
      if (inherits(input_list, "list")) {
        cat("Input is a list, preparing for new classification\n")
        classification_dfs <- lapply(input_list, function(x) {
          x$df_summarised$tag <- "No Tag"
          x$df_summarised$tag_type <- NA
          x$df_summarised$img_name <- basename(x$image_names)
          x$df_summarised$img_path <- x$image_names
          return(x$df_summarised)
        })
        classification_df <- do.call(rbind, classification_dfs)
        n_yes <- round(nrow(classification_df) * threshold)
        set.seed(seed)
        classification_df[sample(nrow(classification_df), n_yes), "tag_type"] <- "TODO"
      } else {
        cat("Input is a data frame, continuing a previous classification\n")
        classification_df <- input_list
        all_true <- all(c("tag", "tag_type", "img_name", "img_path") %in% colnames(classification_df))
        if (!all_true) {
          stop("Input data is not of valid input type and may contain incorrect
           columns.\nPlease double check your input data.\n")
        }
        # Count how many rows are listed as TODO in tag_type
        n_yes <- length(which(classification_df$tag_type == "TODO"))
        cat("There are ", n_yes, " images left to classify\n")
      }

      rv <- reactiveValues(index = 1)
      tag_table <- reactiveValues(
        value = classification_df
      )
      current_tag <- reactiveValues(value = classification_df$tag[[1]])
      tag_status <- reactiveValues(value = classification_df$tag_type[[1]])
      # Render image
      image_files <- classification_df$img_path
      # Send a pre-rendered image, and don't delete the image after sending it
      output$image <- renderImage(
        {
          # When input$n is 3, filename is ./images/image3.jpeg
          filename <- normalizePath(image_files[[rv$index]])
          check_image <- png::readPNG(filename)
          # Get image dimensions
          height <- (dim(check_image)[1]) * 2
          width <- (dim(check_image)[2]) * 2

          # Return a list containing the filename and alt text
          list(
            src = filename,
            alt = paste("Image number", rv$index),
            width = width,
            height = height
          )
        },
        deleteFile = FALSE
      )

      # Cycle to next image
      observeEvent(input$`next`, {
        rv$index <- min(rv$index + 1, length(image_files))
        current_tag$value <- tag_table$value$tag[[rv$index]]
        tag_status$value <- tag_table$value$tag_type[[rv$index]]
      })

      # Cycle to previous image
      observeEvent(input$back, {
        rv$index <- max(rv$index - 1, 1)
        current_tag$value <- tag_table$value$tag[[rv$index]]
        tag_status$value <- tag_table$value$tag_type[[rv$index]]
      })

      output$current_file <- renderText({
        basename(image_files[rv$index])
      })

      observeEvent(input$submit, {
        tag_table$value$tag[[rv$index]] <- input$tag
        current_tag$value <- input$tag
        tag_table$value$tag_type[[rv$index]] <- "Done"
        tag_status$value <- "Done"
      })

      output$current_tag <- renderText({
        current_tag$value
      })
      output$tag_status <- renderText({
        tag_status$value
      })
      # Play sound
      # observeEvent(input$play_sound, {
      #   sound_file <- gsub("__ACI", "", image_files[rv$index])
      #   sound_file <- gsub(".png", ".wav", sound_file)
      # })
      # output$sound_file <- renderText({
      #   sound_file <- gsub("__ACI", "", image_files[rv$index])
      #   sound_file <- gsub(".png", ".wav", sound_file)
      # })

      # tags$audio(src = sound_file, type = "audio/wav", autoplay = NA, controls =
      # TRUE)

      # Update image number
      image_num <- reactive({
        paste("Image", rv$index, "of", length(image_files))
      })

      # Render image number
      output$image_num <- renderText({
        image_num()
      })

      # Render the table
      output$tags_table <- renderDataTable({
        tag_table$value[, c("tag", "tag_type", "img_name")]
      })

      # TODO audio stuff needs to be updated
      current_audio <- reactive({
        sound_file <- gsub("__ACI", "", image_files[rv$index])
        sound_file <- gsub(".png", ".wav", sound_file)
      })

      play_audio <- observeEvent(input$open_audio, {
        # works locally but won't play in browser
        system(paste("afplay", current_audio()))
        # WORKING: cool!
        # browseURL(current_audio())
        # system(paste("open -a Audacity", shQuote(current_audio())))
      })
      play_audacity <- observeEvent(input$open_audio_audacity, {
        # works locally but won't play in browser
        # system(paste("afplay", current_audio))
        # WORKING: cool!
        # browseURL(current_audio())
        system(paste("open -a Audacity", shQuote(current_audio())))
      })

      if (auto_backup) {
        # Save the table to a CSV file every minute (60 seconds
        timer <- reactiveTimer(1000 * 60) # Set the timer to 1 minute

        observe({
          timer()
          write.csv(tag_table$value, file = paste("img-class-backup-", Sys.Date(), ".csv", sep = ""))
        })
      }

      # Define a download handler for the download button
      output$download_table <- downloadHandler(
        filename = function() {
          paste("img-class-data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          # Write the table to a CSV file
          write.csv(tag_table$value, file, row.names = FALSE)
        }
      )
    }
  )
}

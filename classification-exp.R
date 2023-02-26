library(shiny)
library(png)
library(tuneR)
library(wavethresh)
library(shinyjs)

# Load image file names from output folder
image_files <- list.files(paste0(getwd(), "/output/cropped"), pattern = ".png", full.names = TRUE)
# Create empty data frame to store classification tags
# tags <- data.frame(file = basename(image_files), tag = "No Tag", stringsAsFactors = FALSE)


ui <- fluidPage(
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
      actionButton("back", "Previous Image"),
      actionButton("next", "Next Image"),
      br(),
      h3("Assign tag"),
      textInput("tag", "Enter tag here"),
      actionButton("submit", "Submit"),
      br(),
      # actionButton("play_sound", "Play sound"),
      actionButton("open_audio", "Play Sound"),
      actionButton("open_audio_audacity", "Open Sound in Audacity")
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
)



server <- function(input, output, session) {
  # Initialize index
  rv <- reactiveValues(index = 1)
  current_tag <- reactiveValues(value = "")
  tag_table <- reactiveValues(
    value =
      data.frame(
        file =
          basename(image_files),
        image_number = 1:length(image_files),
        tag = "No Tag",
        stringsAsFactors = FALSE
      )
  )

  # Render image
  # Send a pre-rendered image, and don't delete the image after sending it
  output$image <- renderImage(
    {
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath(image_files[[rv$index]])
      check_image <- readPNG(filename)
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
  })

  # Cycle to previous image
  observeEvent(input$back, {
    rv$index <- max(rv$index - 1, 1)
    current_tag$value <- tag_table$value$tag[[rv$index]]
  })

  # Submit classification tag
  # observeEvent(input$submit, {
  #   tags <<- rbind(tags, data.frame(file = image_files[rv$index], tag = input$tag, stringsAsFactors = FALSE))
  # })
  output$current_file <- renderText({
    basename(image_files[rv$index])
  })
  # output$current_tag <- renderText({
  #   tags$tag[[rv$index]]
  # })

  # tags <- reactiveValues()

  observeEvent(input$submit, {
    # tags$file[[rv$index]] <<- image_files[rv$index]
    tag_table$value$tag[[rv$index]] <- input$tag
    current_tag$value <- input$tag
  })

  output$current_tag <- renderText({
    current_tag$value
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
    tag_table$value[, 2:3]
  })

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
}

shinyApp(ui, server)

# should be an option to select your own DF / csv
# output the csv when done
# show the current 1/4 image'

# playing audio on windows
# observeEvent(input$open_audio, {
#     file_path <- paste0(getwd(), "/", rv$dir, "/", rv$files[rv$index])
#     shell.exec(file_path)
#   })

# test <- "/Users/andrew/Documents/GitHub/mimics/output/cropped/20201230T050000+1030_Bon-Bon-Station-Dry-A_418011__ACI_1_match_112-120.png"
#     sound_file <- gsub("__ACI", "", test)
#     sound_file <- gsub(".png", ".wav", sound_file)
#     system(paste("afplay", sound_file))
# browseURL(sound_file)

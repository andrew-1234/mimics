library(shiny)
library(png)
library(tuneR)
library(wavethresh)
library(shinyjs)

# Load image file names from output folder
image_files <- list.files(paste0(getwd(), "/output/cropped"), pattern = ".png", full.names = TRUE)
# Create empty data frame to store classification tags
tags <- data.frame(file = character(), tag = character(), stringsAsFactors = FALSE)

ui <- fluidPage(
  fluidRow(
    column(
      width = 6, align = "center",
      imageOutput("image"),
      br(),
      actionButton("back", "Back"),
      actionButton("next", "Next")
    ),
    column(
      width = 6, align = "center",
      h3("Classification"),
      textInput("tag", "Enter tag here"),
      actionButton("submit", "Submit"),
      br(),
      actionButton("play_sound", "Play sound")
    )
  )
)

server <- function(input, output, session) {
  # Initialize index
  rv <- reactiveValues(index = 1)

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
  })

  # Cycle to previous image
  observeEvent(input$back, {
    rv$index <- max(rv$index - 1, 1)
  })

  # Submit classification tag
  observeEvent(input$submit, {
    tags <<- rbind(tags, data.frame(file = image_files[rv$index], tag = input$tag, stringsAsFactors = FALSE))
  })

  # # Play sound
  # observeEvent(input$play_sound, {
  #   sound_file <- gsub("__ACI", "", image_files[rv$index])
  #   sound_file <- gsub(".png", ".wav", sound_file)
  #   sound <- readWave(sound_file)
  #   play(sound)
  # })
}

shinyApp(ui, server)
test <- readPNG(image_files[2])

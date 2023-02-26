library(shiny)
library(tuneR)
library(seewave)
library(png)

# Load image file names from output folder
image_files <- list.files("./output/cropped", pattern = ".png")

# Create empty data frame to store classification tags
tags <- data.frame(file = character(), tag = character(), stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  titlePanel("Image Classification"),
  mainPanel(
    imageOutput("image"),
    textInput("tag", "Classification Tag:"),
    actionButton("submit", "Submit"),
    actionButton("play_sound", "Play Sound"),
    br(),
    actionButton("back", "Back"),
    actionButton("next", "Next")
  )
)

# Define server function
server <- function(input, output, session) {
  # Initialize index
  index <- reactiveValues(value = 1)

  # Load first image
  current_image <- readPNG(image_files[index])

  # Render image
  output$image <- renderImage(
    {
      list(src = "current_image", contentType = "image/png")
    },
    deleteFile = FALSE
  )

  # Cycle to next image
  observeEvent(input$`next`, {
    index <- min(index + 1, length(image_files))
    current_image <<- readPNG(image_files[index])
  })

  # Cycle to previous image
  observeEvent(input$back, {
    index <- max(index - 1, 1)
    current_image <<- readPNG(image_files[index])
  })

  # Submit classification tag
  observeEvent(input$submit, {
    tags <<- rbind(tags, data.frame(file = image_files[index], tag = input$tag, stringsAsFactors = FALSE))
  })

  # Play sound
  observeEvent(input$play_sound, {
    sound_file <- gsub(".png", ".wav", image_files[index])
    sound <- readWave(sound_file)
    play(sound)
  })
}

# Run the app
shiny::shinyApp(ui, server)

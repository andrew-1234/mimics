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
      actionButton("play_sound", "Play sound")
    ),
    mainPanel(
      h3("Image Display"),
      imageOutput("image"),
      dataTableOutput("tags_table")
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
    current_tag$value <- tags$tag[[rv$index]]
  })

  # Cycle to previous image
  observeEvent(input$back, {
    rv$index <- max(rv$index - 1, 1)
    current_tag$value <- tags$tag[[rv$index]]
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
    tags$tag[[rv$index]] <<- input$tag
    current_tag$value <<- input$tag
  })

  output$current_tag <- renderText({
    current_tag$value
  })

  # # Play sound
  # observeEvent(input$play_sound, {
  #   sound_file <- gsub("__ACI", "", image_files[rv$index])
  #   sound_file <- gsub(".png", ".wav", sound_file)
  #   sound <- readWave(sound_file)
  #   play(sound)
  # })

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
    tag_table
  })
}

shinyApp(ui, server)

# should be an option to select your own DF / csv
# output the csv when done
# show the current 1/4 image'

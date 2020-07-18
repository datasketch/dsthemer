library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(dsmodules)
library(tidyverse)
library(homodatum)
library(dsvizopts)
library(ggmagic)

ui <- panelsPage(
  panel(
    title = "Customize Theme",
    width = 300,
    body = div(
      uiOutput("available_themes"),
      uiOutput("controls")
    )
  ),
  panel(
    title = "Theme YAML",
    width = 300,
    body = div(
      textAreaInput("theme_yaml", "", value = "# Select a themre",
                    height = "800px")
    )
  ),
  panel(
    title = "Viz",
    body = plotOutput("viz"),
    footer = uiOutput("viz_icons")
  )
)

div_dark <- function(...){
  div(style="background-color:#f4f4f7;border: 1px solid #CCC;border-radius:10px;padding:10px;margin-bottom:10px;", ...)
}


server <-  function(input, output, session) {

  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)

  output_parmesan("controls", parmesan = parmesan,
                  container_section = div_dark,
                  input = input, output = output)

  output$available_themes <- renderUI({
    themes <- dsthemer::theme_list()
  })

  output$debug <- renderPrint({
    #str(parmesan_input())
    #str(data())
  })

  data <- reactive({
    req(input$viz_selection)
    if(input$viz_selection %in% c("pie", "donut", "bar", "treemap", "line")){
      data <- sample_data("Cat-Num")
    }
    if(input$viz_selection %in% c("grouped_bars", "stacked_bars")){
      data <- sample_data("Cat-Cat-Num")
    }
    data
  })


  add_color_codes_box <- function(id){
    codes_box_id <- paste0(id,"_codes")
    insertUI(paste0("#",id), immediate = TRUE, where = "afterEnd",
             ui = div(br(),verbatimTextOutput(codes_box_id)))
    output[[codes_box_id]] <- renderPrint({
      cat(paste(input[[id]], collapse = "\n"))
    })
  }

  observeEvent(input$parmesan_updated, {
    add_color_codes_box("palette_colors")
    add_color_codes_box("background_color")
    add_color_codes_box("branding_background_color")
    add_color_codes_box("branding_accent_color")
    add_color_codes_box("text_color")
    add_color_codes_box("title_color")
    add_color_codes_box("subtitle_color")
    add_color_codes_box("caption_color")
    add_color_codes_box("line_color")
    add_color_codes_box("axis_title_color")
    add_color_codes_box("axis_line_color")
    add_color_codes_box("axis_ticks_color")
    add_color_codes_box("grid_color")
    add_color_codes_box("grid_x_color")
    add_color_codes_box("grid_y_color")
    add_color_codes_box("plot_background_color")
    add_color_codes_box("plot_border_color")
    add_color_codes_box("legend_color")
  })

  output$viz <- renderPlot({
    selected_viz <- input$viz_selection
    viz <- paste0("gg_", selected_viz, "_CatNum")
    data <- data()
    opts <- parmesan_input()
    opts$color_by <- names(data)[1]
    do.call(viz, list(data, opts))
  })

  output$viz_icons <- renderUI({
    buttonImageInput('viz_selection',
                     HTML('<div class = "style_section">Choose a visualization type</div>'),
                     images = c("bar",  "pie", "donut", "treemap", "bubbles", "line"),
                     path = 'img/svg/',
                     format = 'svg')
  })

  parmesan_alert(parmesan, env = environment())
}


shinyApp(ui, server)

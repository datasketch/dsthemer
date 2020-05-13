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
    title = "Upload Data", 
    width = 300,
    body = div(
      uiOutput("controls")
    )
  ),
  panel(
    title = "Viz",
    body = plotOutput("viz"),
    footer = uiOutput("viz_icons")
  )
)

server <-  function(input, output, session) {
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  
  output_parmesan("controls", parmesan = parmesan,
                  input = input, output = output)
  
  output$debug <- renderPrint({
    str(parmesan_input())
    str(data())
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

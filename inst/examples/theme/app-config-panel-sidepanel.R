# Side Panel Example
# This example demonstrates the sidePanelInput widget with dynamic content
# using body parameter with uiOutput for server-side rendering

library(shiny)
library(dsshiny)
library(parmesan)
library(dsinputs)
library(shinyinvoer)
library(dsreactwidgets)

# UI
ui <- dsBoardPage(
  # First board - Controls
  dsBoard(
    id = "control_panel",
    title = "Panel de ejemplo",
    body = tagList(
      # Selector de tipo de visualización
      selectInput(
        "viz_type",
        "Tipo de Visualización:",
        choices = c("bar", "line", "map_world_countries", "sankey", "wordcloud"),
        selected = "bar"
      ),

      # Mostrar parámetros actuales
      h4("Parámetros de Tema Actuales:"),
      verbatimTextOutput("current_params")
    ),
    width = "30%"
  ),
  dsBoard(
    id = "data_panel",
    title = "Graph",
    body = div(
      settings_render_ui("theme_settings"),
      div(
        style = "display:flex;border: 1px solid #DDDDDD;margin-top: 12px;height: 600px;",
        div(
          id = "loading_controls"
        ),
        div(
          id = "graph_content",
          h4("Main Content Area"),
          p("This content will shift when the panel opens/closes."),
          verbatimTextOutput("selected_item_display"),
          p("inputs in panel"),
          verbatimTextOutput("test_inputs")
        )
      )
    ),
    width = "70%"
  )
)

# Server
server <- function(input, output, session) {

  r <- reactiveValues(
    data = mtcars,  # Datos de ejemplo
    viz_plot = "bar",  # Tipo de visualización inicial
    org = "datasketch",  # Organización para el tema
    config_lang = "en",  # Idioma para traducciones

    # Configuración opcional del panel lateral
    # Si no se especifican, se usan los valores por defecto
    containerId = "loading_controls",
    mainContentId = "graph_content",
    panelPosition = "left",  # panel cierra a la izquierda
    panelWidth = "350px",
    panelInitialOpen = TRUE,  # Panel abierto
    panelButtonText = "Configurar Tema"
  )

  # Observar cambios en el tipo de visualización
  observe({
    r$viz_plot <- input$viz_type
  })

  # Inicializar el módulo de configuración de temas
  settings_render("theme_settings", r)

  # Mostrar los parámetros actuales
  output$current_params <- renderPrint({
    if (!is.null(r$params)) {
      str(r$params)
    } else {
      "Esperando configuración..."
    }
  })

  # Observar cambios en los parámetros
  # observe({
  #   if (!is.null(r$params)) {
  #     cat("Parámetros actualizados:\n")
  #     cat("Título:", r$params$title %||% "N/A", "\n")
  #     cat("Subtítulo:", r$params$subtitle %||% "N/A", "\n")
  #     cat("Tipo de paleta:", r$params$color_palette_type %||% "N/A", "\n")
  #   }
  # })
}

# Run the application
shinyApp(ui = ui, server = server)

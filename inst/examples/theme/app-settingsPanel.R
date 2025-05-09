library(dsthemer)
library(shiny)
library(shinypanels)
library(dsmods)
library(parmesan)
library(shinyinvoer)
library(shinyjs)
library(shi18ny)

ui <- panelsPage(
  useShinyjs(),
  useShi18ny(),
  panel(
    title = "Data viz",
    id = "data-viz",
    body = div(checkboxGroupInput("id_viz", "Select Viz", c("bar", "line", "map_world_countries")),
               filterVizUI("filterModule"),
               selectVizUI("selectModule")
    )
  ),
  panel(
    title = "Data upload",
    id = "data-xx",
    body = div(
     uiOutput("confViz"),
     actionButton("open_config", " ", icon = icon("cogs"),
                  class = "config-btn", style = "display: none;"),
     checkboxInput("test_id", "mostrar color by")
    )
  ),
  config_panel_ui("config_panel")
)

server <- function(input, output, session) {

  opts <- list(
    defaultLang = "en",
    availableLangs = c("es","en"),
    customTranslationSource = "csv"
  )

  i18n <- i18nLoad(opts)
  lang <- callModule(langSelector,"lang", i18n = opts, showSelector = F)

  r <- reactiveValues(
    data = 1,
    caption = "Créditos",
    label_theme = "dark",
    var_cat_colors = NULL,
    default_var_color = NULL,
    idButton = "open_config",
    has_map = FALSE,
    not_map = TRUE,
    lang = lang,
    i18n = i18n
  )

  availableIcons <- c("bar", "pie", "line", "map_world_countries")

  filteredIcons <- filterVizServer("filterModule", availableIcons, reactive(input$id_viz), r)

  selectedViz <- selectVizServer(
    id = "selectModule",
    iconsToShow = filteredIcons$filteredIcons,
    lastClick = reactive(input$`selectModule-selectedVizInput`),
    r
  )

  observe({
    r$viz_plot <- selectedViz$activeIcon() %||% input$id_viz[1]
    if (is.null(selectedViz$activeIcon())) {
      hide("open_config")  # Ocultar si no hay icono activo
    } else {
      r$org <- "datasketch"
      r$has_bar <- selectedViz$activeIcon() == "bar"
      show("open_config")  # Mostrar cuando hay icono seleccionado
    }
  })

  observeEvent(selectedViz$activeIcon(), {
    if (selectedViz$activeIcon() == "bar") {
      r$has_bar <- TRUE
    } else {
      r$has_bar <- FALSE
    }
  })

  conf <- config_panel_server("config_panel", r)

  observe({
    print(conf$params())  # Imprimir valores actualizados
  })
}



shinyApp(ui, server)

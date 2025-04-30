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
    body = settings_render_ui("controls")
    )
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
    data = NULL,
    viz_plot = "bar",
    org = "datasketch",
    lang = lang,
    i18n = i18n
  )
  settings_render("controls", r)

  observe({
    print(r$params$title)  # Imprimir valores actualizados
  })

}

shinyApp(ui, server)

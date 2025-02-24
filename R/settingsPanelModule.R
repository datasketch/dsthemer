#' @export
config_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      includeScript(dsmods_sys_file("lib/panelControl/panelControl.js")),
      includeCSS(dsmods_sys_file("lib/panelControl/panelControl.css"))
    ),
    panel(
      title = "Graph configuration",
      id = ns("theme_view"),
      width = 300,
      can_collapse = FALSE,
      body = div(
        uiOutput(ns("dynamic_inputs"))
      )
    )
  )
}


#' @export
config_panel_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    path <- dsmods_sys_file("defaults/basic_plots/parmesan")
    parmesan <- parmesan_load(path)  # Cargar configuración YAML
    parmesan_inputs <- parmesan_watch(input, parmesan)  # Observar cambios en inputs
    r_parmesan <- reactiveValues()
    # Actualizar valores reactivos si son funciones ()

    observe({
      r_parmesan$params <- parmesan_inputs()

      for (section in names(parmesan)) {
        if (!is.null(parmesan[[section]]$inputs) && length(parmesan[[section]]$inputs) > 0) {
          for (input_def in parmesan[[section]]$inputs) {
            if (!is.null(input_def$input_params)) {
              for (param in names(input_def$input_params)) {
                param_value <- input_def$input_params[[param]]
                if (!is.null(param_value) && is.character(param_value) && length(param_value) == 1 && grepl("\\(\\)$", param_value)) {
                  param_name <- gsub("\\(\\)$", "", param_value)
                  if (!is.null(r[[param_name]])) {
                    input_def$input_params[[param]] <- r[[param_name]]
                  }
                }
              }
            }
          }
        }
      }

     if (!is.null(r$viz_plot)) {
      palette <- "categorical"
      print(r$has_map)
      if (r$has_map) {
        palette <- "sequential"
        r_parmesan$params$map_name <- gsub("map_", "", r$viz_plot)
      }
      r$palette <- palette
     }
    })

    # Renderizar los inputs dinámicamente
    output$dynamic_inputs <- renderUI({
      input_list <- list()

      for (section in names(parmesan)) {
        section_data <- parmesan[[section]]

        # Verificar si la sección tiene inputs
        if (!is.null(section_data$inputs) && length(section_data$inputs) > 0) {
          # Tooltip para la sección
          tooltip_icon <- if (!is.null(section_data$description)) {
            paste0(
              "<div class='tooltip-theme'>
            <i class='fa fa-info-circle'></i>
            <span class='tooltiptext'>", section_data$description, "</span>
          </div>"
            )
          } else {
            ""
          }

          # Agregar el label y el tooltip al contenedor principal de la sección
          input_list <- append(input_list, list(HTML(
            paste0("<div class='section-parmesan' id='", section, "' style='display: flex; align-items: center; gap: 10px;'>",
                   section_data$label, tooltip_icon, "</div>")
          )))

          # Procesar cada input y agregar el tooltip al widget
          section_inputs <- lapply(as.list(section_data$inputs), function(input_def) {
            input_params <- input_def$input_params

            if (!is.null(input_params)) {
              # Reemplazar valores específicos de los parámetros si es necesario
              for (param in names(input_params)) {
                param_value <- input_params[[param]]
                if (!is.null(param_value) && is.character(param_value) && length(param_value) == 1 && grepl("\\(\\)$", param_value)) {
                  param_name <- gsub("\\(\\)$", "", param_value)
                  if (!is.null(r[[param_name]])) {
                    input_params[[param]] <- r[[param_name]]
                  }
                }
              }
            }

            widget_tooltip <- if (!is.null(input_def$description)) {
              paste0(
                "<span class='tooltip-theme'>
              <i class='fa fa-info-circle'></i>
              <span class='tooltiptext'>", input_def$description, "</span>
            </span>"
              )
            } else {
              ""
            }
            input_params$label <- HTML(paste(input_params$label, widget_tooltip))
            show_widget <- TRUE
            if (!is.null(input_def$show_if)) {
              param_condition <- gsub("\\(\\)$", "", input_def$show_if)
              show_widget <- r[[param_condition]] %||% TRUE
            }

            if (show_widget) {
              div(
                class = "widget-container",
                do.call(input_def$input_type, c(list(inputId = ns(input_def$id)), input_params))
              )
            } else {
              return()
            }
          })
          if (!is.null(section_inputs)) {
            input_list <- append(input_list, section_inputs)
          }
        }
      }
      do.call(tagList, input_list)
    })



    # Sincronización de inputs con valores reactivos
    observe({

      for (section in names(parmesan)) {
        if (!is.null(parmesan[[section]]$inputs) && length(parmesan[[section]]$inputs) > 0) {
          for (input_def in parmesan[[section]]$inputs) {
            observeEvent(input[[ns(input_def$id)]], {
              isolate({
                r[[input_def$id]] <- input[[ns(input_def$id)]]
              })
            }, ignoreNULL = FALSE, ignoreInit = FALSE)
          }
        }
      }


    })


    observe({
      req(parmesan_inputs()$theme)

        r$theme <- parmesan_inputs()$theme
        r$agg_palette <- dsthemer::dsthemer_palette(r$org, theme = r$theme, palette = r$palette)
        r_parmesan$params[[paste0("color_palette_", r$palette)]] <- input$color_palette
    })

    # observeEvent(input$color_palette, {
    #   r_parmesan$params[[paste0("color_palette_", r$palette)]] <- input$color_palette
    # })


    observe({
      req(r$theme)
      theme <- Filter(Negate(is.null), dsthemer::dsthemer(org = r$org, theme = r$theme))
      r_parmesan$params$background_color <- dsthemer::dsthemer_background(r$org, r$theme)
      r_parmesan$params <- modifyList(theme, r_parmesan$params)
    })

    filtered_params <- reactive({
      req(r_parmesan$params)
      ls <- r_parmesan$params
      if ("theme" %in% names(ls)) {
        ls$theme <- NULL
      }
      if ("color_palette" %in% names(ls)) {
        ls$color_palette <- NULL
      }
      #non_null_params <- Filter(Negate(is.null), r_parmesan$params)
      #r_parmesan$params <- modifyList(r_parmesan$params, non_null_params)
      ls
    })


    return(list(params = reactive({filtered_params()})))
  })
}

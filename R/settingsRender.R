#' @export
settings_render_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      includeCSS(dsthemer_sys_file("lib/panelControl/panelControl.css"))
    ),
    uiOutput(ns("side_panel"))
  )
}

#' @export
settings_render <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(r$data)
    })

    path <- dsthemer_sys_file("defaults/basic_plots/parmesan")
    parmesan <- parmesan_load(path)  # Cargar configuración YAML
    parmesan_updates <- yaml::read_yaml(dsthemer_sys_file("defaults/basic_plots/update_inputs.yaml"))$shiny
    r_parmesan <- reactiveValues()

    # Generar menuItems basados en las secciones del YAML
    menu_items <- reactive({
      req(r$data)

      purrr::map(names(parmesan), function(section) {
        section_data <- parmesan[[section]]

        # Iconos SVG para cada sección
        icons <- list(
          data = "<svg width='20' height='20' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><ellipse cx='12' cy='6' rx='8' ry='2'></ellipse><path d='M4 6v12c0 1.1 3.6 2 8 2s8-.9 8-2V6'></path><path d='M4 10v8c0 1.1 3.6 2 8 2s8-.9 8-2v-8'></path></svg>",
          titles = "<svg width='20' height='20' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><line x1='4' y1='7' x2='20' y2='7'></line><line x1='4' y1='12' x2='20' y2='12'></line><line x1='4' y1='17' x2='20' y2='17'></line></svg>",
          colors = "<svg width='20' height='20' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><circle cx='13.5' cy='6.5' r='.5' fill='currentColor'></circle><circle cx='17.5' cy='10.5' r='.5' fill='currentColor'></circle><circle cx='8.5' cy='7.5' r='.5' fill='currentColor'></circle><circle cx='6.5' cy='12.5' r='.5' fill='currentColor'></circle><path d='M12 2C6.5 2 2 6.5 2 12s4.5 10 10 10c.926 0 1.648-.746 1.648-1.688 0-.437-.18-.835-.437-1.125-.29-.289-.438-.652-.438-1.125a1.64 1.64 0 0 1 1.668-1.668h1.996c3.051 0 5.555-2.503 5.555-5.554C21.965 6.012 17.461 2 12 2z'></path></svg>"
        )

        list(
          id = section,
          icon = icons[[section]] %||%  icons[[1]],
          title = dstextui::translate(section_data$label, r$config_lang),
          tooltip = if (!is.null(section_data$description)) {
            dstextui::translate(section_data$description, r$config_lang)
          } else {
            dstextui::translate(section_data$label, r$config_lang)
          },
          body = uiOutput(ns(paste0("section_", section)))
        )
      })
    })

    output$side_panel <- renderUI({
      req(r$data)
      req(menu_items())

      container_id <- r$containerId %||% "loading_controls"
      main_content_id <- r$mainContentId %||% "graph_content"
      panel_position <- r$panelPosition %||% "left"
      panel_width <- r$panelWidth %||% "300px"
      panel_initial_open <- r$panelInitialOpen %||% TRUE
      panel_button_text <- r$panelButtonText %||% dstextui::translate("graph_conf", r$config_lang)

      dsreactwidgets::sidePanelInput(
        inputId = ns("side_panel"),
        menuItems = menu_items(),
        containerId = container_id,
        mainContentId = main_content_id,
        position = panel_position,
        panelWidth = panel_width,
        initialOpen = panel_initial_open,
        buttonText = panel_button_text
      )
    })

    palette_colors <- reactive({
      req(r$data)
      org <- r$org
      list(
        categorical = dsthemer_palette(org,  palette = "categorical") |> unlist(),
        sequential = dsthemer_palette(org,  palette = "sequential") |> unlist(),
        divergening = dsthemer_palette(org,  palette = "divergening") |> unlist()
      )
    })

    colors_list <- reactive({
      req(r$data)
      req(palette_colors())
      palette_colors <- palette_colors()
      lc <- purrr::map(names(palette_colors), function(palette) {
        colors <- palette_colors[[palette]]
        as.character(div(purrr::map(colors, function(color) {
          div(style = paste0("width: 20px; height: 20px; display: inline-block; background-color:",
                             color, ";"))
        })))
      })
      names(lc) <- names(palette_colors)
      lc
    })




    observeEvent(input$color_palette_type, {
      req(r$data)
      req(input$color_palette_type)
      input_debounced <- input$color_palette_type

      r$agg_palette <- dsthemer_palette(r$org, palette = isolate(input_debounced))
      updateRadioButtonsInput(session = session, inputId = "color_palette_type", choices = NULL, selected = input_debounced)
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    observe({
      req(r$data)
      r$color_opts <- colors_list()
      r$discret_plot <- TRUE
      r$continuos_plot <- FALSE


      if (!is.null(r$agg_palette)) {
        updateColorPaletteInput(session = session, inputId = "color_palette",
                                colors = r$agg_palette)
      }

      if (!is.null(r$viz_plot)) {
        r$has_wordcloud <- r$viz_plot %in% "wordcloud"
        r$has_sankey <- r$viz_plot %in% "sankey"
        r$has_num <- r$viz_plot != "sankey"
        r$has_axis <- r$viz_plot %in% c("bar", "line")
        r$has_bar <- r$viz_plot %in% "bar"
        r$has_line <- r$viz_plot %in% "line"
        r$has_map <- grepl("map_", r$viz_plot)
        r$discret_plot <- isTRUE(!r$has_map)

        r$continuos_plot <- grepl("map_", r$viz_plot)

        if (r$has_map) {
          r$discret_plot <- FALSE
          r$has_bar <- FALSE
          r$has_line <- FALSE
          r_parmesan$params$map_name <- gsub("map_", "", r$viz_plot)
        }
      }


      for (section in names(parmesan)) {
        if (!is.null(parmesan[[section]]$inputs) && length(parmesan[[section]]$inputs) > 0) {
          for (input_def in parmesan[[section]]$inputs) {
            if (!is.null(input_def$input_params)) {
              for (param in names(input_def$input_params)) {
                param_value <- input_def$input_params[[param]]
                # Actualizar valores reactivos si son funciones ()
                if (!is.null(param_value) && is.character(param_value) && length(param_value) == 1 && grepl("\\(\\)$", param_value)) {
                  param_name <- gsub("\\(\\)$", "", param_value)
                  if (!is.null(r[[param_name]])) {
                    input_def$input_params[[param]] <- r[[param_name]]
                  }
                }
              }
              # actualiza los inputs update... segun el widget
              if (!is.null(input[[input_def$id]])) {
                if (!any(grepl("\\(\\)$", input[[input_def$id]]))) {
                  r_parmesan$params[[input_def$id]] <- input[[input_def$id]]
                }
              }
            }
          }
        }
      }
    })

    observeEvent(input$color_palette, {
      if (!identical(input$color_palette, "agg_palette()")) {
        colors <- lapply(input$color_palette, function(x) list(x))
        if (!identical(r$agg_palette, colors)) {
          r$agg_palette <- colors
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Función auxiliar para renderizar inputs de una sección
    render_section_inputs <- function(section_name) {
      section_data <- parmesan[[section_name]]
      input_list <- list()

      # Verificar si la sección tiene inputs
      if (!is.null(section_data$inputs) && length(section_data$inputs) > 0) {
        # Procesar cada input y agregar el tooltip al widget
        section_inputs <- lapply(as.list(section_data$inputs), function(input_def) {
          if (is.null(input_def$input_params)) return(NULL)

          input_params <- input_def$input_params

          if (!is.null(input_params)) {
            # Reemplazar valores específicos de los parámetros si es necesario
            for (param in names(input_params)) {

              param_value <- dstextui::translate(input_params[[param]], r$config_lang)
              if (!is.null(param_value) && is.character(param_value) && length(param_value) == 1 && grepl("\\(\\)$", param_value)) {
                param_name <- gsub("\\(\\)$", "", param_value)
                if (!is.null(r[[param_name]])) {
                  input_params[[param]] <- r[[param_name]]
                }
              }

              if (!is.null(r_parmesan$params[[input_def$id]])) {
                if (input_def$id != "color_palette") {
                  param_update <- parmesan_updates[[input_def$input_type]]$update_param
                  input_params[[param_update]] <- r_parmesan$params[[input_def$id]]
                }
              }
            }
          }

          widget_tooltip <- if (!is.null(input_def$description)) {
            paste0(
              "<span class='tooltip-theme'>
            <i class='fa fa-info-circle'></i>
            <span class='tooltiptext'>", dstextui::translate(input_def$description, r$config_lang), "</span>
          </span>"
            )
          } else {
            ""
          }
          input_params$label <- HTML(paste(dstextui::translate(input_params$label, r$config_lang), widget_tooltip))
          show_widget <- TRUE
          if (!is.null(input_def$show_if)) {
            param_condition <- gsub("\\(\\)$", "", input_def$show_if)
            show_widget <- r[[param_condition]] %||% TRUE
          }

          if ("choices" %in% names(input_params)) {
            names(input_params$choices) <- dstextui::translate(names(input_params$choices), r$config_lang)
          }

          if (show_widget) {
            div(
              class = "widget-container",
              do.call(input_def$input_type, c(list(inputId = ns(input_def$id)), input_params))
            )
          } else {
            return(NULL)
          }
        })

        # Filtrar NULLs y agregar a la lista
        section_inputs <- Filter(Negate(is.null), section_inputs)
        if (length(section_inputs) > 0) {
          input_list <- append(input_list, section_inputs)
        }
      }

      do.call(tagList, input_list)
    }

    # Crear renderUI para cada sección
    for (section in names(parmesan)) {
      local({
        section_name <- section
        output[[paste0("section_", section_name)]] <- renderUI({
          req(r$data)
          render_section_inputs(section_name)
        })
      })
    }


    observe({
      req(r_parmesan$params)
      theme <- Filter(Negate(is.null), dsthemer(org = r$org))
      r_parmesan$params$background_color <- dsthemer_background(r$org)
      r_parmesan$params <- modifyList(theme, r_parmesan$params)
      ls <- r_parmesan$params
      ls[[paste0("color_palette_", ls[["color_palette_type"]])]] <- ls$color_palette

      if ("color_palette" %in% names(ls)) {
        ls[["color_palette"]] <- NULL
      }

      if (!"map_tiles" %in% names(ls)) {
        ls["map_provider_tile"] <- list(NULL)
      }

      ls <- Filter(Negate(is.null), ls)

      r$params <- ls
    })


  })
}

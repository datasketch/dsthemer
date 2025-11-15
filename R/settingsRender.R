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
          data = '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="20" height="20" viewBox="0 0 20 20">
<path fill="none" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" stroke="currentColor" stroke-opacity="1" stroke-miterlimit="4" d="M 9 6 C 12.726562 6 15.75 4.992188 15.75 3.75 C 15.75 2.507812 12.726562 1.5 9 1.5 C 5.273438 1.5 2.25 2.507812 2.25 3.75 C 2.25 4.992188 5.273438 6 9 6 Z M 9 6 "/>
<path fill="none" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" stroke="currentColor" stroke-opacity="1" stroke-miterlimit="4" d="M 2.25 3.75 L 2.25 14.25 C 2.25 14.847656 2.960938 15.417969 4.226562 15.839844 C 5.492188 16.261719 7.210938 16.5 9 16.5 C 10.789062 16.5 12.507812 16.261719 13.773438 15.839844 C 15.039062 15.417969 15.75 14.847656 15.75 14.25 L 15.75 3.75 "/>
<path fill="none" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" stroke="currentColor" stroke-opacity="1" stroke-miterlimit="4" d="M 2.25 9 C 2.25 9.597656 2.960938 10.167969 4.226562 10.589844 C 5.492188 11.011719 7.210938 11.25 9 11.25 C 10.789062 11.25 12.507812 11.011719 13.773438 10.589844 C 15.039062 10.167969 15.75 9.597656 15.75 9 "/>
</svg>',
          titles = '<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
  <path d="M9 3V15" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
  <path d="M3 5.25V3.75C3 3.55109 3.07902 3.36032 3.21967 3.21967C3.36032 3.07902 3.55109 3 3.75 3H14.25C14.4489 3 14.6397 3.07902 14.7803 3.21967C14.921 3.36032 15 3.55109 15 3.75V5.25" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
  <path d="M6.75 15H11.25" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
</svg>',
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
      palette_types <- c("categorical", "sequential", "divergening")
      purrr::map(palette_types, function(palette) {
        dsthemer_palette(org, palette = palette) |> unlist()
      }) |>
        purrr::set_names(palette_types)
    })

    colors_list <- reactive({
      req(r$data)
      req(palette_colors())
      palette_colors <- palette_colors()
      purrr::imap(palette_colors, function(colors, palette) {
        as.character(div(purrr::map(colors, function(color) {
          div(style = paste0("width: 20px; height: 20px; display: inline-block; background-color:",
                             color, ";"))
        })))
      })
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


      purrr::walk(names(parmesan), function(section) {
        section_inputs <- parmesan[[section]]$inputs
        if (!is.null(section_inputs) && length(section_inputs) > 0) {
          purrr::walk(section_inputs, function(input_def) {
            if (!is.null(input_def$input_params)) {
              # Actualizar valores reactivos si son funciones ()
              purrr::iwalk(input_def$input_params, function(param_value, param) {
                if (!is.null(param_value) && is.character(param_value) &&
                    length(param_value) == 1 && grepl("\\(\\)$", param_value)) {
                  param_name <- gsub("\\(\\)$", "", param_value)
                  if (!is.null(r[[param_name]])) {
                    input_def$input_params[[param]] <<- r[[param_name]]
                  }
                }
              })
              # actualiza los inputs update... segun el widget
              if (!is.null(input[[input_def$id]])) {
                if (!any(grepl("\\(\\)$", input[[input_def$id]]))) {
                  r_parmesan$params[[input_def$id]] <- input[[input_def$id]]
                }
              }
            }
          })
        }
      })
    })

    observeEvent(input$color_palette, {
      if (!identical(input$color_palette, "agg_palette()")) {
        colors <- purrr::map(input$color_palette, function(x) list(x))
        if (!identical(r$agg_palette, colors)) {
          r$agg_palette <- colors
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Función auxiliar para renderizar inputs de una sección
    render_section_inputs <- function(section_name) {
      section_data <- parmesan[[section_name]]

      # Verificar si la sección tiene inputs
      if (is.null(section_data$inputs) || length(section_data$inputs) == 0) {
        return(tagList())
      }

      # Procesar cada input y agregar el tooltip al widget
      section_inputs <- purrr::map(section_data$inputs, function(input_def) {
        if (is.null(input_def$input_params)) return(NULL)

        input_params <- input_def$input_params

        if (!is.null(input_params)) {
          # Reemplazar valores específicos de los parámetros si es necesario
          input_params <- purrr::imap(input_params, function(param_value, param) {
            translated_value <- dstextui::translate(param_value, r$config_lang)
            if (!is.null(translated_value) && is.character(translated_value) &&
                length(translated_value) == 1 && grepl("\\(\\)$", translated_value)) {
              param_name <- gsub("\\(\\)$", "", translated_value)
              if (!is.null(r[[param_name]])) {
                return(r[[param_name]])
              }
            }
            param_value
          })

          # Actualizar parámetros desde r_parmesan$params
          if (!is.null(r_parmesan$params[[input_def$id]]) && input_def$id != "color_palette") {
            param_update <- parmesan_updates[[input_def$input_type]]$update_param
            input_params[[param_update]] <- r_parmesan$params[[input_def$id]]
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
        show_widget <- if (!is.null(input_def$show_if)) {
          param_condition <- gsub("\\(\\)$", "", input_def$show_if)
          r[[param_condition]] %||% TRUE
        } else {
          TRUE
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
          NULL
        }
      }) |>
        purrr::discard(is.null)

      do.call(tagList, section_inputs)
    }

    # Crear renderUI para cada sección
    purrr::walk(names(parmesan), function(section_name) {
      local({
        output[[paste0("section_", section_name)]] <- renderUI({
          req(r$data)
          render_section_inputs(section_name)
        })
      })
    })


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

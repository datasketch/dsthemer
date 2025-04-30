#' @export
settings_render_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      includeCSS(dsthemer_sys_file("lib/panelControl/panelControl.css"))
    ),

    uiOutput(ns("dynamic_inputs"))

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

    # Renderizar los inputs dinámicamente
    output$dynamic_inputs <- renderUI({
      req(r$data)
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
            <span class='tooltiptext'>", i_(section_data$description,  lang = r$lang(), i18n = r$i18n), "</span>
          </div>"
            )
          } else {
            ""
          }

          # Agregar el label y el tooltip al contenedor principal de la sección
          input_list <- append(input_list, list(HTML(
            paste0("<div class='section-parmesan' id='", section, "' style='display: flex; align-items: center; gap: 10px;'>",
                   i_(section_data$label, lang = r$lang(), i18n = r$i18n), tooltip_icon, "</div>")
          )))

          # Procesar cada input y agregar el tooltip al widget
          section_inputs <- lapply(as.list(section_data$inputs), function(input_def) {
            input_params <- input_def$input_params

            if (!is.null(input_params)) {
              # Reemplazar valores específicos de los parámetros si es necesario
              for (param in names(input_params)) {

                param_value <- i_(input_params[[param]], lang = r$lang(), i18n = r$i18n)
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
              <span class='tooltiptext'>", i_(input_def$description,  lang = r$lang(), i18n = r$i18n), "</span>
            </span>"
              )
            } else {
              ""
            }
            input_params$label <- HTML(paste(i_(input_params$label, lang = r$lang(), i18n = r$i18n), widget_tooltip))
            show_widget <- TRUE
            if (!is.null(input_def$show_if)) {
              param_condition <- gsub("\\(\\)$", "", input_def$show_if)
              show_widget <- r[[param_condition]] %||% TRUE
            }


            if ("choices" %in% names(input_params)) {
              names(input_params$choices) <- i_(names(input_params$choices),  lang = r$lang(), i18n = r$i18n)
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

      ls <- Filter(Negate(is.null), ls)

      if (!"map_tiles" %in% names(ls)) {
        ls["map_provider_tile"] <- list(NULL)
      }

      r$params <- ls
    })


  })
}

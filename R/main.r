run_example <- function() {
  example_file_path <-system.file("examples/highered", package = "ipumsenumview")
  # Get list of surveys
  surveys <- dir(example_file_path) %>%
    str_replace_all("enum_form_(.+)\\.html", "\\1")

  # Set global options to be referenced. By making them an environment,
  # calls from inside shiny app can find them.

  global_opts <- new.env()
  global_opts$survey_info <- data_frame(
    survey = surveys,
    html = map(survey, ~xml_children(xml_find_first(read_html(paste0(example_file_path, "/enum_form_", ., ".html")), "/html/body"))),
    text = map(html, xml_text),
    id = map(html, ~map_chr(., ~xml_attr(., "id")))
  )

  js_shiny_extend <- "
shinyjs.scrollToAnchor = function(aid){
var aTag = $(\"div[id='\"+ aid +\"']\");

current_pos = $('div.scrollable').scrollTop();
$('div.scrollable').scrollTop(0);
new_pos = aTag[0].offsetParent.offsetTop - 175;
$('div.scrollable').scrollTop(current_pos);
$('div.scrollable').animate({scrollTop: new_pos},'fast')
};
"

  js_enter_search <- HTML("
                        $(document).keyup(function(event) {
                        if ($(\"#search_terms\").is(\":focus\") && (event.keyCode == 13)) {
                        $(\"#search_button\").click();
                        }
                        });
                        ")

  find_in_text <- function(survey_text, search_terms) {
    out <- map(search_terms, ~str_detect(survey_text, coll(., ignore_case = TRUE)))
    out <- which(reduce(out, `&`))
    out
  }

  search_in_all <- function(search_terms) {
    out <- map(global_opts$survey_info$text, ~find_in_text(., search_terms))
    names(out) <- global_opts$survey_info$survey
    out
  }

  nchild_to_ids <- function(positions, survey, clean_missing = FALSE) {
    ids <- global_opts$survey_info$id[[which(global_opts$survey_info$survey == survey)]][positions]
    if (clean_missing) {
      ids[is.na(ids)] <- "<no id>"
    }
    ids
  }

  ids_to_nchild <- function(ids, survey, ignore_missing = FALSE) {
    pos <- global_opts$survey_info$id[[which(global_opts$survey_info$survey == survey)]]
    if (ignore_missing) {
      pos <- pos[!is.na(pos)]
    }
    match(ids, pos)
  }

  search_cb_to_main_cb <- function(survey, search_selected, main_selected, search_results) {
    all_cb_ids <- global_opts$survey_info$id[[which(global_opts$survey_info$survey == survey)]]

    if (is.null(search_selected)) search_selected <- character(0)
    if (is.null(main_selected)) main_selected <- character(0)
    search_ids <- all_cb_ids[search_results]
    nonselected_search_ids <- setdiff(search_ids, search_selected)

    out <- main_selected
    out <- setdiff(out, nonselected_search_ids)
    out <- unique(c(out, search_selected))

    out
  }

  main_cb_to_search_cb <- function(survey, main_selected, search_results) {
    all_cb_ids <- global_opts$survey_info$id[[which(global_opts$survey_info$survey == survey)]]
    if (is.null(main_selected)) main_selected <- character(0)
    search_ids <- all_cb_ids[search_results]
    intersect(search_ids, main_selected)
  }

  parse_search_term_input <- function(x) {
    if (x == "" | is.null(x)) return(NULL)
    scan(text = x,  what = "character", quiet = TRUE)
  }

  get_enum_html <- function(survey_name, add_tag_labels = FALSE,
                            add_checkboxes = FALSE, checkbox_default = NULL,
                            input_suffix = "", subset = NULL,
                            remove_ids = FALSE) {
    html <- global_opts$survey_info$html[global_opts$survey_info$survey == survey_name][[1]]
    out <- html

    if (!is.null(subset)) {
      out <- out[subset]
    }

    # Get info on the xml structure while it is still an xml document
    # Because adding nodes in xml2 caused crashes and was slow, most work is done
    # on character vectors. This could probably be improved.
    if (add_tag_labels | add_checkboxes) {
      # tag labels will be added to all of text divs with ids
      child_names <- xml_find_chr(out, "name(.)")
      child_classes <- xml_attr(out, "class")
      child_ids <- xml_attr(out, "id")
    }

    # Convert to character vector
    out <- map_chr(out, ~as.character(.))

    if (add_tag_labels) {
      tag_labels <- rep("", length(child_names))
      needs_tag_labels <- child_names == "div" & (child_classes == "text" & !is.na(child_classes)) & !is.na(tag_labels)
      tag_labels[needs_tag_labels] <-
        paste0("<span class='label label-primary'>",
               remove_sname_from_id(child_ids[needs_tag_labels]),
               "</span>")

      out <- str_replace(out, "(<div class=\"text\" id=\".+\">)", paste0("\\1", tag_labels))
    }

    if (remove_ids) {
      out <- str_replace(out, "(id=\".+\")", "")
    }

    if (add_checkboxes) {
      needs_checkbox <- child_names == "div" & (child_classes == "text" & !is.na(child_classes)) & !is.na(tag_labels)

      names(out) <- child_ids
      custom_cb_group_input(paste0(survey_name, input_suffix), "", out, needs_checkbox,
                            selected = checkbox_default)
    } else {
      HTML(out)
    }

  }

  custom_cb_group_input <- function (inputId, label, tags, needs_cb, selected = NULL,
                                     width = NULL) {
    selected <- shiny::restoreInput(id = inputId, default = selected)
    selected <- names(tags) %in% selected

    options <- custom_gen_options(inputId, names(tags), tags, needs_cb, selected)
    divClass <- "form-group shiny-input-checkboxgroup shiny-input-container"
    shiny::tags$div(id = inputId, style = if (!is.null(width))
      paste0("width: ", shiny::validateCssUnit(width), ";"), class = divClass,
      shiny:::controlLabel(inputId, label), options)
  }

  custom_gen_options <- function(inputId, tag_names, tag_obj, needs_cb, selected) {

    options <- pmap(list(node_name = tag_names, node = tag_obj, need = needs_cb,
                         checked = selected),
                    function(node_name, node, need, checked) {
                      if (need) {
                        inputTag <- tags$input(type = "checkbox", name = inputId, value = node_name)

                        if (checked) inputTag$attribs$checked <- "checked"

                        tags$div(class = "checkbox", tags$label(inputTag, tags$span(HTML(node))))
                      } else {
                        HTML(node)
                      }
                    })

    div(class = "shiny-options-group", options)
  }

  make_empty_panel <- function() {
    hidden(
      div(
        id = "hidden_panel",
        wellPanel(tags$h3("No surveys selected.")),
        style = "margin-left:0.5em;float:left"
      )
    )
  }

  make_search_results_panel <- function() {
    hidden(
      div(
        id = "search_results",
        tags$div(
          wellPanel(uiOutput("search_results_panel")),
          style = "overflow-y:scroll;width:100%;height:75vh;height: calc(100vh - 15em);"
        ),
        style = "margin-left:0.5em;float:left;width:100%"
      )
    )
  }

  make_enum_panel <- function() {
    hidden(
      div(
        id = "single_year",
        tags$div(
          uiOutput("single_year_panel"),
          style = "overflow-y:scroll;width:100%;height:75vh;height: calc(100vh - 15em);",
          class = "scrollable"
        ),
        style = "margin-left:0.5em;float:left;width:100%;min-height:1px;"
      )
    )
  }

  switch_to_panels <- function(panel_id) {
    if (is.null(panel_id)) panel_id <- "missing_panel"
    panel_id[panel_id != "Search results"] <- "single_year"
    panel_id[panel_id == "Search results"] <- "search_results"
    walk(c("search_results", "single_year"), ~hide(.))
    show(panel_id)
  }

  make_scroll_script <- function(tag) {
    if (tag == "") {
      out <- "$('div.scrollable').scrollTop(0);"
    } else {
      out <- paste(
        paste0("var aTag = $(\"div[id='\"+ \"", tag, "\" +\"']\");"),
        "current_pos = $('div.scrollable').scrollTop();",
        "$('div.scrollable').scrollTop(0);",
        "new_pos = aTag[0].offsetParent.offsetTop - 175;",
        "$('div.scrollable').scrollTop(current_pos);",
        "$('div.scrollable').animate({scrollTop: new_pos},'fast')",
        sep = "\n"
      )
    }
    HTML(out)
  }

  clear_all_observers <- function(obs_env) {
    c_obs <- ls(envir = obs_env)
    walk(c_obs, ~obs_env[[.]]$destroy())
    rm(list = c_obs, envir = obs_env)

  }

  make_new_observed_links <- function(link_names, link_texts, observer_func,
                                      obs_env, input, ...) {
    walk(link_names, function(lll) {
      obs_env[[lll]] <- observeEvent(input[[lll]], observer_func(lll, ...))
    })

    map2(link_names, link_texts, ~actionLink(.x, .y))
  }

  remove_sname_from_id <- function(ids) {
    str_replace(ids, "^(.+)-(.+)$", "\\2")
  }

  load_import_file <- safely(quietly(function(fff) {
    data <- read_csv(fff, col_types = "cc")

    out <- str_split(data$tags, ";")
    out <- map2(out, data$survey, function(x,  y) {
      if (!all(is.na(x))) {
        paste0(y, "-", x)
      } else {
        character(0)
      }
    })
    names(out) <- data$survey
    out
  }))

  # ------ search_tab ------
  search_tab <- {tabPanel(
    "Search",
    fillPage(
      tags$div(
        wellPanel(
          div(style="display:inline-block", textInput("search_terms", NULL)),
          div(style="display:inline-block",
              actionButton("search_button", "Search", class = "btn-primary")),
          class = "well-sm"),
        wellPanel(
          uiOutput("search_results_table"), class = "well-sm",
          style = "overflow-y:scroll;height:70vh;height: calc(100vh - 15em);"
        ),
        style = "width:24%;float:left"
      ),
      tags$div(
        wellPanel(
          selectizeInput("survey_views", NULL,
                         c("Search results", global_opts$survey_info$survey),
                         selected = "Search results", width = "50%"
          ),
          make_empty_panel(),
          make_search_results_panel(),
          make_enum_panel(),
          style = "margin-left:1%;width:74%;float:left;")
      )
    )
  )}


  # ------ review_tab ------
  review_tab <- {tabPanel(
    "Review",
    fillPage(
      tags$div(
        style = "width:24%;float:left;",
        wellPanel(
          div(style="display:inline-block",
              textInput("file_name", NULL, width = "150")
          ),
          actionButton("export", "Export as csv", class = "btn-success"),
          uiOutput("review_export_msg"),
          actionButton("clear", "Clear all tagged", class = "btn-danger")

        ),
        wellPanel(
          uiOutput("review_results_table"),
          style = "overflow-y:scroll;height:45vh;height: calc(100vh - 29em);"
        ),
        wellPanel(
          fileInput("import", "Import from file", accept = "text/csv"),
          uiOutput("review_import_msg")
        )
      ),
      tags$div(
        style = "width:74%;float:left;margin-left:1%",
        wellPanel(uiOutput("review_results_full"),
                  style = "overflow-y:scroll;height:90vh;height: calc(100vh - 7em)")
      )
    )
  )}

  # ------ App ------
  shinyApp(
    ui = navbarPage(
      "EnumView",
      theme = shinytheme("flatly"),
      header = div(
        tags$script(js_enter_search),
        useShinyjs(),
        extendShinyjs(text = js_shiny_extend),
        tags$style(type='text/css', ".selectize-input {font-size: 32px ; line-height: 36px;}")
      ),
      search_tab,
      review_tab
    ),
    server = function(input, output, session) {
      cb_search_observers <- new.env(parent = emptyenv())
      cb_year_observers <- new.env(parent = emptyenv())
      link_observers <- new.env(parent = emptyenv())

      observe({
        switch_to_panels(input$survey_views)
      })

      search_results <- reactiveValues(results = vector(length(global_opts$survey_info$survey), mode = "list"))
      selected_tags <- do.call("reactiveValues",
                               set_names(map(global_opts$survey_info$survey, ~character(0)), global_opts$survey_info$survey)
      )

      export_file <- reactiveValues(file_name = "", error = FALSE, bit_flip = FALSE)
      import_file <- reactiveValues(file_name = "", error = FALSE)

      scroll_pos <- reactiveValues(current_page = "", new_page = "")

      observeEvent(input$search_button, {
        search_terms <- input$search_terms

        if (search_terms != "") {
          search_terms <- parse_search_term_input(search_terms)
          search_results$results <- search_in_all(search_terms)
        }
      })

      output$single_year_panel <- renderUI({
        current_view <- input$survey_views
        surveys <- global_opts$survey_info$survey
        # isolate to avoid infinite loop and shouldn't ever need to rerun
        # based only on new scroll pos (avoided for speed)
        isolate(newpage_pos <- scroll_pos$new_page)
        isolate(scroll_pos$new_page <- "")
        # Also have observers to update checkbox so isolate on value change
        isolate(s_tags <- selected_tags[[current_view]])
        if (is.null(s_tags)) s_tags <- character(0)
        clear_all_observers(cb_year_observers)

        if (!current_view %in% surveys) {
          out <- tags$div(style = "margin-left:0.5em;float:left;width:100%")
        } else {
          out <- tags$div(
            get_enum_html(current_view, add_tag_labels = TRUE, add_checkboxes = TRUE,
                          checkbox_default = s_tags, input_suffix = "_full"),
            tags$script(make_scroll_script(newpage_pos))
          )

          cb_year_observers[["from_selected"]] <- observeEvent(selected_tags[[current_view]], {
            new_selected <- selected_tags[[current_view]]
            if (is.null(new_selected)) new_selected <- character(0)
            updateCheckboxGroupInput(session, paste0(current_view, "_full"), selected = new_selected)
          }, ignoreNULL = FALSE)

          cb_year_observers[["from_year"]] <- observeEvent(input[[paste0(current_view, "_full")]], {
            new_selected <- input[[paste0(current_view, "_full")]]
            if (is.null(new_selected)) new_selected <- character(0)
            selected_tags[[current_view]] <- new_selected
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
        }
        out
      })

      output$search_results_panel <- renderUI({
        search_tags <- search_results$results
        surveys <- global_opts$survey_info$survey
        # Observers will handle this after creation so we can isolate
        isolate(selected_cb <- map(surveys, ~selected_tags[[.]]))
        names(selected_cb) <- surveys

        out <- tags$div(
          map2(search_tags, global_opts$survey_info$survey, function(ttt, sss) {
            if (length(ttt) == 0) {
              out_body <- "---"
            } else {
              sel_cb <- selected_cb[[sss]]
              if (is.null(sel_cb)) sel_cb <- character(0)

              out_body <- get_enum_html(sss, add_tag_labels = TRUE, add_checkboxes = TRUE, subset = ttt,
                                        remove_ids = TRUE, input_suffix = "_search",
                                        checkbox_default = sel_cb)
            }
            tags$div(tags$h4(sss, class = "text-info"), out_body, tags$hr())
          })
        )

        # Destroy old and then create new check box observers for search panel.
        clear_all_observers(cb_search_observers)

        walk(global_opts$survey_info$survey, function(sss) {
          force(sss)
          cb_search_observers[[paste0(sss, "_from_selected")]] <- observeEvent(selected_tags[[sss]], {
            sr <- search_results$results[[sss]]
            if (!is.null(sr)) {
              new_selected <- main_cb_to_search_cb(sss, selected_tags[[sss]], sr)
              if (is.null(new_selected)) new_selected <- character(0)
              updateCheckboxGroupInput(session, paste0(sss, "_search"), selected = new_selected)
            }
          }, ignoreNULL = FALSE)

          cb_search_observers[[paste0(sss, "_from_search")]] <- observeEvent(input[[paste0(sss, "_search")]], {
            sr <- search_results$results[[sss]]
            if (!is.null(sr)) {
              new_selected <- search_cb_to_main_cb(sss, input[[paste0(sss, "_search")]],
                                                   selected_tags[[sss]], sr)
              if (is.null(new_selected)) new_selected <- character(0)
              selected_tags[[sss]] <- new_selected
            }
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
        })

        out
      })

      output$search_results_table <- renderUI({
        results <- search_results$results

        all_surveys <- global_opts$survey_info$survey

        clear_all_observers(link_observers)
        results_tag <- map2(results, all_surveys,
                            function(.x, .y) {
                              if (length(.x) == 0) return(tags$span("-"))
                              search_ids <- nchild_to_ids(.x, .y, clean_missing = TRUE)
                              tagList(make_new_observed_links(search_ids, remove_sname_from_id(search_ids),
                                                              function(x, session) {
                                                                args <- str_match(x, "^(.+)-(.+)$")

                                                                if (args[2] != input$survey_views) {
                                                                  updateSelectInput(session, "survey_views", selected = args[2])
                                                                  scroll_pos$new_page <- args[1]
                                                                } else {
                                                                  js$scrollToAnchor(args[1])
                                                                }
                                                              }, link_observers, input, session = session))
                            })



        out_table <- dplyr::data_frame(
          survey = all_surveys,
          tags = results_tag
        )

        out_rows <- by_row(out_table, ~tags$tr(tags$td(.$survey, style = "font-size:11px"),
                                               tags$td(.$tags, style = "font-size:11px")))
        tags$table(
          tags$thead(
            tags$tr(tags$th("Survey"),
                    tags$th("Tags"))
          ),
          tagList(out_rows$.out),
          class = "table table-striped table-hover table-condensed")
      })

      output$review_results_table <- renderUI({
        all_surveys <- global_opts$survey_info$survey

        results <- map(all_surveys, ~selected_tags[[.]])

        out_table <- dplyr::data_frame(
          survey = all_surveys,
          tags = map_chr(results, ~paste(remove_sname_from_id(.), collapse = ", "))
        )

        out_rows <- by_row(out_table, ~tags$tr(tags$td(.$survey, style = "font-size:11px"),
                                               tags$td(.$tags, style = "font-size:11px")))
        tags$table(
          tags$thead(
            tags$tr(tags$th("Survey"),
                    tags$th("Tags"))
          ),
          tagList(out_rows$.out),
          class = "table table-striped table-hover table-condensed")
      })

      output$review_results_full <- renderUI({
        all_surveys <- global_opts$survey_info$survey

        results <- map(all_surveys, ~selected_tags[[.]])
        result_nchilds <- map2(results, all_surveys,
                               ~ids_to_nchild(.x, .y))

        enum <- map2(result_nchilds, all_surveys,
                     ~get_enum_html(.y, subset = .x, remove_ids = TRUE))
        enum <- map2(all_surveys, enum, ~list(tags$h3(.x, class = "text-info"), .y))
        enum
      })

      output$review_export_msg <- renderUI({
        error <- export_file$error
        file_name <- export_file$file_name
        bit_flip <- export_file$bit_flip # Just used to rerun even if filename is repeated

        if (error) {
          div(tags$button(type = "button", class = "close",
                          `data-dismiss` = "alert", "x"),
              tags$small("Could not export file."),
              class = "alert alert-dismissible alert-danger"
          )
        } else if (file_name != "") {
          div(tags$button(type = "button", class = "close",
                          `data-dismiss` = "alert", "x"),
              tags$small(paste0("Exported to '", file_name, "'")),
              class = "alert alert-dismissible alert-success"
          )
        }
      })

      # Clear button observer
      observeEvent(input[["clear"]], {
        walk(
          global_opts$survey_info$survey,
          function(sss) {
            selected_tags[[sss]] <- character(0)
          })
      })

      # Export button observer
      observeEvent(input[["export"]], {
        file_name <- input[["file_name"]]
        if (length(file_name) > 0 & file_name != "") {
          full_file_name <- paste0(getwd(), "/exports/", file_name, ".csv")
          out <- dplyr::data_frame(
            survey = global_opts$survey_info$survey,
            tags = map_chr(survey, ~paste(remove_sname_from_id(selected_tags[[.]]),
                                          collapse = ";"))
          )

          readr::write_csv(out, full_file_name, na = "")

          export_file$error <- FALSE
          export_file$file_name <- full_file_name
          export_file$bit_flip <- !export_file$bit_flip
        } else {
          export_file$error <- TRUE
          export_file$bit_flip <- !export_file$bit_flip
        }
      })

      # Import file observer
      observeEvent(input[["import"]], {
        tags <- load_import_file(input$import$datapath)

        import_file$file_name <- input$import$name

        if (!is.null(tags$error)) {
          import_file$error <- TRUE
        } else {
          import_file$error <- FALSE
          tags <- tags$result$result
          walk(
            global_opts$survey_info$survey,
            function(sss) {
              sel_tags <- tags[[sss]]
              if (is.null(sel_tags)) sel_tags <- character(0)
              selected_tags[[sss]] <- sel_tags
            })
        }
      })

      output$review_import_msg <- renderUI({
        error <- import_file$error
        file_name <- import_file$file_name

        if (error) {
          div(tags$button(type = "button", class = "close",
                          `data-dismiss` = "alert", "x"),
              tags$small("Could not import file."),
              class = "alert alert-dismissible alert-danger"
          )
        } else if (file_name != "") {
          div(tags$button(type = "button", class = "close",
                          `data-dismiss` = "alert", "x"),
              tags$small(paste0("Imported from '", file_name, "'")),
              class = "alert alert-dismissible alert-success"
          )
        }
      })


    })
}

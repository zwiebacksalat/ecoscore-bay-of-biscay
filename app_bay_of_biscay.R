# EcoScore - Bay of Biscay online multi-user assessment
# Adapted from the existing EcoScore online prototype.
# Scores are stored centrally in PostgreSQL/Supabase.
#
# Required environment variables on the server:
#   DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASSWORD
#   DB_SSLMODE (usually require)
#
# Never put database credentials in this file or in GitHub.

library(shiny)
library(DT)
library(dplyr)
library(DBI)
library(RPostgres)

# =========================================================
# MASTER DATA
# =========================================================

pressures <- read.csv("pressures_BOBIC.csv",
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
components <- read.csv("components_BOBIC.csv",
                       stringsAsFactors = FALSE,
                       check.names = FALSE)
subregions <- read.csv("subregions_BOBIC.csv",
                       stringsAsFactors = FALSE,
                       check.names = FALSE)

# =========================================================
# SCORING DEFINITIONS
# =========================================================

overlap_levels <- data.frame(
  label = c("Exogenous", "Site", "Local", "Widespread patchy", "Widespread even"),
  score = c(1, 3, 33, 67, 100),
  description = c(
    "The activity occurs outside of the area occupied by the ecosystem component, but one or more pressures reach the ecosystem component through dispersal.",
    "The activity overlaps with the ecosystem component by between 1 and 5% of the occupied area.",
    "The activity overlaps with the ecosystem component by between 5 and 50% of the occupied area.",
    "The activity overlaps with the ecosystem component by between 50 and 100% of the occupied area, but distribution is patchy.",
    "The activity overlaps with the ecosystem component by between 50 and 100% of the occupied area and is evenly distributed."
  ), stringsAsFactors = FALSE
)

frequency_overlap_levels <- data.frame(
  label = c("Rare", "Occasional", "Common", "Persistent"),
  score = c(8, 33, 67, 100),
  description = c(
    "The frequency is 1 month a year.",
    "The frequency is 1 to 4 months a year.",
    "The frequency is more than 4 months a year.",
    "Occurs every month of the year."
  ), stringsAsFactors = FALSE
)

degree_impact_levels <- data.frame(
  label = c("Low", "Chronic", "Acute"),
  score = c(5, 20, 100),
  description = c(
    "Pressures are not considered to (currently) cause (sub)population level/functional groups effects.",
    "Pressures may have a population-level/functional effect if they have high enough spatial and/or temporal occurrence (i.e. chronic nature).",
    "(Immediate) impacts are expected/known to occur."
  ), stringsAsFactors = FALSE
)

categories <- c("Overlap", "Frequency of overlap", "Degree of impact")

# =========================================================
# DATABASE CONNECTION
# =========================================================

required_env <- c("DB_HOST", "DB_PORT", "DB_NAME", "DB_USER", "DB_PASSWORD")
missing_env <- required_env[!nzchar(Sys.getenv(required_env, unset = ""))]
if (length(missing_env) > 0) {
  stop(paste("Missing database environment variables:", paste(missing_env, collapse = ", ")))
}

db_con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname = Sys.getenv("DB_NAME", "postgres"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode = Sys.getenv("DB_SSLMODE", "require")
)

onStop(function() {
  if (DBI::dbIsValid(db_con)) dbDisconnect(db_con)
})

# =========================================================
# DATABASE HELPERS
# =========================================================

read_scores <- function(con) {
  dbGetQuery(con, paste(
    "SELECT row_id, subregion, pressure, component,",
    "overlap, overlap_score, frequency_overlap, frequency_overlap_score,",
    "degree_impact, degree_impact_score, comments, updated_by, updated_at",
    "FROM assessment_scores",
    "ORDER BY subregion, pressure, component"
  ))
}

seed_scores <- function(con) {
  combinations <- expand.grid(
    subregion = subregions$subregion,
    pressure = pressures$pressure,
    component = components$component,
    stringsAsFactors = FALSE
  )

  temp_name <- paste0("assessment_seed_", as.integer(Sys.getpid()))
  dbWriteTable(
    con, name = temp_name, value = combinations,
    temporary = TRUE, overwrite = TRUE, row.names = FALSE
  )

  dbExecute(con, paste0(
    "INSERT INTO assessment_scores (subregion, pressure, component) ",
    "SELECT subregion, pressure, component FROM \"", temp_name, "\" ",
    "ON CONFLICT (subregion, pressure, component) DO NOTHING"
  ))
}

seed_scores(db_con)

# =========================================================
# UI
# =========================================================

ui <- fluidPage(
  tags$head(tags$style(HTML("\n    .sticky-panel {\n      position: sticky;\n      top: 0;\n      background-color: white;\n      z-index: 1000;\n      padding-bottom: 10px;\n      border-bottom: 2px solid #dddddd;\n    }\n    .user-info {\n      margin-bottom: 10px;\n      color: #555555;\n    }\n  "))),
  titlePanel("EcoScore - Bay of Biscay Assessment"),
  sidebarLayout(
    sidebarPanel(width = 4, div(class = "sticky-panel", uiOutput("main_ui"))),
    mainPanel(DTOutput("table"))
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {

  current_user <- reactive({
    if (is.null(session$user) || !nzchar(session$user)) "local_user" else session$user
  })

  scoring_started <- reactiveVal(FALSE)
  chosen_categories <- reactiveVal(character(0))
  rv <- reactiveVal(read_scores(db_con))
  advance_after_save <- reactiveVal(FALSE)
  last_saved_component <- reactiveVal(NULL)

  output$main_ui <- renderUI({
    if (!scoring_started()) {
      return(tagList(
        div(class = "user-info", paste("User:", current_user())),
        h4("Choose criteria to score"),
        checkboxGroupInput("selected_categories", NULL,
                           choices = categories, selected = character(0)),
        actionButton("start_scoring", "OK - Start scoring")
      ))
    }

    selected <- chosen_categories()
    tagList(
      div(class = "user-info", paste("User:", current_user())),
      actionButton("change_categories", "← Change criteria"),
      actionButton("refresh_data", "Refresh shared data"),
      br(), br(),
      selectInput("selected_subregion", "Select subregion",
                  choices = sort(unique(subregions$subregion))),
      checkboxInput("show_missing", "Show only incomplete combinations", value = TRUE),
      selectInput("selected_pressure", "Select pressure", choices = NULL),
      selectInput("selected_component", "Select ecosystem component", choices = NULL),
      hr(),
      if ("Overlap" %in% selected) tagList(
        h3("Overlap"),
        radioButtons("overlap", NULL, choices = stats::setNames(overlap_levels$label, overlap_levels$label), selected = character(0)),
        verbatimTextOutput("overlap_description"),
        checkboxInput("apply_overlap_all", "Apply Overlap to all subregions", value = FALSE),
        hr()
      ),
      if ("Frequency of overlap" %in% selected) tagList(
        h3("Frequency of overlap"),
        radioButtons("frequency_overlap", NULL,
                     choices = stats::setNames(frequency_overlap_levels$label, frequency_overlap_levels$label),
                     selected = character(0)),
        verbatimTextOutput("frequency_overlap_description"),
        checkboxInput("apply_frequency_overlap_all", "Apply Frequency of overlap to all subregions", value = FALSE),
        hr()
      ),
      if ("Degree of impact" %in% selected) tagList(
        h3("Degree of impact"),
        radioButtons("degree_impact", NULL,
                     choices = stats::setNames(degree_impact_levels$label, degree_impact_levels$label),
                     selected = character(0)),
        verbatimTextOutput("degree_impact_description"),
        checkboxInput("apply_degree_impact_all", "Apply Degree of impact to all subregions", value = FALSE),
        hr()
      ),
      textAreaInput("comments", "Comments", rows = 4),
      actionButton("save_score", "Save score")
    )
  })

  observeEvent(input$start_scoring, {
    req(length(input$selected_categories) > 0)
    chosen_categories(input$selected_categories)
    scoring_started(TRUE)
  })

  observeEvent(input$change_categories, {
    scoring_started(FALSE)
    chosen_categories(character(0))
  })

  observeEvent(input$refresh_data, {
    req(scoring_started())
    rv(read_scores(db_con))
    showNotification("Shared data refreshed.", type = "message")
  })

  filtered_data <- reactive({
    req(scoring_started(), input$selected_subregion)
    df <- rv() %>% filter(subregion == input$selected_subregion)
    selected <- chosen_categories()

    if (isTRUE(input$show_missing) && length(selected) > 0) {
      incomplete <- rep(FALSE, nrow(df))
      if ("Overlap" %in% selected) incomplete <- incomplete | is.na(df$overlap)
      if ("Frequency of overlap" %in% selected) incomplete <- incomplete | is.na(df$frequency_overlap)
      if ("Degree of impact" %in% selected) incomplete <- incomplete | is.na(df$degree_impact)
      df <- df[incomplete, , drop = FALSE]
    }
    df
  })

  observe({
    req(scoring_started())
    df <- filtered_data()
    pressure_choices <- sort(unique(df$pressure))
    current_pressure <- input$selected_pressure
    selected_pressure <- if (!is.null(current_pressure) && length(current_pressure) > 0 &&
                             current_pressure %in% pressure_choices) current_pressure else character(0)
    updateSelectInput(session, "selected_pressure", choices = pressure_choices, selected = selected_pressure)
  })

  observe({
    req(scoring_started(), input$selected_pressure)
    df <- filtered_data() %>% filter(pressure == input$selected_pressure)
    component_choices <- sort(unique(df$component))

    if (length(component_choices) == 0) {
      updateSelectInput(session, "selected_component", choices = character(0), selected = character(0))
      return()
    }

    current_component <- input$selected_component
    if (isTRUE(advance_after_save())) {
      saved_component <- last_saved_component()
      if (!is.null(saved_component) && length(saved_component) > 0 && saved_component %in% component_choices) {
        after_saved <- component_choices[component_choices > saved_component]
        selected_component <- if (length(after_saved) > 0) after_saved[1] else component_choices[1]
      } else {
        selected_component <- component_choices[1]
      }
      advance_after_save(FALSE)
      last_saved_component(NULL)
    } else if (!is.null(current_component) && length(current_component) > 0 && current_component %in% component_choices) {
      selected_component <- current_component
    } else {
      selected_component <- character(0)
    }

    updateSelectInput(session, "selected_component", choices = component_choices, selected = selected_component)
  })

  observe({
    req(scoring_started(), input$selected_subregion, input$selected_pressure, input$selected_component)
    row <- dbGetQuery(
      db_con,
      paste("SELECT * FROM assessment_scores",
            "WHERE subregion = $1 AND pressure = $2 AND component = $3"),
      params = list(input$selected_subregion, input$selected_pressure, input$selected_component)
    )
    if (nrow(row) != 1) return()

    if ("Overlap" %in% chosen_categories()) {
      updateRadioButtons(session, "overlap", selected = if (is.na(row$overlap)) character(0) else row$overlap)
    }
    if ("Frequency of overlap" %in% chosen_categories()) {
      updateRadioButtons(session, "frequency_overlap", selected = if (is.na(row$frequency_overlap)) character(0) else row$frequency_overlap)
    }
    if ("Degree of impact" %in% chosen_categories()) {
      updateRadioButtons(session, "degree_impact", selected = if (is.na(row$degree_impact)) character(0) else row$degree_impact)
    }
    updateTextAreaInput(session, "comments", value = if (is.na(row$comments)) "" else row$comments)
  })

  output$overlap_description <- renderText({
    req(input$overlap)
    x <- overlap_levels %>% filter(label == input$overlap)
    paste0("Score: ", x$score, "\n\n", x$description)
  })
  output$frequency_overlap_description <- renderText({
    req(input$frequency_overlap)
    x <- frequency_overlap_levels %>% filter(label == input$frequency_overlap)
    paste0("Score: ", x$score, "\n\n", x$description)
  })
  output$degree_impact_description <- renderText({
    req(input$degree_impact)
    x <- degree_impact_levels %>% filter(label == input$degree_impact)
    paste0("Score: ", x$score, "\n\n", x$description)
  })

  observeEvent(input$save_score, {
    req(scoring_started(), input$selected_subregion, input$selected_pressure, input$selected_component)
    selected <- chosen_categories()
    exact_updates <- list()
    add_exact <- function(column, value) {
      if (!is.null(value) && length(value) > 0) exact_updates[[column]] <<- value
    }

    if ("Overlap" %in% selected && length(input$overlap) > 0) {
      add_exact("overlap", input$overlap)
      add_exact("overlap_score", overlap_levels$score[overlap_levels$label == input$overlap])
    }
    if ("Frequency of overlap" %in% selected && length(input$frequency_overlap) > 0) {
      add_exact("frequency_overlap", input$frequency_overlap)
      add_exact("frequency_overlap_score", frequency_overlap_levels$score[frequency_overlap_levels$label == input$frequency_overlap])
    }
    if ("Degree of impact" %in% selected && length(input$degree_impact) > 0) {
      add_exact("degree_impact", input$degree_impact)
      add_exact("degree_impact_score", degree_impact_levels$score[degree_impact_levels$label == input$degree_impact])
    }
    if (!is.null(input$comments) && nchar(input$comments) > 0) add_exact("comments", input$comments)

    if (length(exact_updates) == 0) {
      showNotification("No score or comment was entered.", type = "warning")
      return()
    }

    dbWithTransaction(db_con, {
      criteria <- list(
        "Overlap" = list(label = input$overlap, score = if (length(input$overlap) > 0) overlap_levels$score[overlap_levels$label == input$overlap] else NULL,
                          value_col = "overlap", score_col = "overlap_score", apply_all = isTRUE(input$apply_overlap_all)),
        "Frequency of overlap" = list(label = input$frequency_overlap, score = if (length(input$frequency_overlap) > 0) frequency_overlap_levels$score[frequency_overlap_levels$label == input$frequency_overlap] else NULL,
                                       value_col = "frequency_overlap", score_col = "frequency_overlap_score", apply_all = isTRUE(input$apply_frequency_overlap_all)),
        "Degree of impact" = list(label = input$degree_impact, score = if (length(input$degree_impact) > 0) degree_impact_levels$score[degree_impact_levels$label == input$degree_impact] else NULL,
                                  value_col = "degree_impact", score_col = "degree_impact_score", apply_all = isTRUE(input$apply_degree_impact_all))
      )

      for (category in selected) {
        item <- criteria[[category]]
        if (is.null(item$label) || length(item$label) == 0 || is.null(item$score) || length(item$score) == 0) next

        if (item$apply_all) {
          dbExecute(db_con,
            paste0("UPDATE assessment_scores SET ", item$value_col, " = $1, ", item$score_col,
                   " = $2, updated_by = $3, updated_at = now() WHERE pressure = $4 AND component = $5"),
            params = list(item$label, item$score, current_user(), input$selected_pressure, input$selected_component)
          )
        } else {
          dbExecute(db_con,
            paste0("UPDATE assessment_scores SET ", item$value_col, " = $1, ", item$score_col,
                   " = $2, updated_by = $3, updated_at = now() WHERE subregion = $4 AND pressure = $5 AND component = $6"),
            params = list(item$label, item$score, current_user(), input$selected_subregion,
                          input$selected_pressure, input$selected_component)
          )
        }
      }

      if (!is.null(input$comments) && nchar(input$comments) > 0) {
        dbExecute(db_con,
          paste("UPDATE assessment_scores SET comments = $1, updated_by = $2, updated_at = now()",
                "WHERE subregion = $3 AND pressure = $4 AND component = $5"),
          params = list(input$comments, current_user(), input$selected_subregion,
                        input$selected_pressure, input$selected_component)
        )
      }
    })

    last_saved_component(input$selected_component)
    advance_after_save(TRUE)
    rv(read_scores(db_con))
    showNotification(paste("Saved by", current_user()), type = "message")
  })

  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)

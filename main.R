#
# app.R
#
# Shiny app that:
#  1. Reads a CSV of airports (with columns: ID, Name, City, Country, IATA, ICAO, …).
#  2. Presents a selectizeInput allowing the user to search by airport name or city
#     (displayed as "Name (City)"), but under the hood only the ICAO code is propagated.
#     Uses server-side selectize for performance and starts with no selection.
#  3. Fetches raw METAR & raw TAF from AviationWeather.gov using the selected ICAO.
#  4. Sends each raw METAR/TAF string to FlightPlanDatabase.com to decode into a JSON array
#     of { "string", "description" }.
#  5. Displays, for each METAR:
#       • The human‐readable observation time (converted from obsTime)
#       • The raw METAR text
#       • A table of tokens + descriptions from FlightPlanDatabase
#     and similarly for each TAF.
#
# Required packages:
#   install.packages(c("shiny", "httr", "jsonlite", "stringr"))
#

library(shiny)
library(httr)
library(jsonlite)
library(stringr)

# ------------------------------------------------------------------------------
# 1) Load the airports CSV at app startup
#    - The CSV has no header. Column 2 = Airport Name, Column 3 = City, Column 6 = ICAO.
#    - We build a named vector: names = "Name (City)", values = ICAO.
# ------------------------------------------------------------------------------
airports_df <- read.csv(
  "airports.csv",
  header = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

# Ensure the file was read correctly:
#   V2 = airport name, V3 = city, V6 = ICAO
# Remove any rows where ICAO is missing or empty
airports_df <- airports_df[airports_df$V6 != "" & !is.na(airports_df$V6), ]

# Build the "label" column: "Name (City)"
airport_labels <- paste0(
  airports_df$V2,
  " (",
  airports_df$V3,
  ")"
)

# Build a named vector: names = label, values = ICAO
# This will be passed to selectizeInput server-side
icao_choices <- setNames(
  airports_df$V6,
  airport_labels
)

# ------------------------------------------------------------------------------
# 2) Helper: Fetch only the raw METAR & raw TAF from AviationWeather.gov,
#         including obsTime (UNIX epoch) for METAR.
# ------------------------------------------------------------------------------
fetch_metar_plus_taf <- function(icao, hours_back = 3, include_taf = FALSE) {
  base_url <- "https://aviationweather.gov/api/data/metar"
  
  query_list <- list(
    ids    = toupper(icao),
    format = "json",
    hours  = hours_back,
    taf    = tolower(as.character(include_taf))
  )
  
  full_url <- modify_url(base_url, query = query_list)
  message(">>> HTTP GET Request: ", full_url)
  
  resp <- GET(full_url)
  status <- status_code(resp)
  message(">>> HTTP Status Code: ", status)
  
  raw_txt <- content(resp, as = "text", encoding = "UTF-8")
  message(">>> Raw JSON Response:\n", raw_txt)
  
  if (http_error(resp)) {
    stop("Error fetching METAR/TAF: HTTP ", status, " returned.")
  }
  
  parsed <- fromJSON(raw_txt, flatten = FALSE)
  if (length(parsed) == 0 || (is.data.frame(parsed) && nrow(parsed) == 0)) {
    return(list(metar_df = NULL, taf_df = NULL))
  }
  
  if (!is.data.frame(parsed)) {
    df_raw <- as.data.frame(parsed, stringsAsFactors = FALSE)
  } else {
    df_raw <- parsed
  }
  
  # Build a data.frame containing RawMETAR = df_raw$rawOb and ObsTime = df_raw$obsTime
  metar_only <- data.frame(
    RawMETAR = df_raw$rawOb,
    ObsTime  = df_raw$obsTime,
    stringsAsFactors = FALSE
  )
  
  # Build a data.frame containing only RawTAF = unique non-empty df_raw$rawTaf
  taf_only <- NULL
  if (include_taf) {
    raw_taf_vals <- unique(na.omit(df_raw$rawTaf))
    raw_taf_vals <- raw_taf_vals[nzchar(raw_taf_vals)]
    if (length(raw_taf_vals) > 0) {
      taf_only <- data.frame(
        RawTAF = raw_taf_vals,
        stringsAsFactors = FALSE
      )
    }
  }
  
  return(list(metar_df = metar_only, taf_df = taf_only))
}

# ------------------------------------------------------------------------------
# 3) Helper: Query FlightPlanDatabase.com for the decoded "parts" JSON
#          Input:  raw_string (e.g. "KLAX 311053Z 23004KT 6SM BR FEW008 …")
#          Output: data.frame with columns "string" and "description",
#                  or NULL on any error.
# ------------------------------------------------------------------------------
decode_via_flightdb <- function(raw_string) {
  # Build the FlightDB decode URL (URLencode with reserved=TRUE → spaces become "+", "/" → "%2F", etc.)
  base_url <- "https://flightplandatabase.com/METAR?s="
  enc_raw  <- URLencode(raw_string, reserved = TRUE)
  full_url <- paste0(base_url, enc_raw)
  
  # Perform GET
  resp <- GET(full_url)
  if (http_error(resp)) {
    message("FlightDB decode error: HTTP ", status_code(resp))
    return(NULL)
  }
  html_txt <- content(resp, as = "text", encoding = "UTF-8")
  
  # Extract the JSON array inside: var parts = JSON.parse('[ ... ]');
  m <- str_match(html_txt, "var parts = JSON.parse\\('(\\[.*?\\])'\\);")
  if (is.na(m[1,2])) {
    message("FlightDB decode: Could not find the 'parts' JSON in HTML.")
    return(NULL)
  }
  parts_json <- m[1,2]
  
  # Remove any stray escaped single quotes (e.g. \' ) inside the JSON string
  parts_json <- gsub("\\\\'", "'", parts_json)
  
  # Parse into a data.frame
  df_parts <- tryCatch(
    fromJSON(parts_json, flatten = TRUE),
    error = function(e) {
      message("JSON parse error: ", e$message)
      return(NULL)
    }
  )
  if (is.null(df_parts) || !all(c("string", "description") %in% colnames(df_parts))) {
    message("Unexpected JSON structure in FlightDB response.")
    return(NULL)
  }
  
  # Return only the two needed columns
  return(df_parts[, c("string", "description"), drop = FALSE])
}

# ------------------------------------------------------------------------------
# Shiny UI
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Select Airport and View METAR/TAF Breakdown"),
  sidebarLayout(
    sidebarPanel(
      # Use selectizeInput with choices = NULL; we'll populate server-side
      selectizeInput(
        inputId  = "icao_code",
        label    = "Select Airport:",
        choices  = NULL,
        multiple = FALSE,
        options  = list(
          placeholder = "Type airport name or city...",
          maxOptions  = 50
        )
      ),
      numericInput(
        inputId = "hours_back",
        label   = "Hours back to fetch:",
        value   = 3,
        min     = 1,
        max     = 24,
        step    = 1
      ),
      checkboxInput(
        inputId = "include_taf",
        label   = "Fetch TAF (raw) as well",
        value   = TRUE
      ),
      actionButton(
        inputId = "btn_fetch",
        label   = "Fetch METAR/TAF"
      ),
      br(),
      helpText(
        HTML(
          paste0(
            "Choose an airport by typing its name or city (e.g. \"Los Angeles\").<br>",
            "Once selected, the ICAO code will be used to fetch raw METAR/TAF from AviationWeather.gov.<br>",
            "Then each raw METAR/TAF is sent to FlightPlanDatabase.com to decode into tokens + descriptions."
          )
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "METAR Results",
          br(),
          uiOutput("metar_breakdown_ui"),
          br(),
          textOutput("metar_message")
        ),
        tabPanel(
          "TAF Results",
          br(),
          uiOutput("taf_breakdown_ui"),
          br(),
          textOutput("taf_message")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Shiny Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  # Populate the selectizeInput server-side for performance, with no default selection
  updateSelectizeInput(
    session,
    "icao_code",
    choices  = icao_choices,
    server   = TRUE,
    selected = character(0)
  )
  
  # Reactive values to store raw METAR/TAF data.frames & decoded breakdowns
  metar_df_r      <- reactiveVal(NULL)      # data.frame with columns RawMETAR, ObsTime
  metar_map_list  <- reactiveVal(list())    # list of data.frames, one per METAR
  taf_df_r        <- reactiveVal(NULL)      # data.frame with column RawTAF
  taf_map_list    <- reactiveVal(list())    # list of data.frames, one per TAF
  
  observeEvent(input$btn_fetch, {
    # The selectizeInput returns the ICAO code directly, since choices = icao_choices
    icao     <- toupper(trimws(input$icao_code %||% ""))
    hrs      <- input$hours_back
    with_taf <- input$include_taf
    
    # If nothing is selected, show a modal and do nothing
    if (icao == "") {
      showModal(modalDialog(
        title = "No Airport Selected",
        "Please select an airport from the dropdown above.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Attempt to fetch from AviationWeather.gov
    tryCatch({
      fetched <- fetch_metar_plus_taf(icao, hrs, with_taf)
      
      # ── METAR Processing ──
      if (is.null(fetched$metar_df) || nrow(fetched$metar_df) == 0) {
        metar_df_r(NULL)
        metar_map_list(list())
        output$metar_message <- renderText({
          paste0("No METAR records found for “", icao, "” in the past ", hrs, " hour(s).")
        })
      } else {
        metar_df_r(fetched$metar_df)
        raw_metars <- fetched$metar_df$RawMETAR
        
        # Decode each raw METAR via FlightPlanDatabase.com
        maps <- lapply(raw_metars, function(x) {
          decode_via_flightdb(x)
        })
        metar_map_list(maps)
        
        output$metar_message <- renderText({
          paste0("Showing raw METAR string(s) and FlightDB breakdown for “", icao, "” (past ", hrs, " hr).")
        })
      }
      
      # ── TAF Processing ──
      if (with_taf) {
        if (is.null(fetched$taf_df) || nrow(fetched$taf_df) == 0) {
          taf_df_r(NULL)
          taf_map_list(list())
          output$taf_message <- renderText({
            paste0("No TAF strings returned for “", icao, "” in the past ", hrs, " hour(s).")
          })
        } else {
          taf_df_r(fetched$taf_df)
          raw_tafs <- fetched$taf_df$RawTAF
          
          # Decode each raw TAF via FlightPlanDatabase.com
          maps_t <- lapply(raw_tafs, function(x) {
            decode_via_flightdb(x)
          })
          taf_map_list(maps_t)
          
          output$taf_message <- renderText({
            paste0("Showing raw TAF string(s) and FlightDB breakdown for “", icao, "” (past ", hrs, " hr).")
          })
        }
      } else {
        taf_df_r(NULL)
        taf_map_list(list())
        output$taf_message <- renderText({
          "TAF not requested."
        })
      }
    },
    error = function(e) {
      # On error, clear everything and show the error message
      metar_df_r(NULL)
      metar_map_list(list())
      taf_df_r(NULL)
      taf_map_list(list())
      output$metar_message <- renderText({
        paste0("Error fetching METAR/TAF: ", e$message)
      })
      output$taf_message <- renderText({ "" })
    })
  })
  
  # ----------------------------------------------------------------------------
  # Render METAR breakdown UI (with human-readable ObsTime)
  # ----------------------------------------------------------------------------
  output$metar_breakdown_ui <- renderUI({
    df_metar <- metar_df_r()
    maps     <- metar_map_list()
    
    if (is.null(df_metar) || length(maps) == 0) {
      return(NULL)
    }
    
    ui_list <- lapply(seq_len(nrow(df_metar)), function(i) {
      raw_str  <- df_metar$RawMETAR[i]
      parts_df <- maps[[i]]
      
      # Convert obsTime (UNIX epoch) to human-readable UTC timestamp
      obs_epoch <- df_metar$ObsTime[i]
      obs_txt   <- as.character(
        as.POSIXct(obs_epoch, origin = "1970-01-01", tz = "UTC")
      )
      
      if (is.null(parts_df) || nrow(parts_df) == 0) {
        return(
          tags$div(
            tags$h4(
              "Raw METAR #", i,
              tags$small(
                style = "color: #555; margin-left: 8px;",
                "(Observed at ", obs_txt, " UTC)"
              )
            ),
            tags$p(code(raw_str)),
            tags$p(em("FlightDB parsing failed or produced no result."))
          )
        )
      }
      
      tags$div(
        tags$h4(
          "Raw METAR #", i,
          tags$small(
            style = "color: #555; margin-left: 8px;",
            "(Observed at ", obs_txt, " UTC)"
          )
        ),
        tags$p(code(raw_str)),
        tags$h5("FlightDB Breakdown:"),
        tags$table(
          border = "1",
          cellpadding = "4",
          tags$thead(
            tags$tr(
              tags$th("Token"),
              tags$th("Description")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(parts_df)), function(j) {
              tags$tr(
                tags$td(code(parts_df$string[j])),
                tags$td(parts_df$description[j])
              )
            })
          )
        ),
        tags$br()
      )
    })
    
    do.call(tagList, ui_list)
  })
  
  # ----------------------------------------------------------------------------
  # Render TAF breakdown UI (unchanged, except for labeling)
  # ----------------------------------------------------------------------------
  output$taf_breakdown_ui <- renderUI({
    df_taf <- taf_df_r()
    maps   <- taf_map_list()
    
    if (is.null(df_taf) || length(maps) == 0) {
      return(NULL)
    }
    
    ui_list <- lapply(seq_len(nrow(df_taf)), function(i) {
      raw_str  <- df_taf$RawTAF[i]
      parts_df <- maps[[i]]
      
      if (is.null(parts_df) || nrow(parts_df) == 0) {
        return(
          tags$div(
            tags$h4(paste0("Raw TAF #", i, ":")),
            tags$p(code(raw_str)),
            tags$p(em("FlightDB parsing failed or produced no result."))
          )
        )
      }
      
      tags$div(
        tags$h4(paste0("Raw TAF #", i, ":")),
        tags$p(code(raw_str)),
        tags$h5("FlightDB Breakdown:"),
        tags$table(
          border = "1",
          cellpadding = "4",
          tags$thead(
            tags$tr(
              tags$th("Token"),
              tags$th("Description")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(parts_df)), function(j) {
              tags$tr(
                tags$td(code(parts_df$string[j])),
                tags$td(parts_df$description[j])
              )
            })
          )
        ),
        tags$br()
      )
    })
    
    do.call(tagList, ui_list)
  })
}

# ------------------------------------------------------------------------------
# Launch the Shiny app
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

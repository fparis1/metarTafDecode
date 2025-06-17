#
# app.R
#
# Shiny app that:
#  1. Reads a CSV of airports.
#  2. Presents a selectizeInput for airport selection.
#  3. Fetches raw METAR & TAF from AviationWeather.gov.
#  4. Decodes METAR/TAF using either a commercial service (Automatic) or
#     a comprehensive internal parser based on merged FAA & International (WMO) standards (Manual).
#  5. Displays the decoded results in a structured format.
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
# ------------------------------------------------------------------------------
airports_df <- read.csv("airports.csv", header=FALSE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
airports_df <- airports_df[airports_df$V6 != "" & !is.na(airports_df$V6), ]
airport_labels <- paste0(airports_df$V2, " (", airports_df$V3, ")")
icao_choices <- setNames(airports_df$V6, airport_labels)

# ------------------------------------------------------------------------------
# METAR/TAF Abbreviation Dictionary (Merged FAA & International/WMO)
# ------------------------------------------------------------------------------
metar_abbreviations <- list(
  "$" = "Maintenance check indicator", "+FC" = "Tornado/Waterspout", "+" = "Heavy intensity",
  "-" = "Light intensity", "A" = "Altimeter", "ACC" = "Altocumulus Castellanus",
  "ACFT" = "Aircraft", "ACSL" = "Altocumulus Standing Lenticular", "AMD" = "Amended Forecast",
  "AO1" = "Automated station without precipitation discriminator", "AO2" = "Automated station with precipitation discriminator",
  "APCH" = "Approach", "APRX" = "Approximately", "AT" = "At (time)", "ATCT" = "Airport Traffic Control Tower",
  "AUTO" = "Fully automated report", "B" = "Began", "BC" = "Patches", "BCFG" = "Fog Patches",
  "BECMG" = "Becoming (gradual change)", "BKN" = "Broken", "BL" = "Blowing",
  "BLDU" = "Blowing Dust", "BLSA" = "Blowing Sand", "BLSN" = "Blowing Snow", "BR" = "Mist",
  "C" = "Center (runway)", "CA" = "Cloud-Air Lightning", "CAVOK" = "Ceiling and Visibility OK",
  "CB" = "Cumulonimbus", "CBMAM" = "Cumulonimbus Mammatus", "CB TCU" = "Cumulonimbus Towering Cumulus",
  "CC" = "Cloud-Cloud Lightning", "CCSL" = "Cirrocumulus Standing Lenticular",
  "CG" = "Cloud-Ground Lightning", "CHI" = "Cloud-Height Indicator", "CHINO" = "Sky Condition at Secondary Location Not Available",
  "CIG" = "Ceiling", "CLR" = "Clear", "CONS" = "Continuous", "COR" = "Correction",
  "DR" = "Low Drifting", "DS" = "Duststorm", "DSNT" = "Distant", "DU" = "Widespread Dust",
  "DZ" = "Drizzle", "E" = "East", "FC" = "Funnel Cloud", "FEW" = "Few", "FG" = "Fog",
  "FM" = "From (rapid change)", "FROPA" = "Frontal Passage", "FT" = "Feet", "FU" = "Smoke",
  "FZ" = "Freezing", "FZDZ" = "Freezing Drizzle", "FZFG" = "Freezing Fog", "FZRA" = "Freezing Rain",
  "G" = "Gust", "GR" = "Hail", "GS" = "Small Hail and/or Snow Pellets", "HLSTO" = "Hailstone",
  "HZ" = "Haze", "IC" = "Ice Crystals", "INC" = "In Cloud", "INTSF" = "Intensifying",
  "ISOL" = "Isolated", "KT" = "Knots", "L" = "Left (runway)", "LTD" = "Limited",
  "LTG" = "Lightning", "M" = "Minus / Less Than", "MAR" = "At sea", "METAR" = "Aviation Routine Weather Report",
  "MI" = "Shallow", "MIFG" = "Shallow Fog", "MOD" = "Moderate", "MOV" = "Moving",
  "N" = "North", "NC" = "No Change", "NE" = "Northeast", "NO" = "Not Available",
  "NOSIG" = "No Significant Change", "NOSPECI" = "No SPECI reports are taken", "NSC" = "No Significant Clouds",
  "NSW" = "No Significant Weather", "NW" = "Northwest", "OCNL" = "Occasional", "OVC" = "Overcast",
  "P" = "Greater than", "PE" = "Ice Pellets", "PL" = "Ice Pellets", "PNO" = "Precipitation Amount Not Available",
  "PO" = "Dust/Sand Whirls", "PR" = "Partial", "PRES" = "Pressure", "PRESFR" = "Pressure Falling Rapidly",
  "PRESRR" = "Pressure Rising Rapidly", "PROB" = "Probability", "PROB30" = "30% Probability",
  "PROB40" = "40% Probability", "PWINO" = "Precipitation Identifier Sensor Not Available",
  "PY" = "Spray", "QNH" = "Altimeter (hPa)", "R" = "Right (runway)", "RA" = "Rain",
  "RE" = "Recent", "RMK" = "Remark", "R/SNOCLO" = "Runway closed due to snow",
  "RVR" = "Runway Visual Range", "RVRNO" = "Runway Visual Range Not Available",
  "S" = "South", "SA" = "Sand", "SCT" = "Scattered", "SE" = "Southeast", "SFC" = "Surface",
  "SG" = "Snow Grains", "SH" = "Showers", "SHRA" = "Rain Showers", "SHSN" = "Snow Showers",
  "SKC" = "Sky Clear", "SLP" = "Sea-Level Pressure", "SLPNO" = "Sea-Level Pressure Not Available",
  "SM" = "Statute Mile", "SN" = "Snow", "SNINCR" = "Snow Increasing Rapidly", "SP" = "Snow Pellets",
  "SPECI" = "Aviation Special Weather Report", "SQ" = "Squalls", "SS" = "Sandstorm",
  "STNR" = "Stationary", "SW" = "Southwest", "TCU" = "Towering Cumulus", "TEMPO" = "Temporary",
  "TL" = "Till (until)", "TS" = "Thunderstorm", "TSNO" = "Thunderstorm Information Not Available",
  "TWR" = "Tower", "UP" = "Unknown Precipitation", "V" = "Variable", "VA" = "Volcanic Ash",
  "VC" = "In the Vicinity", "VCFG" = "Vicinity Fog", "VCSH" = "Vicinity Showers",
  "VIS" = "Visibility", "VRB" = "Variable", "VV" = "Vertical Visibility", "W" = "West",
  "WDSPR" = "Widespread", "WKN" = "Weakening", "WND" = "Wind", "WSHFT" = "Wind Shift",
  "WS" = "Wind Shear", "Z" = "Zulu (UTC)"
)

# ------------------------------------------------------------------------------
# 2) Helper: Fetch raw METAR & TAF from AviationWeather.gov
# ------------------------------------------------------------------------------
fetch_metar_plus_taf <- function(icao, hours_back = 3, include_taf = FALSE) {
  # This function remains unchanged
  base_url <- "https://aviationweather.gov/api/data/metar"
  query_list <- list(ids=toupper(icao), format="json", hours=hours_back, taf=tolower(as.character(include_taf)))
  resp <- GET(modify_url(base_url, query = query_list))
  if (http_error(resp)) stop("AviationWeather.gov API error: HTTP ", status_code(resp))
  parsed <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = FALSE)
  if (length(parsed) == 0 || (is.data.frame(parsed) && nrow(parsed) == 0)) return(list(metar_df = NULL, taf_df = NULL))
  df_raw <- if (!is.data.frame(parsed)) as.data.frame(parsed, stringsAsFactors = FALSE) else parsed
  metar_only <- data.frame(RawMETAR = df_raw$rawOb, ObsTime = df_raw$obsTime, stringsAsFactors = FALSE)
  taf_only <- NULL
  if (include_taf && "rawTaf" %in% colnames(df_raw)) {
    raw_taf_vals <- unique(na.omit(df_raw$rawTaf)); raw_taf_vals <- raw_taf_vals[nzchar(raw_taf_vals)]
    if (length(raw_taf_vals) > 0) taf_only <- data.frame(RawTAF = raw_taf_vals, stringsAsFactors = FALSE)
  }
  return(list(metar_df = metar_only, taf_df = taf_only))
}

# ------------------------------------------------------------------------------
# 3a) Helper: Decode via FlightPlanDatabase.com (Automatic Method)
# ------------------------------------------------------------------------------
decode_via_flightdb <- function(raw_string) {
  # This function remains unchanged
  base_url <- "https://flightplandatabase.com/METAR?s="; enc_raw  <- URLencode(raw_string, reserved = TRUE)
  resp <- GET(paste0(base_url, enc_raw)); if (http_error(resp)) { return(NULL) }
  html_txt <- content(resp, as = "text", encoding = "UTF-8")
  m <- str_match(html_txt, "var parts = JSON.parse\\('(\\[.*?\\])'\\);"); if (is.na(m[1,2])) { return(NULL) }
  parts_json <- gsub("\\\\'", "'", m[1,2]); df_parts <- tryCatch(fromJSON(parts_json, flatten = TRUE), error = function(e) NULL)
  if (is.null(df_parts) || !all(c("string", "description") %in% colnames(df_parts))) return(NULL)
  return(df_parts[, c("string", "description"), drop = FALSE])
}

# ------------------------------------------------------------------------------
# 3b) Helper: Decode Manually using merged FAA/WMO standards
# ------------------------------------------------------------------------------
decode_manually <- function(raw_string) {
  tokens <- unlist(str_split(trimws(raw_string), "\\s+"))
  results <- list(); i <- 1; in_remarks_section <- FALSE
  
  describe_token <- function(token) metar_abbreviations[[token]] %||% "N/A"
  
  while (i <= length(tokens)) {
    token <- tokens[i]; description <- "N/A"
    
    # TAF Change Groups & Probability
    if (token %in% c("FM", "BECMG", "TEMPO", "PROB30", "PROB40")) {
      description <- describe_token(token)
      if (token == "FM" && str_detect(tokens[i+1], "^\\d{6}$")) {
        time_tok <- tokens[i+1]
        description <- sprintf("%s (from day %s at %s:%s UTC)", description, substr(time_tok,1,2), substr(time_tok,3,4), substr(time_tok,5,6))
        token <- paste(token, time_tok); i <- i + 1
      }
    } else if (token == "RMK") { in_remarks_section <- TRUE; description <- "Remarks"
    } else if (in_remarks_section) {
      description <- describe_token(token)
    } else {
      # Main Body Elements
      if (token %in% c("TAF", "METAR", "SPECI", "AUTO", "COR", "AMD", "CAVOK", "NOSIG")) { description <- describe_token(token)
      } else if (str_detect(token, "^[A-Z]{4}$") && i <= 3) { description <- "ICAO Station Identifier"
      } else if (str_detect(token, "^\\d{4}/\\d{4}$")) { # TAF Valid Time (DDHH/DDHH)
        d1<-substr(token,1,2); h1<-substr(token,3,4); d2<-substr(token,6,7); h2<-substr(token,8,9)
        description <- sprintf("Forecast valid from day %s at %s:00 to day %s at %s:00 UTC", d1,h1,d2,h2)
      } else if (str_detect(token, "^\\d{6}Z$")) { # Date/Time
        d<-substr(token,1,2); h<-substr(token,3,4); m<-substr(token,5,6)
        description <- sprintf("Report Time: Day %s at %s:%s UTC", d,h,m)
      } else if (str_detect(token, "KT$")) { # Wind
        dir<-substr(token,1,3); spd<-substr(token,4,5); gust<-if(str_detect(token,"G")) str_match(token, "G(\\d+)KT")[1,2] else NA
        dir_desc <- if(dir=="VRB") "Variable direction" else paste("From",dir,"degrees")
        description <- sprintf("Wind: %s at %s knots", dir_desc, as.numeric(spd))
        if (!is.na(gust)) description <- paste0(description, ", gusting to ", as.numeric(gust), " knots")
      } else if (str_detect(token, "^WS\\d{3}")) { # Wind Shear
        hgt<-as.numeric(substr(token,3,5))*100; dir<-substr(token,7,9); spd<-substr(token,10,12)
        description <- sprintf("Wind Shear at %s ft: from %s° at %s knots", hgt, dir, spd)
      } else if (str_detect(token, "^\\d+V\\d+")) { # Variable Wind
        description <- sprintf("Variable Wind Direction: From %s° to %s°", str_extract(token,"^\\d+"), str_extract(token,"\\d+$"))
      } else if (str_detect(token, "^[0-9/]+SM$")) { description <- sprintf("Visibility: %s statute miles", gsub("SM","",token))
      } else if (str_detect(token, "^\\d{4}$") && i > 3 && !str_detect(token, "/")) { description <- sprintf("Visibility: %s meters", token)
      } else if (str_detect(token, "^R\\d{2}")) { # RVR
        rwy<-str_match(token,"^R(\\d{2}[RLC]?)")[1,2]; vis<-str_match(token,"/(.+)")[1,2]
        description <- sprintf("RVR for Runway %s: %s", rwy, vis)
      } else if (str_detect(token, "^(FEW|SCT|BKN|OVC|VV|SKC|CLR|NSC)")) { # Clouds
        cover<-str_match(token,"^([A-Z]+)")[1,2]; alt<-as.numeric(str_extract(token,"\\d+"))*100; type<-str_extract(token,"(CB|TCU)$")
        desc_cover <- describe_token(cover)
        description <- if(is.na(alt)) desc_cover else sprintf("Sky: %s at %s feet", desc_cover, alt)
        if(!is.na(type)) description <- paste(description, describe_token(type))
      } else if (str_detect(token, "^[+-]?[A-Z]+")) { description <- describe_token(token) # Weather phenomena
      } else if (str_detect(token, "^M?\\d{2}/M?\\d{2}$")) { # Temp/Dew Point
        p<-str_split(token,"/")[[1]]; t<-p[1]; d<-p[2]
        temp_c<-if(str_starts(t,"M")) -as.numeric(substr(t,2,3)) else as.numeric(t)
        dew_c<-if(str_starts(d,"M")) -as.numeric(substr(d,2,3)) else as.numeric(d)
        description <- sprintf("Temperature: %d°C / Dew Point: %d°C", temp_c, dew_c)
      } else if (str_detect(token, "^(A|Q)\\d{4}$")) { # Altimeter
        val <- as.numeric(substr(token,2,5))
        if(str_starts(token,"A")) description <- sprintf("Altimeter: %.2f inHg", val/100) else description <- sprintf("Altimeter: %d hPa", val)
      } else if (str_detect(token, "^5\\d{5}")) { # Turbulence (TAF)
        type<-substr(token,2,2); base<-as.numeric(substr(token,3,3))*1000; depth<-as.numeric(substr(token,5,5))*1000
        description <- sprintf("Turbulence: Type %s, from %s ft to %s ft", type, base, base+depth)
      } else if (str_detect(token, "^6\\d{5}")) { # Icing (TAF)
        type<-substr(token,2,2); base<-as.numeric(substr(token,3,3))*1000; depth<-as.numeric(substr(token,5,5))*1000
        description <- sprintf("Icing: Type %s, from %s ft to %s ft", type, base, base+depth)
      } else { description <- describe_token(token) }
    }
    results[[length(results) + 1]] <- data.frame(string = token, description = description)
    i <- i + 1
  }
  if (length(results) == 0) return(NULL); return(do.call(rbind, results))
}


# ------------------------------------------------------------------------------
# Shiny UI
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("METAR & TAF Decoder"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("icao_code", "1. Select Airport:", choices = NULL, options = list(placeholder = "Type airport name or city...")),
      radioButtons("decode_method", "2. Decoding Method:", choices = c("Automatic (via FlightPlanDatabase)"="auto", "Manual (Global Ruleset)"="manual"), selected = "manual"),
      numericInput("hours_back", "3. Hours Back:", value = 3, min = 1, max = 24, step = 1),
      checkboxInput("include_taf", "4. Fetch TAF?", value = TRUE),
      actionButton("btn_fetch", "Fetch & Decode"), br(), br(),
      helpText(HTML("Data is from AviationWeather.gov.<br><b>Automatic</b> uses FlightPlanDatabase site for message decoding<br><b>Manual</b> uses a comprehensive decoder based on merged FAA & WMO international standards."))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("METAR Results", br(), uiOutput("metar_breakdown_ui"), br(), textOutput("metar_message")),
        tabPanel("TAF Results", br(), uiOutput("taf_breakdown_ui"), br(), textOutput("taf_message"))
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Shiny Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  # This section remains unchanged
  updateSelectizeInput(session, "icao_code", choices = icao_choices, server = TRUE, selected = character(0))
  metar_df_r <- reactiveVal(NULL); metar_map_list <- reactiveVal(list())
  taf_df_r <- reactiveVal(NULL); taf_map_list <- reactiveVal(list())
  
  observeEvent(input$btn_fetch, {
    icao <- toupper(trimws(input$icao_code %||% "")); if (icao == "") {
      showModal(modalDialog(title = "No Airport Selected", "Please select an airport.", easyClose = TRUE)); return()
    }
    decode_function <- if (input$decode_method == "manual") decode_manually else decode_via_flightdb
    tryCatch({
      fetched <- fetch_metar_plus_taf(icao, input$hours_back, input$include_taf)
      # METAR
      if (is.null(fetched$metar_df)||nrow(fetched$metar_df)==0) {
        metar_df_r(NULL); metar_map_list(list()); output$metar_message <- renderText({paste0("No METAR for “",icao,"”.")})
      } else {
        metar_df_r(fetched$metar_df); metar_map_list(lapply(fetched$metar_df$RawMETAR, decode_function))
        output$metar_message <- renderText({paste0("Showing results for “",icao,"”.")})
      }
      # TAF
      if (input$include_taf) {
        if (is.null(fetched$taf_df)||nrow(fetched$taf_df)==0) {
          taf_df_r(NULL); taf_map_list(list()); output$taf_message <- renderText({paste0("No TAF for “",icao,"”.")})
        } else {
          taf_df_r(fetched$taf_df); taf_map_list(lapply(fetched$taf_df$RawTAF, decode_function))
          output$taf_message <- renderText({paste0("Showing results for “",icao,"”.")})
        }
      } else { taf_df_r(NULL); taf_map_list(list()); output$taf_message <- renderText({"TAF not requested."}) }
    }, error = function(e) {
      metar_df_r(NULL); metar_map_list(list()); taf_df_r(NULL); taf_map_list(list())
      output$metar_message <- renderText({paste0("Error: ", e$message)}); output$taf_message <- renderText({""})
    })
  })
  
  # UI Rendering
  output$metar_breakdown_ui <- renderUI({
    df_metar <- metar_df_r(); maps <- metar_map_list(); if (is.null(df_metar)||length(maps)==0) return(NULL)
    lapply(seq_len(nrow(df_metar)), function(i) {
      obs_time <- as.POSIXct(df_metar$ObsTime[i], origin = "1970-01-01", tz = "UTC")
      tags$div(class="report-item",
               tags$h4(sprintf("METAR Report #%d (Observed: %s UTC)", i, format(obs_time, "%Y-%m-%d %H:%M"))),
               tags$p(code(df_metar$RawMETAR[i])),
               if(is.null(maps[[i]])){tags$p(em("Decoding failed."))}else{
                 tags$table(class="table table-bordered table-striped",style="width:auto;",
                            tags$thead(tags$tr(tags$th("Token"),tags$th("Description"))),
                            tags$tbody(lapply(seq_len(nrow(maps[[i]])), function(j){
                              tags$tr(tags$td(code(maps[[i]]$string[j])), tags$td(maps[[i]]$description[j]))
                            }))
                 )
               }, tags$hr()
      )
    })
  })
  
  output$taf_breakdown_ui <- renderUI({
    df_taf <- taf_df_r(); maps <- taf_map_list(); if (is.null(df_taf)||length(maps)==0) return(NULL)
    lapply(seq_len(nrow(df_taf)), function(i) {
      tags$div(class="report-item",
               tags$h4(sprintf("TAF Report #%d", i)),
               tags$p(code(df_taf$RawTAF[i])),
               if(is.null(maps[[i]])){tags$p(em("Decoding failed."))}else{
                 tags$table(class="table table-bordered table-striped",style="width:auto;",
                            tags$thead(tags$tr(tags$th("Token"),tags$th("Description"))),
                            tags$tbody(lapply(seq_len(nrow(maps[[i]])), function(j){
                              tags$tr(tags$td(code(maps[[i]]$string[j])), tags$td(maps[[i]]$description[j]))
                            }))
                 )
               }, tags$hr()
      )
    })
  })
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)
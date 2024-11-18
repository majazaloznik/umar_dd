# app.R
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RPostgres))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(shinyTime))
suppressPackageStartupMessages(library(hms))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(magrittr))
Sys.setenv(TZ = "Europe/Ljubljana")

# Database connection function
get_db_connection <- function() {
        tryCatch({
                conn <- dbConnect(RPostgres::Postgres(),
                                  dbname = "umar_dd",
                                  host = "localhost",
                                  port = 5432,
                                  user = "postgres",
                                  password = Sys.getenv("PG_PG_PSW"))
                return(conn)
        }, error = function(e) {
                message("Povezava na bazo ni uspela: ", e$message)
                return(NULL)
        })
}

safe_db_query <- function(query, params = NULL, fetch = TRUE) {
        conn <- get_db_connection()
        if (is.null(conn)) {
                return(NULL)
        }
        tryCatch({
                if (fetch) {
                        result <- dbGetQuery(conn, query, params)
                } else {
                        result <- dbExecute(conn, query, params)
                }
                return(result)
        }, error = function(e) {
                message("Neuspešna poizvedba: ", e$message)
                return(NULL)
        }, finally = {
                dbDisconnect(conn)
        })
}
# Authentication function
authenticate <- function(user, password) {
        query <- "SELECT id, username, fullname, contract_type, access, arrival_start, arrival_end, departure_start, departure_end, sector 
              FROM employees WHERE username = $1 AND password = $2"
        result <- safe_db_query(query, list(user, password))
        
        if (!is.null(result) && nrow(result) == 1) {
                parse_time_safely <- function(x) {
                        if (is.null(x) || is.na(x) || x == "") return(NULL)
                        tryCatch({
                                hms::as_hms(x)
                        }, error = function(e) {
                                warning(paste("Could not parse time:", x))
                                NULL
                        })
                }
                
                credentials <- list(
                        user_auth = TRUE, 
                        user = result$username, 
                        fullname = as.character(result$fullname),
                        user_id = result$id, 
                        contract_type = result$contract_type,
                        permissions = result$access,
                        arrival_start = parse_time_safely(result$arrival_start),
                        arrival_end = parse_time_safely(result$arrival_end),
                        departure_start = parse_time_safely(result$departure_start),
                        departure_end = parse_time_safely(result$departure_end),
                        sector = result$sector
                )
                
                return(credentials)
        } else {
                return(list(user_auth = FALSE))
        }
}

ui <- fluidPage(
        useShinyjs(),
        tags$head(
                tags$style(HTML("
                        @font-face {
            font-family: 'Aptos';
            src: url('fonts/Aptos.ttf') format('truetype');
            font-weight: normal;
            font-style: normal;
        }
        @font-face {
            font-family: 'Aptos';
            src: url('fonts/Aptos-Bold.ttf') format('truetype');
            font-weight: bold;
            font-style: normal;
        }
        body, input, select, textarea, button {
            font-family: 'Aptos', sans-serif;
        }
            .shiny-input-container {margin-bottom: 10px;}
            #submit, #clear {margin-top: 10px;}
            #calculatedWorkTime {margin-bottom: 20px;}
            #header {
                background-color: #f8f9fa;
                padding: 10px;
                margin-bottom: 15px;
                border-bottom: 1px solid #dee2e6;
            }
            #header-container {
                display: flex;
                align-items: flex-start;
                justify-content: space-between;
                position: relative;
                height: 30px; 
        }
            #logo-name-container {
        display: flex;
        align-items: flex-end;
        height: 100%;
        }
            #app-logo {
            width: 75px;  
            height: auto;
             align-self: flex-start;
        }
            #user-fullname {
            font-weight: bold;
                position: absolute;
        left: 50%;
        bottom: 0;
        transform: translateX(-50%);
        margin: 0;
            }
            #logout {
        align-self: flex-start;
    }
            #startTime, #endTime {
            width: 80%;
            max-width: 150px; 
            border-radius: 5px; 
        }
        ")),
                tags$script("
                    Shiny.addCustomMessageHandler('resetUI', function(message) {
                        window.location.reload();
                    });
                "),
                tags$script(HTML("
    $(document).on('keydown', '#loginForm', function(e) {
      if (e.which == 13) {
        e.preventDefault();
        $('#login').click();
      }
    });
  "))
        ),
        tags$script(HTML("
    Shiny.addCustomMessageHandler('openPDF', function(filename) {
      window.open(filename, '_blank');
    });
  ")),
        tags$script(" 
  $(document).on('keydown', '#date', function(e) {
    e.preventDefault();
    return false;
  });
"), #prevents enterind dates manually
        tags$script("
        Shiny.addCustomMessageHandler('updateInputStyle', function(message) {
            $('#' + message.id).css('background-color', message.style.split(':')[1].trim());
        });
        Shiny.addCustomMessageHandler('triggerWorkTimeCalc', function(message) {
            Shiny.setInputValue('startTime', $('#startTime').val(), {priority: 'event'});
            Shiny.setInputValue('endTime', $('#endTime').val(), {priority: 'event'});
            Shiny.setInputValue('breakStart', $('#breakStart').val(), {priority: 'event'});
            Shiny.setInputValue('breakEnd', $('#breakEnd').val(), {priority: 'event'});
        });
    "),
        
        # Header with login/logout UI
        div(id = "header", 
            uiOutput("loginUI")
        ),
        
        # Main app UI
        uiOutput("mainUI"),
        
        # version number
        tags$footer(
                tags$hr(),
                tags$p(
                        "Špička\U2122 - 2024 - App Version: 1.2.2", 
                        style = "text-align: center; font-size: 0.8em; color: #888;"
                )
        )
)

server <- function(input, output, session) {
        # Reactive value to store user credentials
        credentials <- reactiveVal(NULL)
        entry_update <- reactiveVal(0)
        just_logged_in <- reactiveVal(FALSE)
        
        # Login UI
        output$loginUI <- renderUI({
                if (is.null(credentials())) {
                        tagList(
                                tags$form(
                                        id = "loginForm",
                                        textInput("username", "Uporabniško ime"),
                                        passwordInput("password", "Geslo"),
                                        actionButton("login", "Prijava")
                                ),
                                tags$script("
                                    $('#loginForm').on('submit', function(e) {
                                        e.preventDefault();
                                        $('#login').click();
                                    });
                                ")
                        )
                } else {
                        div(id = "header-container",
                            div(id = "logo-name-container",
                                img(src = "logo.png", id = "app-logo"),
                                p(id = "user-fullname", credentials()$fullname)
                            ),
                            actionButton("logout", "Odjava")
                        )
                }
        })
        
        # Login logic
        
        observeEvent(input$login, {
                req(input$username, input$password)
                user_credentials <- authenticate(input$username, input$password)
                if (user_credentials$user_auth) {
                        credentials(user_credentials)
                        just_logged_in(TRUE) 
                } else {
                        showModal(modalDialog(
                                title = "Napaka pri prijavi",
                                "Napačno uporabniško ime ali geslo. Poskusi še enkrat.",
                                easyClose = TRUE,
                                footer = modalButton("Razumem")
                        ))
                }
        })
        
        observeEvent(input$logout, {
                # Invalidate all reactive values that depend on user session
                credentials(NULL)
                entry_update(0)
                just_logged_in(FALSE)
                
                # Clear all inputs
                lapply(names(input), function(x) {
                        if (x != "logout") {  # Avoid clearing the logout button
                                updateTextInput(session, x, value = "")
                        }
                })
                
                # Reset the calculated work time output
                output$calculatedWorkTime <- renderUI({
                        div(
                                style = "background-color: white; padding: 20px; border-radius: 10px; text-align: center;",
                                h4("Skupna prisotnost:"),
                                h2("-- : --", style = "font-weight: bold;")
                        )
                })
                
                # Show notification
                showNotification("Uspešna odjava.", type = "message", duration = 5)
                
                # Instead of reloading or closing the session, we'll update the UI
                session$sendCustomMessage(type = "resetUI", message = list())
        })
        
        
        # Main app logic
        output$mainUI <- renderUI({
                req(credentials())
                default_range <- get_default_date_range()
                tabsetPanel(
                        tabPanel("Vnos in popravki",
                                 fluidRow(
                                         column(10,
                                                fluidRow(
                                                        column(5,  
                                                               div(style = "margin-top: 20px;",
                                                                   dateInput("date", "Datum", value = NULL, weekstart = 1, format = "dd.mm.yyyy", daysofweekdisabled = c(0, 6), language = "sl"),
                                                                   timeInput("startTime", "Prihod na delo", value = "", seconds = FALSE),
                                                                   timeInput("endTime", "Odhod z dela", value = "", seconds = FALSE),                                                               timeInput("breakStart", "Začetek privatnega izhoda (ne malice)", value = "", seconds = FALSE),
                                                                   timeInput("breakEnd", "Konec privatnega izhoda (ne malice)", value = "", seconds = FALSE),
                                                                   hr(),
                                                                   uiOutput("entryHistory")
                                                               )),
                                                        column(7,  # Second column (wider)
                                                               div(style = "margin-top: 20px;",
                                                                   textAreaInput("tasks", "Poročilo o opravljenem delu", value = "", rows = 10),
                                                                   textAreaInput("notes", "Dodatne opombe za Špico", value = "", rows = 5),
                                                                   div(
                                                                           style = "accent-color: #64af80;",
                                                                           checkboxInput("lunch", "Odmor med delovnim časom ('malica')", value = TRUE)
                                                                   ),
                                                                   numericInput("lunchDuration", "Obseg izrabe odmora", value = NULL, min = 0, max = 99, step = 1, width = "100px")
                                                               ))
                                                )
                                         ),
                                         column(2,  # Third column (narrowest)
                                                div(style = "margin-top: 20px;", 
                                                    uiOutput("calculatedWorkTime")
                                                ),
                                                br(),
                                                br(),
                                                br(),
                                                br(),
                                                br(),
                                                actionButton("submit", "Oddaj/posodobi", width = "100%", 
                                                             style="background-color:  #ced4da"),
                                                br(),
                                                br(),
                                                br(),
                                                br(),
                                                br(),
                                                br(),
                                                actionButton("clear", "Počisti polja", width = "100%"),
                                                br(),
                                                br(),
                                                actionButton("delete", "Izbriši vnos", width = "100%", 
                                                             style="color: #ff7d9d; border-color: #ced4da")
                                         )
                                 )
                        ),
                        tabPanel("Spremeni geslo",
                                 div(style = "margin-top: 20px;",
                                     passwordInput("current_password", "Obstoječe geslo"),
                                     passwordInput("new_password", "Novo geslo"),
                                     passwordInput("confirm_password", "Potrdi novo geslo"),
                                     actionButton("change_password", "Spremeni geslo")
                                 )),
                        tabPanel("Poročila",
                                 h3("Generiraj poročilo"),
                                 dateRangeInput("report_date_range", "Izberi časovno obdobje:",
                                                start = default_range$start, 
                                                end = default_range$end,
                                                min = "2024-09-01",  # Adjust this to your needs
                                                max = Sys.Date(),
                                                format = "dd.mm.yyyy",
                                                language = "sl",
                                                weekstart = 1),
                                 if (credentials()$permissions == "admin") {
                                         actionButton("generate_admin_report", "Generiraj admin poročilo")
                                 } else if (credentials()$permissions == "vodja") {
                                         actionButton("generate_head_report", "Generiraj poročilo vodje")
                                 },
                                 
                                 # Employee report button that's always visible
                                 actionButton("generate_employee_report", "Generiraj svoje poročilo")
                        )
                )
        })
        
        observe({
                req(credentials())
                contract_hours <- credentials()$contract_type
                default_value <- switch(as.character(contract_hours),
                                        "8" = 30,
                                        "6" = 22.5,
                                        "4" = 15,
                                        0)
                updateNumericInput(session, "lunchDuration", value = default_value)
        })
        
        
        observe({
                req(credentials(), input$lunchDuration)
                contract_hours <- credentials()$contract_type
                max_allowed <- switch(as.character(contract_hours),
                                      "8" = 30,
                                      "6" = 22.5,
                                      "4" = 15,
                                      0)
                if (input$lunchDuration > max_allowed) {
                        showNotification("Obseg izrabe odmora presega dovoljeno vrednost. Presežek vnesi kot privatni izhod.", type = "warning")
                }
        })
        
        time_in_range <- reactive({
                req(credentials())
                
                if (is.null(input$date) || is.na(input$date)) {
                        return(list(start = NULL, end = NULL))
                }
                
                extract_time <- function(time_input) {
                        if (inherits(time_input, "POSIXt")) {
                                extracted_time <- hms::as_hms(time_input)
                                return(if (extracted_time == hms::as_hms("00:00:00")) NULL else extracted_time)
                        }
                        return(NULL)
                }
                
                start_time <- extract_time(input$startTime)
                end_time <- extract_time(input$endTime)
                
                # Ensure input$date is a Date object
                date <- if(inherits(input$date, "Date")) input$date else as.Date(input$date)
                friday <- !is.na(date) && wday(date) == 6
                contract_hours <- credentials()$contract_type
                departure_start <- credentials()$departure_start
                departure_end <- credentials()$departure_end
                
                if(friday & contract_hours == 8) {
                        departure_start <- hms::as_hms(as.numeric(departure_start) - 30 * 60)  # Subtract 30 minutes
                        departure_end <- hms::as_hms(as.numeric(departure_end) - 60 * 60)  # Subtract 1 hour
                }
                
                
                start_in_range <- if (is.null(start_time) || start_time == "") NULL else 
                        !is.null(credentials()$arrival_start) &&
                        start_time >= credentials()$arrival_start && 
                        start_time <= credentials()$arrival_end
                
                end_in_range <- if (is.null(end_time) || end_time == "") NULL else 
                        !is.null(departure_start) &&
                        end_time >= departure_start && 
                        end_time <= departure_end
                
                # Trigger color update
                shinyjs::runjs(sprintf("
        var startInput = document.getElementById('startTime');
        var endInput = document.getElementById('endTime');
        if (startInput && endInput) {
            startInput.style.backgroundColor = '%s';
            endInput.style.backgroundColor = '%s';
        }
    ", if (is.null(start_in_range)) "white" else if (start_in_range) "white" else "#ff7d9d",
                                       if (is.null(end_in_range)) "white" else if (end_in_range) "white" else "#ff7d9d"))
                
                list(
                        start = start_in_range,
                        end = end_in_range
                )
        })
        
        update_time_input_style <- function(session, inputId, in_range) {
                color <- if(is.null(in_range)) NULL else if(in_range) "white" else "#ff7d9d"
                shinyjs::runjs(sprintf("
        var input = document.getElementById('%s');
        if (input) {
            input.style.backgroundColor = '%s';
            console.log('Updated %s background to %s');
        } else {
            console.log('Could not find input element %s');
        }
    ", inputId, color, inputId, color, inputId))
        }
        
        observe({
                req(credentials())
                ranges <- time_in_range()
                update_time_input_style(session, "startTime", ranges$start)
                update_time_input_style(session, "endTime", ranges$end)
        })
        
        workTimeCalc <- reactive({
                # First, check if credentials exist
                if (is.null(credentials())) {
                        return(list(time = "-- : --", color = "white"))
                }
                
                if (is.null(credentials()) || is.null(input$date) || is.na(input$date)) {
                        return(list(time = "-- : --", color = "white"))
                }
                
                # Helper function to extract time from POSIXlt or return NULL if empty
                extract_time <- function(time_input) {
                        if (inherits(time_input, "POSIXt")) {
                                return(time_input)
                        }
                        return(NULL)
                }
                
                # Extract times
                start_time <- extract_time(input$startTime)
                end_time <- extract_time(input$endTime)
                break_start <- extract_time(input$breakStart)
                break_end <- extract_time(input$breakEnd)
                
                # If start or end time is not set, return default
                if (is.null(start_time) || is.null(end_time) || start_time >= end_time) {
                        return(list(time = "-- : --", color = "white"))
                }
                
                # Calculate total time
                total_time <- as.numeric(difftime(end_time, start_time, units = "hours"))
                
                # Handle break times if both are set and valid
                if (!is.null(break_start) && !is.null(break_end) && break_start < break_end) {
                        break_time <- as.numeric(difftime(break_end, break_start, units = "hours"))
                        total_time <- total_time - break_time
                }
                
                total_minutes <- round(total_time * 60)
                total_hours <- floor(total_minutes / 60)
                remaining_minutes <- total_minutes %% 60
                
                # Get the user's contract hours
                contract_hours <- credentials()$contract_type
                
                # Determine color based on contract hours
                color <- if (abs(total_time - contract_hours) < 0.01) "#64af80" 
                else if (total_time < contract_hours) "#f5d16c" 
                else "#add8e6"
                
                list(time = sprintf("%02d:%02d", total_hours, remaining_minutes), color = color)
        })
        
        output$calculatedWorkTime <- renderUI({
                result <- workTimeCalc()
                div(
                        style = paste("background-color:", result$color, 
                                      "; padding: 5px;", # Reduced padding
                                      "border-radius: 10px;", # Smaller border radius
                                      "text-align: center;",
                                      "width: 85%;", # Reduced width
                                      "height: 85%;", # Reduced width
                                      "margin: 0 auto;"), # Center the div
                        h4("Skupna prisotnost:"),
                        h2(result$time, style = "font-weight: bold;"),
                        textOutput("contractHours")
                )
        })
        
        output$entryHistory <- renderUI({
                req(credentials())
                if (is.null(input$date) || is.na(input$date)) {
                        return(p("Ni izbranega datuma."))
                }
                
                query <- "SELECT entry_timestamp, is_current 
           FROM time_entries 
           WHERE user_id = $1 AND date = $2 
           ORDER BY entry_timestamp DESC"
                history <- safe_db_query(query, list(credentials()$user_id, input$date))
                
                if (is.null(history)) {
                        return(p("Napaka pri pridobivanju podatkov. Poskusite znova kasneje."))
                }
                
                if (nrow(history) > 0) {
                        tagList(
                                h4("Zgodovina sprememb"),
                                lapply(1:nrow(history), function(i) {
                                        status <- if (history$is_current[i]) "Aktivna" else "Prejšnja"
                                        p(paste(status, "verzija oddana", history$entry_timestamp[i]))
                                })
                        )
                } else {
                        p("Za ta datum še ni vnosa.")
                }
        })
        
        
        
        # Helper function to get the date range for allowed entries, excluding weekends
        get_allowed_date_range <- function() {
                current_date <- Sys.Date()
                current_weekday <- as.integer(format(current_date, "%u"))
                
                # Calculate the start of the current week (Monday)
                start_of_week <- current_date - (current_weekday - 1)
                
                # If it's Monday or Tuesday, include the previous week
                if (current_weekday <= 2) {
                        start_of_previous_week <- start_of_week - 7
                        start_date <- start_of_previous_week
                } else {
                        start_date <- start_of_week 
                }
                
                # Generate a vector of all dates in the range
                all_dates <- seq(start_date, current_date, by = "day")
                
                list(start_date = min(all_dates), end_date = max(all_dates))
        }
        
        format_time_input <- function(time_str) {
                if (is.null(time_str) || nchar(trimws(time_str)) == 0) {
                        return("")
                }
                formatted_time <- sprintf("%04d", as.integer(time_str))
                paste0(substr(formatted_time, 1, 2), ":", substr(formatted_time, 3, 4))
        }
        
        # helper funciton for report date range
        get_default_date_range <- function() {
                today <- Sys.Date()
                current_day <- as.integer(format(today, "%u"))  # 1 = Monday, 7 = Sunday
                
                if (current_day >= 5) {  # Friday, Saturday, or Sunday
                        end_date <- today + (5 - current_day)  # Adjust to the current week's Friday
                        start_date <- end_date - 4  # Monday of the same week
                } else {  # Monday to Thursday
                        end_date <- today - current_day - 2  # Friday of the previous week
                        start_date <- end_date - 4  # Monday of the previous week
                }
                
                list(start = start_date, end = end_date)
        }
        # Clear form when credentials change (i.e., on login)
        observeEvent(credentials(), {
                if (!is.null(credentials())) {
                        clearForm(session)
                }
        })
        
        observeEvent(input$date, {
                req(credentials())
                
                entry <- get_entry_details(input$date)
                if (!is.null(entry)) {
                        # Populate form with existing entry data
                        updateTimeInput(session, "startTime", value = entry$start_time)
                        updateTimeInput(session, "endTime", value = entry$end_time)
                        updateTimeInput(session, "breakStart", value = entry$break_start)
                        updateTimeInput(session, "breakEnd", value = entry$break_end)
                        updateTextAreaInput(session, "tasks", value = entry$tasks)
                        updateTextAreaInput(session, "notes", value = entry$notes)
                        updateCheckboxInput(session, "lunch", value = as.logical(entry$lunch))
                        updateNumericInput(session, "lunchDuration", value = entry$lunch_mins)
                        
                        # Trigger recalculation of work time
                        session$sendCustomMessage(type = 'triggerWorkTimeCalc', message = list())
                } else {
                        # Clear the form for a new entry
                        clearForm(session)
                }
        })
        
        # Submit new entry
        observeEvent(input$submit, {
                req(credentials())
                
                # Validation checks
                if (is.null(input$date) || is.na(input$date)) {
                        showModal(modalDialog(
                                title = "Napaka: Neveljaven datum",
                                "Prosim, izberite veljaven datum.",
                                footer = modalButton("Razumem"),
                                easyClose = TRUE
                        ))
                        return()
                }
                
                start_time <- strftime(input$startTime, "%H:%M:%S")
                end_time <- strftime(input$endTime, "%H:%M:%S")
                
                if (start_time == "00:00:00" || end_time == "00:00:00" || start_time >= end_time) {
                        showModal(modalDialog(
                                title = "Napaka: Neveljaven čas",
                                "Prosim, vnesite veljaven čas začetka in konca. Čas konca mora biti po času začetka.",
                                footer = modalButton("Razumem"),
                                easyClose = TRUE
                        ))
                        return()
                }
                
                break_start <- strftime(input$breakStart, "%H:%M:%S")
                break_end <- strftime(input$breakEnd, "%H:%M:%S")
                
                if ((break_start != "00:00:00" && break_end == "00:00:00") || 
                    (break_start == "00:00:00" && break_end != "00:00:00")) {
                        showModal(modalDialog(
                                title = "Napaka: Neveljaven čas odmora",
                                "Prosim, vnesi oba časa odmora ali pusti oba prazna.",
                                footer = modalButton("Razumem"),
                                easyClose = TRUE
                        ))
                        return()
                }
                
                if ((break_start != "00:00:00" && break_end != "00:00:00") &&
                    (break_start < start_time || break_end > end_time)) {
                        showModal(modalDialog(
                                title = "Napaka: Čas odmora izven delovnega časa",
                                "Čas odmora mora biti znotraj delovnega časa (med časom začetka in konca dela).",
                                footer = modalButton("Razumem"),
                                easyClose = TRUE
                        ))
                        return()
                }
                
                if (break_start != "00:00:00" && break_end != "00:00:00" && break_start >= break_end) {
                        showModal(modalDialog(
                                title = "Napaka: Neveljaven čas odmora",
                                "Čas začetka odmora mora biti pred časom konca odmora.",
                                footer = modalButton("Razumem"),
                                easyClose = TRUE
                        ))
                        return()
                }
                
                # Calculate total time
                total_time <- as.numeric(difftime(as.POSIXct(end_time, format="%H:%M:%S"),
                                                  as.POSIXct(start_time, format="%H:%M:%S"),
                                                  units="hours"))
                
                # Handle break times if both are set and valid
                if (break_start != "00:00:00" && break_end != "00:00:00") {
                        break_time <- as.numeric(difftime(as.POSIXct(break_end, format="%H:%M:%S"),
                                                          as.POSIXct(break_start, format="%H:%M:%S"), 
                                                          units = "hours"))
                        total_time <- total_time - break_time
                }
                
                if (total_time <= 0) {
                        showModal(modalDialog(
                                title = "Napaka: Neveljaven delovni čas",
                                "Skupni delovni čas mora biti večji od nič.",
                                footer = modalButton("Razumem"),
                                easyClose = TRUE
                        ))
                        return()
                }
                
                # Check lunch duration
                contract_hours <- credentials()$contract_type
                max_allowed <- switch(as.character(contract_hours),
                                      "8" = 30,
                                      "6" = 22.5,
                                      "4" = 15,
                                      0)
                if ( total_time < contract_hours/2 & input$lunch) {
                        print("malica")
                        showModal(modalDialog(
                                title = "Nisi upravičen/a do odmora za malico",
                                paste("Delal/a si manj kot polovico ur, zato moraš izbrisati kljukico za odmor med delovnim časom pred oddajo."),
                                footer = modalButton("Razumem"),
                                easyClose = TRUE
                        ))
                        return()
                }
                
                if (input$lunchDuration > max_allowed & input$lunch) {
                        showModal(modalDialog(
                                title = "Opozorilo: Predolg odmor",
                                paste("Obseg izrabe odmora (", input$lunchDuration, " minut) presega dovoljeno vrednost (", max_allowed, " minut).",
                                      "Presežek vnesi kot privatni izhod. Spremeni vrednost odmora pred oddajo."),
                                footer = modalButton("Razumem"),
                                easyClose = TRUE
                        ))
                } else {
                        proceedWithSubmission()
                }
        })
        # Function to proceed with submission
        proceedWithSubmission <- function() {
                check_query <- "SELECT COUNT(*) FROM time_entries WHERE user_id = $1 AND date = $2 AND is_current = TRUE"
                existing_count <- safe_db_query(check_query, list(credentials()$user_id, as.Date(input$date)))[[1]]
                
                if (is.null(existing_count)) {
                        showNotification("Error checking existing entries. Please try again.", type = "error")
                        return()
                }
                
                if (existing_count > 0) {
                        showModal(modalDialog(
                                title = "Posodobitev obstoječega vnosa",
                                "Za ta datum že obstaja vnos. Ali ga res želiš posodobiti?",
                                footer = tagList(
                                        modalButton("Prekliči"),
                                        actionButton("confirmUpdate", "Posodobi")
                                )
                        ))
                } else {
                        insertEntry()
                }
        }
        
        observeEvent(input$clear, {
                clearForm(session)
        })
        
        observeEvent(input$delete, {
                req(credentials(), input$date)
                
                # Check if an entry exists for the selected date
                entry <- get_entry_details(input$date)
                
                if (is.null(entry)) {
                        showNotification("Ni vnosa za izbrani datum.", type = "warning")
                } else {
                        showModal(modalDialog(
                                title = "Potrdi izbris",
                                "Ali si prepričan, da hočeš izbrisati ta vnos?",
                                footer = tagList(
                                        modalButton("Prekliči"),
                                        actionButton("confirmDelete", "Izbriši", class = "btn-danger")
                                )
                        ))
                }
        })
        
        observeEvent(input$confirmDelete, {
                req(credentials(), input$date)
                
                # Update the is_current flag to FALSE
                update_query <- "UPDATE time_entries SET is_current = FALSE 
                   WHERE user_id = $1 AND date = $2 AND is_current = TRUE"
                result <- safe_db_query(update_query, list(credentials()$user_id, as.Date(input$date)), fetch = FALSE)
                
                if (!is.null(result)) {
                        showNotification("Vnos uspešno izbrisan", type = "message")
                        removeModal()
                        clearForm(session)
                        entry_update(entry_update() + 1)  # Trigger update of View/Edit tab
                } else {
                        showNotification("Napaka pri brisanju vnosa. Poskusite znova.", type = "error")
                }
        })
        render_admin_report <- function(start_date, end_date) {
                output_file <- paste0("DD admin - ", format(Sys.time(), "%Y-%m-%d %H-%M"), ".pdf")
                www_dir <- file.path(getwd(), "www")
                if (!dir.exists(www_dir)) dir.create(www_dir)
                output_path <- file.path(www_dir, output_file)
                
                tryCatch({
                        result <- rmarkdown::render(
                                input = "../docs/admin_report.Rmd",
                                output_file = output_path,
                                params = list(
                                        start_date = start_date,
                                        end_date = end_date
                                ),
                                envir = new.env()
                        )
                        print(paste("Rezultat renderiranja:", result))  # Debug print
                        return(output_file)  # Return just the filename
                }, error = function(e) {
                        print(paste("Napaka pri renderiranju poročila:", e$message))
                        return(NULL)
                })
        }
        
        render_head_report <- function(start_date, end_date, sector, user) {
                output_file <- paste0("DD sektorsko - ",user, " ", format(Sys.time(), "%Y-%m-%d %H-%M"), ".pdf")
                www_dir <- file.path(getwd(), "www")
                if (!dir.exists(www_dir)) dir.create(www_dir)
                output_path <- file.path(www_dir, output_file)
                
                tryCatch({
                        result <- rmarkdown::render(
                                input = "../docs/head_report.Rmd",
                                output_file = output_path,
                                params = list(
                                        start_date = start_date,
                                        end_date = end_date,
                                        sector = sector
                                ),
                                envir = new.env()
                        )
                        print(paste("Rezultat renderiranja:", result))  # Debug print
                        return(output_file)  # Return just the filename
                }, error = function(e) {
                        print(paste("Napaka pri renderiranju poročila:", e$message))
                        return(NULL)
                })
        }
        
        
        render_employee_report <- function(start_date, end_date, user_id, username) {
                output_file <- paste0("DD - ", username, " ",format(Sys.time(), "%Y-%m-%d %H-%M"), ".pdf")
                www_dir <- file.path(getwd(), "www")
                if (!dir.exists(www_dir)) dir.create(www_dir)
                output_path <- file.path(www_dir, output_file)
                
                tryCatch({
                        result <- rmarkdown::render(
                                input = "../docs/employee_report.Rmd",
                                output_file = output_path,
                                params = list(
                                        start_date = start_date,
                                        end_date = end_date,
                                        user_id = user_id
                                ),
                                envir = new.env()
                        )
                        print(paste("Rezultat renderiranja:", result))  # Debug print
                        return(output_file)  # Return just the filename
                }, error = function(e) {
                        print(paste("Napaka pri renderiranju poročila:", e$message))
                        return(NULL)
                })
        }
        
        
        
        
        
        # Event handler for generating and downloading admin report
        observeEvent(input$generate_admin_report, {
                req(credentials()$permissions == "admin")
                
                # Show a progress notification
                withProgress(message = 'Generiranje poročila...', value = 0, {
                        
                        # Increment the progress bar
                        incProgress(0.1, detail = "Pripravljanje podatkov")
                        
                        # Get the date range
                        start_date <- input$report_date_range[1]
                        end_date <- input$report_date_range[2]
                        
                        # Increment the progress bar
                        incProgress(0.4, detail = "Renderiranje pdf-ja")
                        
                        # Generate the report
                        report_filename <- render_admin_report(start_date, end_date)
                        
                        # Increment the progress bar
                        incProgress(0.5, detail = "Še zadnje malenkosti")
                        
                        if (!is.null(report_filename)) {
                                # Open the PDF in a new tab
                                session$sendCustomMessage("openPDF", report_filename)
                                showNotification("Poročilo je bilo uspešno generirano.", type = "message")
                        } else {
                                showNotification("Hm, nekje se je zataknilo. Mogoče poskusi še enkrat, sicer pa pokliči Majo..", type = "error")
                        }
                })
        })
        
        # Event handler for generating and downloading admin report
        observeEvent(input$generate_employee_report, {
                req(credentials())
                # Show a progress notification
                withProgress(message = 'Generiranje poročila...', value = 0, {
                        
                        # Increment the progress bar
                        incProgress(0.1, detail = "Pripravljanje podatkov")
                        
                        # Get the date range
                        start_date <- input$report_date_range[1]
                        end_date <- input$report_date_range[2]
                        user_id <- credentials()$user_id
                        username <- credentials()$user
                        
                        # Increment the progress bar
                        incProgress(0.4, detail = "Renderiranje pdf-ja")
                        
                        # Generate the report
                        report_filename <- render_employee_report(start_date, end_date, user_id, username)
                        
                        # Increment the progress bar
                        incProgress(0.5, detail = "Še zadnje malenkosti")
                        
                        if (!is.null(report_filename)) {
                                # Open the PDF in a new tab
                                session$sendCustomMessage("openPDF", report_filename)
                                showNotification("Poročilo je bilo uspešno generirano.", type = "message")
                        } else {
                                showNotification("Hm, nekje se je zataknilo. Mogoče poskusi še enkrat, sicer pa pokliči Majo Z...", type = "error")
                        }
                })
        })
        observeEvent(input$generate_head_report, {
                req(credentials())
                # Show a progress notification
                withProgress(message = 'Generiranje poročila...', value = 0, {
                        
                        # Increment the progress bar
                        incProgress(0.1, detail = "Pripravljanje podatkov")
                        
                        # Get the date range
                        start_date <- input$report_date_range[1]
                        end_date <- input$report_date_range[2]
                        sector <- credentials()$sector
                        user <- credentials()$user
                        # Increment the progress bar
                        incProgress(0.4, detail = "Renderiranje pdf-ja")
                        
                        # Generate the report
                        report_filename <- render_head_report(start_date, end_date, sector, user)
                        
                        # Increment the progress bar
                        incProgress(0.5, detail = "Še zadnje malenkosti")
                        
                        if (!is.null(report_filename)) {
                                # Open the PDF in a new tab
                                session$sendCustomMessage("openPDF", report_filename)
                                showNotification("Poročilo je bilo uspešno generirano.", type = "message")
                        } else {
                                showNotification("Hm, nekje se je zataknilo. Mogoče poskusi še enkrat, sicer pa pokliči Majo Z...", type = "error")
                        }
                })
        })
        
        # Define a function to insert or update an entry
        insertEntry <- function() {
                sanitize_input <- function(text) {
                        if (is.null(text) || is.na(text)) return(text)
                        text %>%
                                stringr::str_replace_all("\\\\", "\\\\textbackslash{}") %>%
                                stringr::str_replace_all("([&%$#_{}~^])", "\\\\\\1") %>%
                                stringr::str_replace_all("~", "\\\\textasciitilde{}") %>%
                                stringr::str_replace_all("\\^", "\\\\textasciicircum{}") %>%
                                stringr::str_replace_all("\n", "\\\\newline ")
                }
                
                start_time <- strftime(input$startTime, "%H:%M:%S")
                end_time <- strftime(input$endTime, "%H:%M:%S")
                
                break_start <- NA
                break_end <- NA
                if (!is.null(input$breakStart) && strftime(input$breakStart, "%H:%M:%S") != "00:00:00" &&
                    !is.null(input$breakEnd) && strftime(input$breakEnd, "%H:%M:%S") != "00:00:00") {
                        break_start <- strftime(input$breakStart, "%H:%M:%S")
                        break_end <- strftime(input$breakEnd, "%H:%M:%S")
                }
                
                # calculate working hours
                total_time <- as.numeric(difftime(as.POSIXct(end_time, format="%H:%M:%S"),
                                                  as.POSIXct(start_time, format="%H:%M:%S"),
                                                  units="hours"))
                
                # Handle break times if both are set and valid
                if (!is.na(break_start) && !is.na(break_end) && break_start < break_end) {
                        break_time <- as.numeric(difftime(as.POSIXct(break_end, format="%H:%M:%S"),
                                                          as.POSIXct(break_start, format="%H:%M:%S"), 
                                                          units = "hours"))
                        total_time <- total_time - break_time
                }
                
                # Format total_time as interval
                calculated_time <- sprintf("%d hours %d minutes",
                                           floor(total_time),
                                           round((total_time - floor(total_time)) * 60))
                
                # Sanitize text inputs
                sanitized_tasks <- sanitize_input(as.character(input$tasks))
                sanitized_notes <- sanitize_input(as.character(input$notes))
                
                # Update existing entries
                update_query <- "UPDATE time_entries SET is_current = FALSE WHERE user_id = $1 AND date = $2 AND is_current = TRUE"
                safe_db_query(update_query, list(credentials()$user_id, as.Date(input$date)), fetch = FALSE)
                
                # Prepare query parameters
                params <- list(
                        credentials()$user_id,
                        as.Date(input$date),
                        start_time,
                        end_time, 
                        break_start,
                        break_end,
                        sanitized_tasks,
                        sanitized_notes,
                        calculated_time,
                        input$lunch,
                        input$lunchDuration 
                )
                
                # Insert new entry
                insert_query <- "INSERT INTO time_entries (user_id, date, start_time, end_time, break_start, break_end, tasks, notes, is_current, entry_timestamp, calculated_time, lunch, lunch_mins) 
                   VALUES ($1, $2, $3, $4, $5, $6, $7, $8, TRUE, CURRENT_TIMESTAMP AT TIME ZONE 'Europe/Ljubljana', $9::interval, $10, $11)"
                
                result <- safe_db_query(insert_query, params, fetch = FALSE)
                
                if (!is.null(result) && result == 1) {
                        showNotification("Vnos uspešno oddan", type = "message")
                        entry_update(entry_update() + 1)  # Trigger update of View/Edit tab
                        
                        # Clear the form
                        clearForm(session)                        
                        # Reset the calculated work time
                        session$sendCustomMessage(type = 'triggerWorkTimeCalc', message = list())
                } else {
                        showModal(modalDialog(
                                title = "Napaka pri zapisu",
                                "Vnos ni uspel. Ali so podatki nepopolni ali pa nepravilni.",
                                easyClose = TRUE
                        ))
                }
        }
        
        # Handle the update confirmation
        observeEvent(input$confirmUpdate, {
                removeModal()
                insertEntry()
        })
        
        # Get dates for which the user has submissions
        get_user_dates <- reactive({
                entry_update()  # This ensures the reactive recalculates when entry_update changes
                req(credentials())
                
                date_range <- get_allowed_date_range()
                
                query <- "SELECT date 
                FROM time_entries 
                WHERE user_id = $1 
                AND is_current = TRUE
                AND date BETWEEN $2 AND $3
                AND EXTRACT(DOW FROM date) BETWEEN 1 AND 5
                ORDER BY date DESC"
                result <- safe_db_query(query, list(credentials()$user_id, date_range$start_date, date_range$end_date))
                
                if (is.null(result)) {
                        return(character(0))
                }
                
                as.character(result$date)
        })
        
        
        # Update date choices when viewing submissions
        observe({
                req(credentials())
                updateSelectInput(session, "selectDate", choices = get_user_dates())
        })
        
        # Update date input on the entry tab
        observe({
                req(credentials())
                date_range <- get_allowed_date_range()
                updateDateInput(session, "date",
                                min = date_range$start_date,
                                max = date_range$end_date)
        })
        
        # Get entry details for a specific date
        get_entry_details <- function(date, update_form = FALSE) {
                req(credentials())
                
                if (is.null(date) || is.na(date)) {
                        if (update_form) {
                                clearForm(session)
                        }
                        return(NULL)
                }
                
                query <- "SELECT * FROM time_entries 
              WHERE user_id = $1 AND date = $2 AND is_current = TRUE"
                result <- safe_db_query(query, list(credentials()$user_id, date))
                
                desanitize_input <- function(text) {
                        if (is.null(text) || is.na(text)) return(text)
                        text %>%
                                stringr::str_replace_all("\\\\textbackslash\\{\\}", "\\") %>%
                                stringr::str_replace_all("\\\\([&%$#_{}~^])", "\\1") %>%
                                stringr::str_replace_all("\\\\textasciitilde\\{\\}", "~") %>%
                                stringr::str_replace_all("\\\\textasciicircum\\{\\}", "^") %>%
                                stringr::str_replace_all("\\\\newline ", "\n")
                }
                
                if (!is.null(result) && nrow(result) > 0) {
                        result$tasks <- desanitize_input(result$tasks)
                        result$notes <- desanitize_input(result$notes)
                        if (update_form) {
                                updateDateInput(session, "date", value = result$date)
                                updateTimeInput(session, "startTime", value = result$start_time)
                                updateTimeInput(session, "endTime", value = result$end_time)
                                updateTimeInput(session, "breakStart", value = result$break_start)
                                updateTimeInput(session, "breakEnd", value = result$break_end)
                                updateTextAreaInput(session, "tasks", value = result$tasks)
                                updateTextAreaInput(session, "notes", value = result$notes)
                                updateCheckboxInput(session, "lunch", value = as.logical(result$lunch)) 
                                updateNumericInput(session, "lunchDuration", value = result$lunch_mins)
                                # Trigger recalculation of work time
                                session$sendCustomMessage(type = 'triggerWorkTimeCalc', message = list())
                        }
                        
                        return(result[1,])
                } else {
                        if (update_form) {
                                clearForm(session)
                        }
                        return(NULL)
                }
        }
        
        # Add a new function to clear the form
        clearForm <- function(session) {
                updateDateInput(session, "date", value = NULL)
                
                updateTextInput(session, "startTime", value = "")
                updateTextInput(session, "endTime", value = "")
                updateTextInput(session, "breakStart", value = "")
                updateTextInput(session, "breakEnd", value = "")
                updateTextAreaInput(session, "tasks", value = "")
                updateTextAreaInput(session, "notes", value = "")
                #if (is.null(input$lunch) ) {
                updateCheckboxInput(session, "lunch", value = TRUE)
                #}
                req(credentials())
                contract_hours <- credentials()$contract_type
                default_value <- switch(as.character(contract_hours),
                                        "8" = 30,
                                        "6" = 22.5,
                                        "4" = 15,
                                        0)
                updateNumericInput(session, "lunchDuration", value = default_value)
                # Reset the background color of time input fields
                shinyjs::runjs("$('#startTime').css('background-color', 'white');")
                shinyjs::runjs("$('#endTime').css('background-color', 'white');")
                
                # Reset the calculated work time
                session$sendCustomMessage(type = 'triggerWorkTimeCalc', message = list())
                
        }
        
        
        observeEvent(input$change_password, {
                req(credentials())
                # Disable the button immediately
                shinyjs::disable("change_password")
                
                # Reset the button after 3 seconds (whether the operation succeeds or fails)
                on.exit({
                        Sys.sleep(1)  # Optional delay before re-enabling
                        shinyjs::enable("change_password")
                })
                if (input$new_password != input$confirm_password) {
                        showModal(modalDialog(
                                title = "Napaka: napačno geslo",
                                "Novi gesli se ne ujemata.",
                                footer = modalButton("Razumem"),
                                easyClose = TRUE
                        ))
                        return()
                }
                
                # First, verify current password
                verify_query <- "SELECT id FROM employees WHERE id = $1 AND password = $2"
                verify_result <- safe_db_query(verify_query, list(credentials()$user_id, input$current_password))
                
                if (is.null(verify_result) || nrow(verify_result) == 0) {
                        showModal(modalDialog(
                                title = "Napaka: napačno geslo",
                                "Obstoječe geslo ni pravilno.",
                                footer = modalButton("Razumem"),
                                easyClose = TRUE
                        ))
                        return()
                }
                
                # If verified, update password
                update_query <- "UPDATE employees SET password = $1 WHERE id = $2"
                result <- safe_db_query(update_query, list(input$new_password, credentials()$user_id), fetch = FALSE)
                
                if (!is.null(result)) {
                        showModal(modalDialog(
                                title = "Sprememba gesla",
                                "Geslo uspešno posodobljeno.",
                                footer = modalButton("Kul"),
                                easyClose = TRUE
                        ))
                        
                        updateTextInput(session, inputId = "current_password", value = "")
                        updateTextInput(session, inputId = "new_password", value = "")
                        updateTextInput(session, inputId = "confirm_password", value = "")
                        
                } else {
                        showNotification("Napaka pri posodabljanju gesla. Prosim poskusi kasneje.", type = "error")
                }
        })
        
        rotate_log <- function(log_file, max_size = 10 * 1024 * 1024, keep = 5) {
                if (file.exists(log_file) && file.size(log_file) > max_size) {
                        # Rotate existing log files
                        for (i in keep:1) {
                                old <- paste0(log_file, ".", i)
                                new <- paste0(log_file, ".", i + 1)
                                if (file.exists(old)) file.rename(old, new)
                        }
                        # Rename current log file
                        file.rename(log_file, paste0(log_file, ".1"))
                        
                        # Reopen the log connection
                        sink(NULL, type = "output")
                        sink(NULL, type = "message")
                        close(log_con)
                        log_con <<- file(log_file, open = "a")
                        sink(log_con, type = "output")
                        sink(log_con, type = "message")
                        
                        cat(paste("Log rotated at", Sys.time(), "\n"))
                }
        }
        # observe({
        #         invalidateLater(36000000)  # 5 minutes in milliseconds
        #         rotate_log(log_file)
        # })
}

shinyApp(ui, server)
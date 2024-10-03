# app.R
library(shiny)
library(DBI)
library(RPostgres)
library(lubridate)
library(shinyTime)
library(hms)
library(shinyjs)

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
                message("Failed to connect to database: ", e$message)
                return(NULL)
        })
}

# Authentication function
authenticate <- function(user, password) {
        conn <- get_db_connection()
        on.exit(dbDisconnect(conn))
        
        query <- "SELECT id, username, fullname, contract_type, access, arrival_start, arrival_end, departure_start, departure_end 
              FROM employees WHERE username = $1 AND password = $2"
        result <- dbGetQuery(conn, query, list(user, password))
        
        if (nrow(result) == 1) {
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
                        fullname = as.character(result$fullname),  # Explicitly convert to character
                        user_id = result$id, 
                        contract_type = result$contract_type,
                        permissions = result$access,
                        arrival_start = parse_time_safely(result$arrival_start),
                        arrival_end = parse_time_safely(result$arrival_end),
                        departure_start = parse_time_safely(result$departure_start),
                        departure_end = parse_time_safely(result$departure_end)
                )
                
                return(credentials)
        } else {
                list(user_auth = FALSE)
        }
}

ui <- fluidPage(
        useShinyjs(),
        tags$head(
                tags$style(HTML("
            .shiny-input-container {margin-bottom: 10px;}
            #submit, #clear {margin-top: 10px;}
            #calculatedWorkTime {margin-bottom: 20px;}
            #header {
                background-color: #f8f9fa;
                padding: 10px;
                margin-bottom: 15px;
                border-bottom: 1px solid #dee2e6;
            }
        "))
        ),
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
                        "Špička\U2122 - 2024 - App Version: 0.3.0", 
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
                        wellPanel(
                                textInput("username", "Uporabniško ime"),
                                passwordInput("password", "Geslo"),
                                actionButton("login", "Prijava")
                        )
                } else {
                        div(style = "display: flex; justify-content: space-between; align-items: center;",
                            div(
                                    p(credentials()$fullname),
                            ),
                            actionButton("logout", "Odjava")
                        )
                }
        })
        
        # Login logic
        observeEvent(input$login, {
                user_credentials <- authenticate(input$username, input$password)
                if (user_credentials$user_auth) {
                        credentials(user_credentials)
                        just_logged_in(TRUE)  
                } else {
                        showNotification("Invalid username or password", type = "error")
                }
        })
        
        observeEvent(input$logout, {
                # Invalidate all reactive values that depend on user session
                credentials(NULL)
                entry_update(0)
                just_logged_in(FALSE)
                
                # Clear all inputs
                lapply(names(input), function(x) updateTextInput(session, x, value = ""))
                
                # Clear any ongoing database connections
                if (exists("conn") && inherits(conn, "DBIConnection")) {
                        dbDisconnect(conn)
                }
                
                # Reset the calculated work time output
                output$calculatedWorkTime <- renderUI({
                        div(
                                style = "background-color: white; padding: 20px; border-radius: 10px; text-align: center;",
                                h4("Skupna prisotnost:"),
                                h2("-- : --", style = "font-weight: bold;")
                        )
                })
                
                # Reload the session
                session$reload()
                
                showNotification("Uspešna odjava.", type = "message")
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
                                                        column(5,  # First column (narrower)
                                                               # shinyWidgets::airDatepickerInput("date", "Datum", value = NULL, firstDay = 1, 
                                                               #                                  dateFormat = "dd.MM.yyyy", disabledDaysOfWeek = c(0, 6), 
                                                               #                                  language = "en", readonly = TRUE),
                                                               dateInput("date", "Datum", value = NULL, weekstart = 1, format = "dd.mm.yyyy", daysofweekdisabled = c(0, 6), language = "sl"),
                                                               timeInput("startTime", "Prihod na delo", value = "", seconds = FALSE),
                                                               timeInput("endTime", "Odhod iz dela", value = "", seconds = FALSE),                                                               timeInput("breakStart", "Začetek privatnega izhoda (ne malice)", value = "", seconds = FALSE, minute.steps = 5),
                                                               timeInput("breakEnd", "Konec privatnega izhoda (ne malice)", value = "", seconds = FALSE, minute.steps = 5),
                                                               hr(),
                                                               uiOutput("entryHistory")
                                                        ),
                                                        column(7,  # Second column (wider)
                                                               textAreaInput("tasks", "Poročilo o opravljenem delu", value = "", rows = 10),
                                                               textAreaInput("notes", "Dodatne opombe za špico", value = "", rows = 5),
                                                               checkboxInput("lunch", "Odmor med delovnim časom (malica)", value = TRUE)
                                                        )
                                                )
                                         ),
                                         column(2,  # Third column (narrowest)
                                                uiOutput("calculatedWorkTime"),
                                                br(),
                                                actionButton("submit", "Oddaj/posodobi", width = "100%"),
                                                br(),
                                                br(),
                                                actionButton("clear", "Počisti", width = "100%")
                                         )
                                 )
                        ),
                        tabPanel("Spremeni geslo",
                                 passwordInput("current_password", "Obstoječe geslo"),
                                 passwordInput("new_password", "Novo geslo"),
                                 passwordInput("confirm_password", "Potrdi novo geslo"),
                                 actionButton("change_password", "Spremeni geslo")
                        ),
                        if (credentials()$permissions %in% c("admin", "head")) {
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
                                         } else {
                                                 actionButton("generate_head_report", "Generiraj poročilo vodje")
                                         }
                                )
                        }
                )
        })
        
        time_in_range <- reactive({
                req(credentials(), input$date)  # Ensure both credentials and date are available
                
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
                
                start_in_range <- if (is.null(start_time)) NULL else 
                        !is.null(credentials()$arrival_start) &&
                        start_time >= credentials()$arrival_start && 
                        start_time <= credentials()$arrival_end
                
                end_in_range <- if (is.null(end_time)) NULL else 
                        !is.null(departure_start) &&
                        end_time >= departure_start && 
                        end_time <= departure_end
                
                list(
                        start = start_in_range,
                        end = end_in_range
                )
        })
        
        update_time_input_style <- function(session, inputId, in_range) {
                color <- if(is.null(in_range)) NULL else if(in_range) "white" else "orange"
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
                
                total_hours <- floor(total_time)
                total_minutes <- round((total_time - total_hours) * 60)
                
                # Get the user's contract hours
                contract_hours <- credentials()$contract_type
                
                # Determine color based on contract hours
                color <- if (abs(total_time - contract_hours) < 0.01) "green" 
                else if (total_time < contract_hours) "yellow" 
                else "lightblue"
                
                list(time = sprintf("%02d:%02d", total_hours, total_minutes), color = color)
        })
        
        output$calculatedWorkTime <- renderUI({
                result <- workTimeCalc()
                div(
                        style = paste("background-color:", result$color, "; padding: 20px; border-radius: 10px; text-align: center;"),
                        h4("Skupna prisotnost:"),
                        h2(result$time, style = "font-weight: bold;"),
                        textOutput("contractHours")
                )
        })
        
        output$entryHistory <- renderUI({
                req(credentials())
                req(input$date)
                
                conn <- get_db_connection()
                if (is.null(conn)) {
                        return(p("Napaka pri povezavi z bazo podatkov. Poskusite znova kasneje."))
                }
                on.exit(dbDisconnect(conn))
                
                tryCatch({
                        query <- "SELECT entry_timestamp, is_current 
           FROM time_entries 
           WHERE user_id = $1 AND date = $2 
           ORDER BY entry_timestamp DESC"
                        history <- dbGetQuery(conn, query, list(credentials()$user_id, input$date))
                        
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
                }, error = function(e) {
                        p(paste("Napaka pri pridobivanju podatkov:", e$message))
                })
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
        
        
        # Helper function to validate and format time input
        validate_time <- function(time_str, field_name) {
                if (is.null(time_str) || nchar(trimws(time_str)) == 0) {
                        if (field_name %in% c("Start Time", "End Time")) {
                                return(list(valid = FALSE, message = paste(field_name, "is required."), value = NULL))
                        }
                        return(list(valid = TRUE, message = NULL, value = NULL))
                }
                tryCatch({
                        if (grepl(":", time_str)) {
                                formatted_time <- format(as.POSIXct(time_str, format = "%H:%M"), "%H:%M:%S")
                        } else {
                                formatted_time <- format_time_input(time_str)
                                formatted_time <- paste0(formatted_time, ":00")
                        }
                        return(list(valid = TRUE, message = NULL, value = formatted_time))
                }, error = function(e) {
                        return(list(valid = FALSE, message = paste(field_name, "must be in HHMM or HH:MM format."), value = NULL))
                })
        }
        
        # Helper function to parse time strings
        parse_time <- function(time_str) {
                if (is.null(time_str) || time_str == "") return(NA)
                tryCatch(
                        as.POSIXct(time_str, format = "%H:%M"),
                        error = function(e) NA
                )
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
                        clearForm(session, setDefaultDate = TRUE)
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
                
                # Check for existing entry
                conn <- get_db_connection()
                on.exit(dbDisconnect(conn))
                
                check_query <- "SELECT COUNT(*) FROM time_entries WHERE user_id = $1 AND date = $2 AND is_current = TRUE"
                existing_count <- dbGetQuery(conn, check_query, list(credentials()$user_id, as.Date(input$date)))[[1]]
                
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
        })
        
        observeEvent(input$clear, {
                clearForm(session, setDefaultDate = TRUE)
        })
        
        observeEvent(input$generate_admin_report, {
                req(credentials()$permissions == "admin")
                # Placeholder for admin report generation
                showNotification("Generiranje admin poročila...", type = "message")
                # Here you would add the logic to generate the admin report
                # For now, we'll just show a message
                Sys.sleep(2)  # Simulate report generation time
                showNotification("Admin poročilo generirano!", type = "message")
        })
        
        observeEvent(input$generate_head_report, {
                req(credentials()$permissions == "head")
                # Placeholder for head report generation
                showNotification("Generiranje poročila vodje...", type = "message")
                # Here you would add the logic to generate the head's report
                # For now, we'll just show a message
                Sys.sleep(2)  # Simulate report generation time
                showNotification("Poročilo vodje generirano!", type = "message")
        })
        
        # Define a function to insert or update an entry
        insertEntry <- function() {
                conn <- get_db_connection()
                on.exit(dbDisconnect(conn))
                
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
                                                          as.POSIXct( break_start, format="%H:%M:%S"), 
                                                          units = "hours"))
                        total_time <- total_time - break_time
                }
                
                # Format total_time as interval
                calculated_time <- sprintf("%d hours %d minutes",
                                           floor(total_time),
                                           round((total_time - floor(total_time)) * 60))
                
                # Start transaction
                dbExecute(conn, "BEGIN")
                
                # Check if an entry already exists for this date
                check_query <- "SELECT COUNT(*) FROM time_entries WHERE user_id = $1 AND date = $2 AND is_current = TRUE"
                existing_count <- dbGetQuery(conn, check_query, list(credentials()$user_id, as.Date(input$date)))[[1]]
                
                if (existing_count > 0) {
                        # Update: Set the current entry to not current
                        update_query <- "UPDATE time_entries SET is_current = FALSE WHERE user_id = $1 AND date = $2 AND is_current = TRUE"
                        dbExecute(conn, update_query, list(credentials()$user_id, as.Date(input$date)))
                }
                
                # Prepare query parameters
                params <- list(
                        credentials()$user_id,
                        as.Date(input$date),
                        start_time,
                        end_time, 
                        break_start,
                        break_end,
                        as.character(input$tasks),
                        as.character(input$notes),
                        calculated_time,
                        input$lunch  # Add this line
                )
                
                # Insert new entry
                insert_query <- "INSERT INTO time_entries (user_id, date, start_time, end_time, break_start, break_end, tasks, notes, is_current, entry_timestamp, calculated_time, lunch) 
                     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, TRUE, CURRENT_TIMESTAMP AT TIME ZONE 'Europe/Ljubljana', $9::interval, $10)"
                
                result <- tryCatch({
                        dbExecute(conn, insert_query, params)
                }, error = function(e) {
                        print(paste("Error in submit query:", e$message))
                        dbExecute(conn, "ROLLBACK")
                        return(NULL)
                })
                
                if (!is.null(result) && result == 1) {
                        dbExecute(conn, "COMMIT")
                        showNotification(ifelse(existing_count > 0, "Vnos uspešno posodobljen", "Vnos uspešno oddan"), type = "message")
                        entry_update(entry_update() + 1)  # Trigger update of View/Edit tab
                        
                        # Clear the form
                        clearForm(session, setDefaultDate = TRUE)
                        
                        # Reset the calculated work time
                        session$sendCustomMessage(type = 'triggerWorkTimeCalc', message = list())
                } else {
                        dbExecute(conn, "ROLLBACK")
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
                conn <- get_db_connection()
                on.exit(dbDisconnect(conn))
                
                date_range <- get_allowed_date_range()
                
                query <- "SELECT date 
                FROM time_entries 
                WHERE user_id = $1 
                AND is_current = TRUE
                AND date BETWEEN $2 AND $3
                AND EXTRACT(DOW FROM date) BETWEEN 1 AND 5
                ORDER BY date DESC"
                result <- dbGetQuery(conn, query, list(credentials()$user_id, date_range$start_date, date_range$end_date))
                
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
                conn <- get_db_connection()
                on.exit(dbDisconnect(conn))
                
                query <- "SELECT * FROM time_entries 
              WHERE user_id = $1 AND date = $2 AND is_current = TRUE"
                result <- dbGetQuery(conn, query, list(credentials()$user_id, date))
                
                if (nrow(result) > 0) {
                        if (update_form) {
                                updateDateInput(session, "date", value = result$date)
                                updateTimeInput(session, "startTime", value = result$start_time)
                                updateTimeInput(session, "endTime", value = result$end_time)
                                updateTimeInput(session, "breakStart", value = result$break_start)
                                updateTimeInput(session, "breakEnd", value = result$break_end)
                                updateTextAreaInput(session, "tasks", value = result$tasks)
                                updateTextAreaInput(session, "notes", value = result$notes)
                                updateCheckboxInput(session, "lunch", value = as.logical(result$lunch)) 
                                
                                # Trigger recalculation of work time
                                session$sendCustomMessage(type = 'triggerWorkTimeCalc', message = list())
                        }
                        
                        result[1,]
                } else {
                        if (update_form) {
                                # Clear the form if no entry is found
                                clearForm(session, setDefaultDate = TRUE)
                        }
                        
                        NULL
                }
        }
        
        # Add a new function to clear the form
        clearForm <- function(session, setDefaultDate = FALSE) {
                if (setDefaultDate) {
                        updateDateInput(session, "date", value = Sys.Date())
                } else {
                        updateDateInput(session, "date", value = NULL)
                }
                updateTextInput(session, "startTime", value = "")
                updateTextInput(session, "endTime", value = "")
                updateTextInput(session, "breakStart", value = "")
                updateTextInput(session, "breakEnd", value = "")
                updateTextAreaInput(session, "tasks", value = "")
                updateTextAreaInput(session, "notes", value = "")
                if (is.null(input$lunch)) {
                        updateCheckboxInput(session, "lunch", value = TRUE)
                }
                
                
                # Reset the calculated work time
                session$sendCustomMessage(type = 'triggerWorkTimeCalc', message = list())
        }
        
        observeEvent(input$selectDate, {
                req(input$selectDate != "", input$tabs == "View Submissions")
                get_entry_details(input$selectDate, update_form = TRUE)
        })
        
        observeEvent(input$change_password, {
                req(credentials())
                
                if (input$new_password != input$confirm_password) {
                        showNotification("New passwords do not match", type = "error")
                        return()
                }
                
                conn <- get_db_connection()
                on.exit(dbDisconnect(conn))
                
                # First, verify current password
                verify_query <- "SELECT id FROM employees WHERE id = $1 AND password = $2"
                verify_result <- dbGetQuery(conn, verify_query, list(credentials()$user_id, input$current_password))
                
                if (nrow(verify_result) == 0) {
                        showNotification("Current password is incorrect", type = "error")
                        return()
                }
                
                # If verified, update password
                update_query <- "UPDATE employees SET password = $1 WHERE id = $2"
                dbExecute(conn, update_query, list(input$new_password, credentials()$user_id))
                
                showNotification("Password updated successfully", type = "message")
        })
}

shinyApp(ui, server)
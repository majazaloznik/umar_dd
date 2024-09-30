# app.R

library(shiny)
library(DBI)
library(RPostgres)
library(lubridate)
library(shinyTime)
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
        
        query <- "SELECT id, username, contract_type, access FROM employees WHERE username = $1 AND password = $2"
        result <- dbGetQuery(conn, query, list(user, password))
        
        if (nrow(result) == 1) {
                list(user_auth = TRUE, 
                     user = result$username, 
                     user_id = result$id, 
                     contract_type = result$contract_type,
                     permissions = result$access)
        } else {
                list(user_auth = FALSE)
        }
}

ui <- fluidPage(
        tags$head(
                tags$style(HTML("
            .shiny-input-container {margin-bottom: 10px;}
            #submit, #clear {margin-top: 10px;}
            #calculatedWorkTime {margin-bottom: 20px;}
        "))
        ),
        tags$script("
        Shiny.addCustomMessageHandler('triggerWorkTimeCalc', function(message) {
            Shiny.setInputValue('startTime', $('#startTime').val(), {priority: 'event'});
            Shiny.setInputValue('endTime', $('#endTime').val(), {priority: 'event'});
            Shiny.setInputValue('breakStart', $('#breakStart').val(), {priority: 'event'});
            Shiny.setInputValue('breakEnd', $('#breakEnd').val(), {priority: 'event'});
        });
    "),
        # Login UI
        uiOutput("loginUI"),
        
        # Main app UI
        uiOutput("mainUI"),
        
        # version number
        tags$footer(
                tags$hr(), # Optional: adds a horizontal line above the version text
                tags$p(
                        "App Version: 0.3.0", 
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
                                textInput("username", "Username"),
                                passwordInput("password", "Password"),
                                actionButton("login", "Log In")
                        )
                } else {
                        actionButton("logout", "Odjava")
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
                
                # Reload the session
                session$reload()
                
                showNotification("Uspešna odjava.", type = "message")
        })
        
        # Main app logic
        output$mainUI <- renderUI({
                req(credentials())
                
                tabsetPanel(
                        tabPanel("Vnos in popravki",
                                 fluidRow(
                                         column(10,
                                                fluidRow(
                                                        column(5,  # First column (narrower)
                                                               dateInput("date", "Datum", value = Sys.Date(), weekstart = 1, format = "dd-mm-yyyy", daysofweekdisabled = c(0, 6)),
                                                               timeInput("startTime", "Prihod na delo", value = "", seconds = FALSE),
                                                               timeInput("endTime", "Odhod iz dela", value = "", seconds = FALSE),
                                                               timeInput("breakStart", "Začetek privatnega izhoda (ne malice)", value = "", seconds = FALSE, minute.steps = 5),
                                                               timeInput("breakEnd", "Konec privatnega izhoda (ne malice)", value = "", seconds = FALSE, minute.steps = 5),
                                                               hr(),
                                                               uiOutput("entryHistory")
                                                        ),
                                                        column(7,  # Second column (wider)
                                                               textAreaInput("tasks", "Poročilo o opravljenem delu", value = "", rows = 10),
                                                               textAreaInput("notes", "Dodatne opombe za špico", value = "", rows = 5),
                                                               checkboxInput("lunch", "Malica", value = TRUE)
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
                        )
                )
        })
        
        workTimeCalc <- reactive({
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
                                        modalButton("Cancel"),
                                        actionButton("confirmUpdate", "Update")
                                )
                        ))
                } else {
                        insertEntry()
                }
        })
        
        observeEvent(input$clear, {
                clearForm(session, setDefaultDate = TRUE)
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
                                title = "Submission Error",
                                "Failed to submit time entry. Please try again or contact support if the problem persists.",
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
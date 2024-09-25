# app.R

library(shiny)
library(DBI)
library(RPostgres)
library(lubridate)
library(shinyTime)

# Database connection function
get_db_connection <- function() {
        dbConnect(RPostgres::Postgres(),
                  dbname = "umar_dd",
                  host = "localhost",
                  port = 5432,
                  user = "postgres",
                  password = Sys.getenv("PG_PG_PSW"))
}

# User authentication (simplified for example)
user_base <- data.frame(
        user_id = c(1, 2, 3, 4, 5, 6),
        user = c("user1", "user2", "user4", "hr1", "user5",  "user6"),
        password = c("pass1", "pass2", "pass4", "hrpass", "pass5", "pass6"),
        permissions = c("employee", "employee", "hr", "employee","employee", "employee"),
        stringsAsFactors = FALSE
)

# Authentication function
authenticate <- function(user, password) {
        user_row <- user_base[user_base$user == user & user_base$password == password, ]
        if (nrow(user_row) == 1) {
                list(user_auth = TRUE, user = user, user_id = user_row$user_id, permissions = user_row$permissions)
        } else {
                list(user_auth = FALSE)
        }
}

ui <- fluidPage(
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
        uiOutput("mainUI")
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
                        actionButton("logout", "Log Out")
                }
        })
        
        # Login logic
        observeEvent(input$login, {
                user_credentials <- authenticate(input$username, input$password)
                if (user_credentials$user_auth) {
                        credentials(user_credentials)
                        just_logged_in(TRUE)  # Set this to TRUE when user logs in
                } else {
                        showNotification("Invalid username or password", type = "error")
                }
        })
        
        # Logout logic
        observeEvent(input$logout, {
                credentials(NULL)
        })
        
        # Main app logic
        output$mainUI <- renderUI({
                req(credentials())
                
                tabsetPanel(id = "tabs",
                            tabPanel("Submit/Edit Entry",
                                     dateInput("date", "Datum", value = NULL),
                                     timeInput("startTime", "Prihod na delo", value = "", seconds = FALSE, minute.steps = 5),
                                     timeInput("endTime", "Odhod iz dela", value = "", seconds = FALSE, minute.steps = 5),
                                     timeInput("breakStart", "Začetek privatnega izhoda (ne malice)", value = "", seconds = FALSE, minute.steps = 5),
                                     timeInput("breakEnd", "Konec privatnega izhoda (ne malice)", value = "", seconds = FALSE, minute.steps = 5),
                                     uiOutput("calculatedWorkTime"),
                                     textAreaInput("tasks", "Poročilo o opravljenem delu", value = "", rows = 5),
                                     textAreaInput("notes", "Dodatne opombe za špico", value = "", rows = 3),
                                     actionButton("submit", "Oddaj/posodobi"),
                                     actionButton("clear", "Počisti") 
                            ),
                            tabPanel("View Submissions",
                                     selectInput("selectDate", "Select Date", choices = NULL),
                                     uiOutput("submissionDetails"),
                                     uiOutput("entryHistory")
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
                
                color <- if (abs(total_time - 8) < 0.01) "green" else if (total_time < 8) "yellow" else "lightblue"
                
                list(time = sprintf("%02d:%02d", total_hours, total_minutes), color = color)
        })
        
        output$calculatedWorkTime <- renderUI({
                result <- workTimeCalc()
                div(
                        style = paste("background-color:", result$color, "; padding: 10px; border-radius: 5px;"),
                        h4("Calculated Work Time:"),
                        h3(paste(result$time, "hours"))
                )
        })
        
        output$entryHistory <- renderUI({
                req(input$selectDate)
                conn <- get_db_connection()
                on.exit(dbDisconnect(conn))
                
                query <- "SELECT entry_timestamp, is_current 
            FROM time_entries 
            WHERE user_id = $1 AND date = $2 
            ORDER BY entry_timestamp DESC"
                history <- dbGetQuery(conn, query, list(credentials()$user_id, input$selectDate))
                
                if (nrow(history) > 0) {
                        tagList(
                                h4("Entry History"),
                                lapply(1:nrow(history), function(i) {
                                        status <- if (history$is_current[i]) "Current" else "Previous"
                                        p(paste(status, "version submitted at", history$entry_timestamp[i]))
                                })
                        )
                }
        })
        
        # Helper function to convert empty strings to NULL
        convert_empty_to_null <- function(x) {
                if (is.character(x) && nchar(trimws(x)) == 0) NULL else x
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
                                title = "Update Existing Entry",
                                "An entry for this date already exists. Do you want to update it?",
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
                        as.character(input$notes)
                )
                
                # Insert new entry
                insert_query <- "INSERT INTO time_entries (user_id, date, start_time, end_time, break_start, break_end, tasks, notes, is_current, entry_timestamp) 
                     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, TRUE, CURRENT_TIMESTAMP)"
                
                result <- tryCatch({
                        dbExecute(conn, insert_query, params)
                }, error = function(e) {
                        print(paste("Error in submit query:", e$message))
                        dbExecute(conn, "ROLLBACK")
                        return(NULL)
                })
                
                if (!is.null(result) && result == 1) {
                        dbExecute(conn, "COMMIT")
                        showNotification(ifelse(existing_count > 0, "Entry updated successfully", "Entry submitted successfully"), type = "message")
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
                
                query <- "SELECT date 
            FROM time_entries 
            WHERE user_id = $1 AND is_current = TRUE
            ORDER BY date DESC"
                result <- dbGetQuery(conn, query, list(credentials()$user_id))
                
                as.character(result$date)
        })
        
        # Update date choices when viewing submissions
        observe({
                req(credentials())
                updateSelectInput(session, "selectDate", choices = get_user_dates())
        })
        
        # Display selected submission details
        output$submissionDetails <- renderUI({
                req(credentials(), input$selectDate)
                entry <- get_entry_details(input$selectDate)
                
                if (!is.null(entry)) {
                        tagList(
                                p(strong("Date:"), entry$date),
                                p(strong("Start Time:"), entry$start_time),
                                p(strong("End Time:"), entry$end_time),
                                p(strong("Break Start:"), entry$break_start),
                                p(strong("Break End:"), entry$break_end),
                                p(strong("Tasks:"), entry$tasks),
                                p(strong("Notes:"), entry$notes),
                                p(strong("Last Updated:"), entry$entry_timestamp)
                        )
                }
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
                                # updateTextInput(session, "startTime", value = format(result$start_time, "%H:%M"))
                                # updateTextInput(session, "endTime", value = format(result$end_time, "%H:%M"))
                                # updateTextInput(session, "breakStart", value = ifelse(is.na(result$break_start), "", format(result$break_start, "%H:%M")))
                                # updateTextInput(session, "breakEnd", value = ifelse(is.na(result$break_end), "", format(result$break_end, "%H:%M")))
                                updateTextAreaInput(session, "tasks", value = result$tasks)
                                updateTextAreaInput(session, "notes", value = result$notes)
                                
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
                
                # Reset the calculated work time
                session$sendCustomMessage(type = 'triggerWorkTimeCalc', message = list())
        }
        
        observeEvent(input$selectDate, {
                req(input$selectDate != "", input$tabs == "View Submissions")
                get_entry_details(input$selectDate, update_form = TRUE)
        })
}

shinyApp(ui, server)
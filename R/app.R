# app.R

library(shiny)
library(DBI)
library(RPostgres)
library(lubridate)

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
                                 dateInput("date", "Date", value = NULL),
                                 textInput("startTime", "Start Time (HH:MM)", value = ""),
                                 textInput("endTime", "End Time (HH:MM)", value = ""),
                                 textInput("breakStart", "Break Start (HH:MM, optional)", value = ""),
                                 textInput("breakEnd", "Break End (HH:MM, optional)", value = ""),
                                 uiOutput("calculatedWorkTime"),
                                 textAreaInput("tasks", "Tasks Accomplished", value = "", rows = 5),
                                 textAreaInput("notes", "Additional Notes", value = "", rows = 3),
                                 actionButton("submit", "Submit/Update Entry"),
                                 actionButton("clear", "Clear Form") 
                        ),
                        tabPanel("View Submissions",
                                 selectInput("selectDate", "Select Date", choices = NULL),
                                 uiOutput("submissionDetails"),
                                 uiOutput("entryHistory")
                        )
                )
        })
        
        workTimeCalc <- reactive({
                start_time <- parse_time(input$startTime)
                end_time <- parse_time(input$endTime)
                break_start <- parse_time(input$breakStart)
                break_end <- parse_time(input$breakEnd)
                
                if (is.na(start_time) || is.na(end_time)) {
                        return(list(time = "-- : --", color = "white"))
                }
                
                total_time <- as.numeric(end_time - start_time, units = "hours")
                
                if (!is.na(break_start) && !is.na(break_end)) {
                        break_time <- as.numeric(break_end - break_start, units = "hours")
                        total_time <- total_time - break_time
                }
                
                color <- if (abs(total_time - 8) < 0.01) "green" else if (total_time < 8) "yellow" else "lightblue"
                
                list(time = sprintf("%.2f", total_time), color = color)
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
                        formatted_time <- format(as.POSIXct(time_str, format = "%H:%M"), "%H:%M:%S")
                        return(list(valid = TRUE, message = NULL, value = formatted_time))
                }, error = function(e) {
                        return(list(valid = FALSE, message = paste(field_name, "must be in HH:MM format."), value = NULL))
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
                
                # Validate and format time inputs
                start_time <- validate_time(input$startTime, "Start Time")
                end_time <- validate_time(input$endTime, "End Time")
                break_start <- validate_time(input$breakStart, "Break Start")
                break_end <- validate_time(input$breakEnd, "Break End")
                
                error_messages <- c()
                if (!start_time$valid) error_messages <- c(error_messages, start_time$message)
                if (!end_time$valid) error_messages <- c(error_messages, end_time$message)
                if (!break_start$valid) error_messages <- c(error_messages, break_start$message)
                if (!break_end$valid) error_messages <- c(error_messages, break_end$message)
                
                if (length(error_messages) > 0) {
                        showModal(modalDialog(
                                title = "Invalid Input",
                                HTML(paste(error_messages, collapse = "<br>")),
                                easyClose = TRUE
                        ))
                        return()
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
                        as.character(start_time$value),
                        as.character(end_time$value),
                        if (is.null(break_start$value)) NA else as.character(break_start$value),
                        if (is.null(break_end$value)) NA else as.character(break_end$value),
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
                                updateTextInput(session, "startTime", value = format(result$start_time, "%H:%M"))
                                updateTextInput(session, "endTime", value = format(result$end_time, "%H:%M"))
                                updateTextInput(session, "breakStart", value = ifelse(is.na(result$break_start), "", format(result$break_start, "%H:%M")))
                                updateTextInput(session, "breakEnd", value = ifelse(is.na(result$break_end), "", format(result$break_end, "%H:%M")))
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
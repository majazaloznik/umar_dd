setup_logging <- function(log_dir = "D:/umar_dd/logs") {  # Use absolute path
        if (!dir.exists(log_dir)) {
                dir.create(log_dir, recursive = TRUE)
        }
        
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        log_file <- file.path(log_dir, paste0("employee_sync_", timestamp, ".txt"))
        
        # Create the file immediately to ensure it exists
        file.create(log_file)
        
        return(log_file)
}

log_entry <- function(level, message, employee_row = NULL, log_file) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        entry <- paste0("[", timestamp, "] ", level, ": ", message)
        if (!is.null(employee_row)) {
                entry <- paste0(entry, " (Row: ", employee_row, ")")
        }
        
        # Open connection in append mode, write, and close immediately
        log_con <- file(log_file, open = "a")
        writeLines(entry, log_con)
        close(log_con)
        
        cat(entry, "\n")  # Also to console/batch output
}

# Read Excel file with employee data
read_employee_excel <- function(excel_path, log_file) {
        tryCatch({
                employees_raw <- openxlsx::read.xlsx(excel_path)
                log_entry("INFO", paste("Successfully read", nrow(employees_raw), "rows from Excel"), 
                          log_file = log_file)
                return(employees_raw)
        }, error = function(e) {
                log_entry("ERROR", paste("Failed to read Excel file:", e$message),
                          log_file = log_file)
                stop("Cannot proceed without Excel data")
        })
}

# Basic structure validation
validate_excel_structure <- function(df, log_file) {
        required_cols <- c("id", "username", "fullname", "contract_type", 
                           "arrival_start", "arrival_end", "departure_start", 
                           "departure_end", "access", "sector")
        
        missing_cols <- setdiff(required_cols, names(df))
        if (length(missing_cols) > 0) {
                log_entry("ERROR", paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
                          log_file)
                return(FALSE)
        }
        
        log_entry("INFO", "Excel structure validation passed", log_file = log_file)
        return(TRUE)
}

# Validate contract types
validate_contract_types <- function(df, log_file) {
        valid_contracts <- c(4, 6, 8)
        invalid_rows <- which(!df$contract_type %in% valid_contracts)
        
        if (length(invalid_rows) > 0) {
                for (row in invalid_rows) {
                        log_entry("ERROR", paste("Invalid contract_type:", df$contract_type[row], 
                                                 "Must be 4, 6, or 8"), 
                                  employee_row = row, log_file = log_file)
                }
                return(FALSE)
        }
        
        log_entry("INFO", "Contract type validation passed", log_file = log_file)
        return(TRUE)
}

# Validate access levels
validate_access_levels <- function(df, log_file) {
        valid_access <- c("vodja", "zaposlen", "admin")
        
        # Convert to lowercase for comparison
        df$access_clean <- tolower(trimws(df$access))
        invalid_rows <- which(!df$access_clean %in% valid_access)
        
        if (length(invalid_rows) > 0) {
                for (row in invalid_rows) {
                        log_entry("ERROR", paste("Invalid access level:", df$access[row], 
                                                 "Must be vodja, zaposlen, or admin"), 
                                  employee_row = row, log_file = log_file)
                }
                return(FALSE)
        }
        
        # Update original with cleaned values
        df$access <- df$access_clean
        df$access_clean <- NULL
        
        log_entry("INFO", "Access level validation passed", log_file = log_file)
        return(TRUE)
}


# Validate and fix usernames
validate_usernames <- function(df, log_file) {
        errors <- FALSE
        
        for (i in 1:nrow(df)) {
                # Generate username if empty (for new employees)
                if (is.na(df$username[i]) || trimws(df$username[i]) == "") {
                        if (!is.na(df$fullname[i]) && trimws(df$fullname[i]) != "") {
                                generated_username <- generate_username(df$fullname[i])
                                df$username[i] <- generated_username
                                log_entry("WARNING", paste("Generated username:", generated_username, 
                                                           "for", df$fullname[i]), 
                                          employee_row = i, log_file = log_file)
                        } else {
                                log_entry("ERROR", "Cannot generate username - fullname is empty", 
                                          employee_row = i, log_file = log_file)
                                errors <- TRUE
                                next
                        }
                }
                
                # Validate username format
                username <- trimws(df$username[i])
                if (grepl("[^a-z0-9]", username)) {
                        log_entry("ERROR", paste("Invalid username format:", username, 
                                                 "- only lowercase letters and numbers allowed"), 
                                  employee_row = i, log_file = log_file)
                        errors <- TRUE
                }
        }
        
        # Check for duplicates
        duplicates <- df$username[duplicated(df$username)]
        if (length(duplicates) > 0) {
                log_entry("ERROR", paste("Duplicate usernames found:", paste(duplicates, collapse = ", ")), 
                          log_file = log_file)
                errors <- TRUE
        }
        
        if (!errors) {
                log_entry("INFO", "Username validation passed", log_file = log_file)
        }
        
        return(list(valid = !errors, data = df))
}

# Helper function to generate username
generate_username <- function(fullname) {
        # Your existing logic
        replace_special_chars <- function(text) {
                text %>%
                        stringr::str_replace_all("č", "c") %>%
                        stringr::str_replace_all("ć", "c") %>%
                        stringr::str_replace_all("š", "s") %>%
                        stringr::str_replace_all("ž", "z")
        }
        
        first_name <- trimws(gsub("^[A-ZČŠŽĆĐ -]+ (.+)$", "\\1", fullname))
        last_name <- trimws(gsub("^([A-ZČŠŽĆĐ -]+)(?= [A-Z][a-z]).*", "\\1", fullname, perl = TRUE))
        
        username <- tolower(paste0(
                stringr::str_sub(first_name, 1, 1),
                stringr::word(last_name, 1)
        ))
        
        return(replace_special_chars(username))
}

# Validate time fields and ordering
validate_times <- function(df, log_file) {
        errors <- FALSE
        
        time_cols <- c("arrival_start", "arrival_end", "departure_start", "departure_end")
        
        for (i in 1:nrow(df)) {
                times <- list()
                
                for (col in time_cols) {
                        time_value <- df[[col]][i]
                        
                        # Check for NA or empty values first
                        if (is.na(time_value) || trimws(as.character(time_value)) == "") {
                                log_entry("ERROR", paste("Time field", col, "cannot be empty"), 
                                          employee_row = i, log_file = log_file)
                                errors <- TRUE
                                times[[col]] <- NA
                                next  # Skip to next column
                        }
                        time_value_clean <- trimws(time_value)
                        # Check if it matches valid time patterns (H:MM, HH:MM, H:MM:SS, HH:MM:SS)
                        if (!grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", time_value_clean)) {
                                log_entry("ERROR", paste("Invalid time format in", col, ":", time_value, 
                                                         "- use HH:MM or HH:MM:SS format"), 
                                          employee_row = i, log_file = log_file)
                                errors <- TRUE
                                times[[col]] <- NA
                                next
                        }
                        # Convert character to numeric if it looks like a decimal
                        if (is.character(time_value)) {
                                # Try to convert to numeric (for Excel decimal times)
                                
                                # Try to parse as time string (HH:MM:SS)
                                tryCatch({
                                        time_parsed <- as.POSIXct(paste("1970-01-01", time_value), tz = "UTC")
                                        times[[col]] <- format(time_parsed, "%H:%M:%S")
                                }, error = function(e) {
                                        log_entry("ERROR", paste("Invalid time format in", col, ":", time_value), 
                                                  employee_row = i, log_file = log_file)
                                        errors <- TRUE
                                        times[[col]] <- NA
                                })
                                
                        } else {
                                log_entry("ERROR", paste("Unexpected time data type in", col, ":", class(time_value)), 
                                          employee_row = i, log_file = log_file)
                                errors <- TRUE
                                times[[col]] <- NA
                        }
                }
                
                # Update dataframe with parsed times
                for (col in time_cols) {
                        if (!is.na(times[[col]])) {
                                df[[col]][i] <- times[[col]]
                        }
                }
                
                # Validate time ordering (skip if any times are NA)
                if (!any(is.na(unlist(times)))) {
                        time_sequence <- unlist(times[c("arrival_start", "arrival_end", "departure_start", "departure_end")])
                        
                        if (!all(time_sequence == sort(time_sequence))) {
                                log_entry("ERROR", "Invalid time sequence - must be: arrival_start < arrival_end < departure_start < departure_end", 
                                          employee_row = i, log_file = log_file)
                                errors <- TRUE
                        }
                }
        }
        
        if (!errors) {
                log_entry("INFO", "Time validation passed", log_file = log_file)
        }
        
        return(list(valid = !errors, data = df))
}

# Validate required fields and fullname format
validate_required_fields <- function(df, log_file) {
        errors <- FALSE
        
        for (i in 1:nrow(df)) {
                fullname <- trimws(df$fullname[i])
                
                # Check if fullname is empty
                if (is.na(fullname) || fullname == "") {
                        log_entry("ERROR", "Fullname cannot be empty", 
                                  employee_row = i, log_file = log_file)
                        errors <- TRUE
                        next
                }
                
                # Check if first word (last name) is in all caps
                first_word <- stringr::word(fullname, 1)
                if (!identical(first_word, toupper(first_word))) {
                        log_entry("ERROR", paste("Last name must be in all caps. Found:", first_word, 
                                                 "Expected:", toupper(first_word)), 
                                  employee_row = i, log_file = log_file)
                        errors <- TRUE
                }
        }
        
        if (!errors) {
                log_entry("INFO", "Required fields validation passed", log_file = log_file)
        }
        
        return(!errors)
}

# Validate sectors against database
validate_sectors <- function(df, log_file, db_connection) {
        # Get valid sectors from database
        valid_sectors_query <- "SELECT sector_name FROM sectors"
        valid_sectors_df <- DBI::dbGetQuery(db_connection, valid_sectors_query)
        valid_sectors <- tolower(trimws(valid_sectors_df$sector_name))
        
        errors <- FALSE
        
        for (i in 1:nrow(df)) {
                sector_value <- tolower(trimws(df$sector[i]))
                
                if (!sector_value %in% valid_sectors) {
                        log_entry("ERROR", paste("Invalid sector:", df$sector[i], 
                                                 "Valid sectors are:", paste(valid_sectors_df$sector_name, collapse = ", ")), 
                                  employee_row = i, log_file = log_file)
                        errors <- TRUE
                }
        }
        
        if (!errors) {
                log_entry("INFO", "Sector validation passed", log_file = log_file)
        }
        
        return(!errors)
}




save_db_table_to_excel <- function(file_path, con){
        
        # save current employees table to Excel
        query <- "
SELECT 
 e.id,
 e.username,
 e.fullname,
 e.contract_type,
 e.arrival_start,
 e.arrival_end,
 e.departure_start,
 e.departure_end,
 e.access,
 s.sector_name AS sector
FROM employees e
LEFT JOIN sectors s ON e.sector = s.id
ORDER BY e.id
"
        
        employees_for_excel <- DBI::dbGetQuery(con, query) |> 
                dplyr::relocate(sector, .after = fullname) |> 
                mutate(across(c(arrival_start, arrival_end, departure_start, departure_end), 
                              ~format(as.POSIXct(paste("1970-01-01", .), tz = "UTC"), "%H:%M:%S")))
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "zaposleni")
        openxlsx::writeDataTable(wb, "zaposleni", employees_for_excel)
        
        # Apply text format to time columns
        text_style <- openxlsx::createStyle(numFmt = "@", locked = FALSE)  # @ = text format
        
        time_cols <- c("arrival_start", "arrival_end", "departure_start", "departure_end")
        max_rows <- 1000
        for (col in time_cols) {
                col_num <- which(names(employees_for_excel) == col)
                if (length(col_num) > 0) {
                        openxlsx::addStyle(wb, "zaposleni", 
                                           style = text_style,  # Text instead of time format
                                           rows = 1:max_rows,  # Include header
                                           cols = col_num,
                                           gridExpand = TRUE)
                }
        }
        
        # Unlock non-time columns with regular unlocked style
        unlocked_style <- openxlsx::createStyle(locked = FALSE)
        non_time_cols <- setdiff(2:ncol(employees_for_excel), 
                                 which(names(employees_for_excel) %in% time_cols))
        
        openxlsx::addStyle(wb, "zaposleni", 
                           style = unlocked_style,
                           rows = 1:max_rows,
                           cols = non_time_cols,
                           gridExpand = TRUE)
        
        # Rest of your protection code...
        openxlsx::protectWorksheet(wb, "zaposleni", 
                                   protect = TRUE, 
                                   password = "spicka2025!",
                                   lockFormattingColumns = FALSE,
                                   lockFormattingRows = FALSE,
                                   lockInsertingRows = FALSE,
                                   lockDeletingRows = FALSE)
        openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
}


## mergeing the validated and updated df


# Read current employees from database
read_current_employees <- function(db_connection, log_file) {
        tryCatch({
                query <- "SELECT id, username, fullname, password, contract_type, 
                     arrival_start, arrival_end, departure_start, departure_end, 
                     access, sector, created_at 
              FROM employees 
              ORDER BY id"
                
                current_employees <- DBI::dbGetQuery(db_connection, query)
                log_entry("INFO", paste("Read", nrow(current_employees), "employees from database"), 
                          log_file = log_file)
                return(current_employees)
        }, error = function(e) {
                log_entry("ERROR", paste("Failed to read current employees:", e$message), 
                          log_file = log_file)
                stop("Cannot proceed without current employee data")
        })
}

# Convert sector names to IDs
map_sectors_to_ids <- function(df, db_connection, log_file) {
        # Get sector mapping
        sector_query <- "SELECT id, sector_name FROM sectors"
        sector_mapping <- DBI::dbGetQuery(db_connection, sector_query)
        
        # Create lookup
        sector_lookup <- setNames(sector_mapping$id, tolower(sector_mapping$sector_name))
        
        # Map sectors
        df$sector_id <- sector_lookup[tolower(df$sector)]
        
        log_entry("INFO", "Mapped sector names to IDs", log_file = log_file)
        return(df)
}


# Prepare data for merge
prepare_merge_data <- function(excel_df, current_db_df, log_file) {
        changes_log <- list()
        
        # Separate new vs existing employees
        new_employees <- excel_df[is.na(excel_df$id) | excel_df$id == "", ]
        existing_employees <- excel_df[!is.na(excel_df$id) & excel_df$id != "", ]
        
        log_entry("INFO", paste("Found", nrow(new_employees), "new employees and", 
                                nrow(existing_employees), "existing employees"), 
                  log_file = log_file)
        
        # Process existing employees - merge with DB data
        if (nrow(existing_employees) > 0) {
                for (i in 1:nrow(existing_employees)) {
                        emp_id <- existing_employees$id[i]
                        db_row <- current_db_df[current_db_df$id == emp_id, ]
                        
                        if (nrow(db_row) == 0) {
                                log_entry("ERROR", paste("Employee ID", emp_id, "not found in database"), 
                                          employee_row = i, log_file = log_file)
                                next
                        }
                        
                        # Create merged record
                        merged_record <- list(
                                id = emp_id,
                                username = existing_employees$username[i],
                                fullname = existing_employees$fullname[i],
                                password = db_row$password,  # Preserve from DB
                                contract_type = existing_employees$contract_type[i],
                                arrival_start = existing_employees$arrival_start[i],
                                arrival_end = existing_employees$arrival_end[i],
                                departure_start = existing_employees$departure_start[i],
                                departure_end = existing_employees$departure_end[i],
                                access = existing_employees$access[i],
                                sector = existing_employees$sector_id[i],
                                created_at = db_row$created_at  # Preserve from DB
                        )
                        
                        # Detect changes (compare with DB record)
                        # Fix the comparison section in prepare_merge_data
                        # Convert DB record to comparable format
                        db_record_clean <- list(
                                username = as.character(db_row$username),
                                fullname = as.character(db_row$fullname),
                                contract_type = as.numeric(db_row$contract_type),
                                arrival_start = as.character(db_row$arrival_start),      # Convert hms to character
                                arrival_end = as.character(db_row$arrival_end),
                                departure_start = as.character(db_row$departure_start),
                                departure_end = as.character(db_row$departure_end),
                                access = as.character(db_row$access),
                                sector = as.numeric(db_row$sector)
                        )
                        
                        excel_record_clean <- list(
                                username = as.character(merged_record$username),
                                fullname = as.character(merged_record$fullname),
                                contract_type = as.numeric(merged_record$contract_type),
                                arrival_start = as.character(merged_record$arrival_start),
                                arrival_end = as.character(merged_record$arrival_end),
                                departure_start = as.character(merged_record$departure_start),
                                departure_end = as.character(merged_record$departure_end),
                                access = as.character(merged_record$access),
                                sector = as.numeric(merged_record$sector)
                        )
                        
                        if (!identical(db_record_clean, excel_record_clean)) {
                                changes_log[[length(changes_log) + 1]] <- list(
                                        change_type = "UPDATE",
                                        employee_id = emp_id,
                                        old_values = jsonlite::toJSON(db_record_clean, auto_unbox = TRUE),
                                        new_values = jsonlite::toJSON(excel_record_clean, auto_unbox = TRUE)
                                )
                                log_entry("INFO", paste("Changes detected for employee ID", emp_id), 
                                          log_file = log_file)
                                log_entry("INFO", paste("OLD:", changes_log[[length(changes_log)]]$old_values), log_file = log_file)
                                log_entry("INFO", paste("NEW:", changes_log[[length(changes_log)]]$new_values), log_file = log_file)
                        }
                        
                        # Add to final data
                        if (i == 1) {
                                final_existing <- data.frame(merged_record)
                        } else {
                                final_existing <- rbind(final_existing, data.frame(merged_record))
                        }
                }
        } else {
                final_existing <- data.frame()
        }
        
        # Process new employees
        if (nrow(new_employees) > 0) {
                new_employees$password <- "123"  # Default password
                new_employees$created_at <- Sys.time()
                new_employees$id <- NA  # Will be assigned by database
                
                for (i in 1:nrow(new_employees)) {
                        changes_log[[length(changes_log) + 1]] <- list(
                                change_type = "INSERT",
                                employee_id = NA,  # Will be updated after insert
                                old_values = jsonlite::toJSON(list(), auto_unbox = TRUE),
                                new_values = jsonlite::toJSON(new_employees[i, ], auto_unbox = TRUE)
                        )
                }
                
                log_entry("INFO", paste("Prepared", nrow(new_employees), "new employees for insert"), 
                          log_file = log_file)
        }
        
        return(list(
                existing = if(exists("final_existing")) final_existing else data.frame(),
                new = if(nrow(new_employees) > 0) new_employees else data.frame(),
                changes = changes_log
        ))
}


# Write changes to database
write_employee_changes <- function(merge_data, db_connection, log_file) {
        success <- TRUE
        new_employee_ids <- c()
        
        tryCatch({
                # Start transaction
                DBI::dbBegin(db_connection)
                
                # Insert new employees
                if (nrow(merge_data$new) > 0) {
                        for (i in 1:nrow(merge_data$new)) {
                                new_emp <- merge_data$new[i, ]
                                
                                insert_query <- "INSERT INTO employees 
                        (username, fullname, password, contract_type, 
                         arrival_start, arrival_end, departure_start, departure_end, 
                         access, sector, created_at) 
                        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
                        RETURNING id"
                                
                                result <- DBI::dbGetQuery(db_connection, insert_query, list(
                                        new_emp$username, new_emp$fullname, new_emp$password, 
                                        as.integer(new_emp$contract_type),
                                        new_emp$arrival_start, new_emp$arrival_end, 
                                        new_emp$departure_start, new_emp$departure_end,
                                        new_emp$access, as.integer(new_emp$sector_id), 
                                        new_emp$created_at
                                ))
                                
                                new_id <- result$id
                                new_employee_ids <- c(new_employee_ids, new_id)
                                log_entry("INFO", paste("Inserted new employee with ID:", new_id), 
                                          log_file = log_file)
                        }
                }
                
                # Update existing employees - ONLY those with changes
                if (nrow(merge_data$existing) > 0) {
                        # Get list of employee IDs that actually changed
                        changed_employee_ids <- sapply(merge_data$changes, function(change) {
                                if (change$change_type == "UPDATE") {
                                        return(change$employee_id)
                                }
                                return(NULL)
                        })
                        changed_employee_ids <- unlist(changed_employee_ids)
                        
                        for (i in 1:nrow(merge_data$existing)) {
                                existing_emp <- merge_data$existing[i, ]
                                
                                # Only update if this employee actually changed
                                if (existing_emp$id %in% changed_employee_ids) {
                                        update_query <- "UPDATE employees SET 
                      username = $1, fullname = $2, contract_type = $3,
                      arrival_start = $4, arrival_end = $5, 
                      departure_start = $6, departure_end = $7,
                      access = $8, sector = $9
                      WHERE id = $10"
                                        
                                        DBI::dbExecute(db_connection, update_query, list(
                                                existing_emp$username, existing_emp$fullname, 
                                                as.integer(existing_emp$contract_type),
                                                existing_emp$arrival_start, existing_emp$arrival_end,
                                                existing_emp$departure_start, existing_emp$departure_end,
                                                existing_emp$access, as.integer(existing_emp$sector),
                                                existing_emp$id
                                        ))
                                        
                                        log_entry("INFO", paste("Updated employee ID:", existing_emp$id), 
                                                  log_file = log_file)
                                }
                        }
                }
                
                # Commit transaction
                DBI::dbCommit(db_connection)
                log_entry("INFO", "Database changes committed successfully", log_file = log_file)
                
        }, error = function(e) {
                DBI::dbRollback(db_connection)
                log_entry("ERROR", paste("Database write failed, rolled back:", e$message), 
                          log_file = log_file)
                success <- FALSE
        })
        
        return(list(success = success, new_ids = new_employee_ids))
}

# Function to check if file is locked
is_file_locked <- function(filepath) {
        tryCatch({
                con <- file(filepath, open = "r+b")
                close(con)
                return(FALSE)
        }, error = function(e) {
                return(TRUE)
        })
}





# Email setup and authentication
setup_email <- function() {
        # You'll need to set up OAuth credentials first
        # This assumes you have gmail credentials configured
        tryCatch({
                home <- "\\\\192.168.38.7\\public$\\Avtomatizacija\\furs-surs-soap\\"
                setwd(home)
                gm_auth_configure(path = "\\\\192.168.38.7\\public$\\Avtomatizacija\\furs-surs-soap\\data\\credentials.json") 
                gm_auth(email = "umar.data.bot@gmail.com", cache = ".secret")  # Update email
                return(TRUE)
        }, error = function(e) {
                cat("Failed to authenticate Gmail: ", e$message)
                return(FALSE)
        })
}

# Send notification email
send_notification_email <- function(log_file, status) {
        if (!setup_email()) {
                log_entry("WARNING", "Could not authenticate email - skipping notification", log_file = log_file)
                return(FALSE)
        }
        
        # Email recipients
        recipients <- c("maja.zaloznik@gmail.com", "peter.hazler@gov.si")  
        
        if (status) {
                subject <- paste("\u0160pi\u010dka\u2122 - uspe\u0161na posodobitev baze zaposlenih")
                body <- paste0(
                        "To je avtomatsko sporo\u010dilo.<br><br>",
                        "Spodaj je log uvoza posodobitve baze zaposlenih za \u0160pi\u010dko\u2122.<br><br>",
                        "Tvoj Umar Data Bot &#129302;<br><br>"
                )
        } else {
                subject <- "\u0160pi\u010dka\u2122 - NE-uspe\u0161na posodobitev baze zaposlenih!"
                body <- paste0(
                        "To je avtomatsko sporo\u010dilo.<br><br>",
                        "V priponki je log uvoza posodobitve baze zaposlenih za \u0160pi\u010dko\u2122.<br><br>",
                        "POSODOBITEV NI BILA USPE\u0160NA!<br><br>",
                        "Tvoj Umar Data Bot &#129302;<br><br>"
                )
        }
        
        # Create and send email
        tryCatch({
                log_entry("INFO", paste("Sending email notification to:", paste(recipients, collapse = ", ")), log_file = log_file)
                
                email <- gm_mime() %>%
                        gm_to(recipients) %>%
                        gm_from("your-sender@gmail.com") %>%  # Update email
                        gm_subject(subject) %>%
                        gm_html_body(body) |> 
                        gm_attach_file(log_file)
                gm_send_message(email)
                return(TRUE)
        }, error = function(e) {
                log_entry("WARNING", paste("Failed to send email notification:", e$message), log_file = log_file)
                return(FALSE)
        })
}

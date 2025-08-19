library(openxlsx)
library(dplyr)
library(gmailr)
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "umar_dd",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"),
                      client_encoding = "utf8")

# # create copies of main tables for testing
# DBI::dbExecute(con, "CREATE TABLE employees_test (LIKE employees INCLUDING ALL)")
# DBI::dbExecute(con, "CREATE TABLE sectors_test (LIKE sectors INCLUDING ALL)")
# DBI::dbExecute(con, "INSERT INTO employees_test SELECT * FROM employees")
# DBI::dbExecute(con, "INSERT INTO sectors_test SELECT * FROM sectors")
# get functions
source("D:/umar_dd/R/functions_employee_table_maintenance.R")
file_path <- "\\\\192.168.38.7\\public$\\Avtomatizacija\\umar-automation-scripts\\data\\spicka\\zaposleni_spicka.xlsx"

# # first time only, create first excel table to be updated manually
# save_db_table_to_excel(file_path, con)

################################################################################
# main script
################################################################################
log_file <- setup_logging()
log_entry("INFO", "Starting employee sync process", log_file = log_file)
if (is_file_locked(file_path)){
        log_entry("ERROR", "Please close Excel and run the sync again",  log_file = log_file)
        send_notification_email(log_file, FALSE)
        stop("Could not run snyc")
} else {
        # Read and validate data
        df <- read_employee_excel(file_path, log_file)
        
        # Run all validations and collect results
        validation_results <- list()
        validation_results$structure <- validate_excel_structure(df, log_file)
        validation_results$sectors <- validate_sectors(df, log_file, con)
        validation_results$contracts <- validate_contract_types(df, log_file)
        validation_results$access <- validate_access_levels(df, log_file)
        validation_results$required <- validate_required_fields(df, log_file)
        
        username_result <- validate_usernames(df, log_file)
        validation_results$usernames <- username_result$valid
        df <- username_result$data
        
        time_result <- validate_times(df, log_file)
        validation_results$times <- time_result$valid
        df <- time_result$data
        
        # Check if all validations passed
        all_valid <- all(unlist(validation_results))
        
        if (!all_valid) {
                log_entry("ERROR", "Validation failed - stopping process", log_file = log_file)
                send_notification_email(log_file, FALSE)
                stop("Validation errors found - check log file")
        } else {
                # Proceed with database operations only if validation passed
                log_entry("INFO", "All validations passed - proceeding with database sync", log_file = log_file)
                cur <- read_current_employees(con, log_file)
                df <- map_sectors_to_ids(df, con, log_file)
                merge <- prepare_merge_data(df, cur, log_file)
                write_result <- write_employee_changes(merge, con, log_file)
                
                if (write_result$success) {
                        log_entry("INFO", "Employee sync completed successfully", log_file = log_file)
                        save_db_table_to_excel(file_path, con)
                        log_entry("INFO", "Saved changes to Excel",  log_file = log_file)
                        send_notification_email(log_file, write_result$success)
                        
                } else {
                        log_entry("ERROR", "Database write failed", log_file = log_file)
                        send_notification_email(log_file, write_result$success)
                }
        }
}



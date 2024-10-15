# funciton to insert employees into database
insert_employee <- function(conn, username, fullname, password, contract_type, 
                            arrival_start, arrival_end, departure_start, departure_end, 
                            access, sector) {
        # Prepare the SQL query
        query <- "INSERT INTO employees (username, fullname, password, contract_type, 
                                     arrival_start, arrival_end, departure_start, 
                                     departure_end, access, sector) 
              VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
              RETURNING id;"
        
        # Convert time inputs to proper format if they're not already
        format_time <- function(time) {
                if (inherits(time, "POSIXt")) {
                        return(format(time, "%H:%M:%S"))
                }
                return(time)
        }
        
        # Format time inputs
        arrival_start <- format_time(arrival_start)
        arrival_end <- format_time(arrival_end)
        departure_start <- format_time(departure_start)
        departure_end <- format_time(departure_end)
        
        # Execute the query
        result <- tryCatch({
                dbGetQuery(conn, query, list(
                        username, 
                        fullname,
                        password, 
                        as.integer(contract_type), 
                        arrival_start, 
                        arrival_end, 
                        departure_start, 
                        departure_end, 
                        access,
                        as.integer(sector)
                ))
        }, error = function(e) {
                message("Error inserting employee: ", e$message)
                return(NULL)
        })
        
        # Check if the insertion was successful
        if (!is.null(result) && nrow(result) > 0) {
                message("Employee inserted successfully with ID: ", result$id)
                return(result$id)
        } else {
                message("Failed to insert employee.")
                return(NULL)
        }
}

# get employee table and clean it up
library(openxlsx)
library(hms)
replace_special_chars <- function(text) {
        text %>%
                stringr::str_replace_all("č", "c") %>%
                stringr::str_replace_all("ć", "c") %>%
                stringr::str_replace_all("š", "s") %>%
                stringr::str_replace_all("ž", "z")
}

employees_raw <- read.xlsx("data/Špička - seznam JU.xlsx")
employees_raw$sector <- as.factor(employees_raw$sector)

# add sector lookup table to database
sector_lookup <- data.frame(
        id = 1:length(levels(employees_raw$sector)),
        sector_name = levels(employees_raw$sector))
dbWriteTable(con, "sectors", sector_lookup, append = TRUE, row.names = FALSE)



# clean up employee data
employee_data <- employees_raw |> 
        mutate(sector = as.nymeric(sector)) |> 
        mutate(
                last_name = gsub("^([A-ZČŠŽĆĐ -]+)(?= [A-Z][a-z]).*", "\\1", fullname, perl = TRUE),
                first_name = trimws(gsub("^[A-ZČŠŽĆĐ -]+ (.+)$", "\\1", fullname))) |> 
        mutate(last_name = trimws(last_name),
               first_name = trimws(first_name),
               username = tolower(paste0(
                       stringr::str_sub(first_name, 1, 1),
                       stringr::word(last_name, 1))),
               username = replace_special_chars(username),
               password = 123) |> 
        mutate(
                arrival_start = as.POSIXct(arrival_start * 86400, origin = "1970-01-01", tz = "UTC"),
                arrival_start = format(arrival_start, "%H:%M"),
                arrival_end = as.POSIXct(arrival_end * 86400, origin = "1970-01-01", tz = "UTC"),
                arrival_end = format(arrival_end, "%H:%M"),
                departure_start = as.POSIXct(departure_start * 86400, origin = "1970-01-01", tz = "UTC"),
                departure_start = format(departure_start, "%H:%M"),
                departure_end = as.POSIXct(departure_end * 86400, origin = "1970-01-01", tz = "UTC"),
                departure_end = format(departure_end, "%H:%M")
        )

# insert employee data into database
employee_data %>%
        purrr::pmap(function(username, fullname, password, contract_type, 
                      arrival_start, arrival_end, departure_start, departure_end, 
                      access, sector, ...) {
                insert_employee(con, username, fullname, password, contract_type, 
                                arrival_start, arrival_end, departure_start, departure_end, 
                                access, sector)
        })

# get employee ids and sectors from database
employees_w_ids <- tbl(con, "employees") |> 
        collect()
# get heads
heads <- employees_w_ids |> 
        filter(access == "vodja") |> 
        pull(id)
# get heads with their employees
sector_heads_and_employees <- sector_lookup %>%
        # First, filter to get only the sector heads
        # Then, join back with the full employee table to get all employees in each sector
        left_join(employees_w_ids %>% select(id, sector), by = c("id" = "sector")) %>%
        # Group by head and sector, and aggregate employee IDs
        group_by(id, sector_name) %>%
        summarise(employee_ids = list(id.y), .groups = "drop") |> 
        # add heads to INOE
        mutate(employee_ids = purrr::map_if(employee_ids, 
                                     id == 1, 
                                     ~unique(c(unlist(.x), heads))))
# turn list to char vector
sector_heads_and_employees_formatted <- sector_heads_and_employees %>%
        mutate(employee_ids = sapply(employee_ids, function(x) paste0("{", paste(x, collapse = ","), "}")))

# write to database table
dbWriteTable(con, "sectors", 
             sector_heads_and_employees_formatted,
             append = TRUE, row.names = FALSE)

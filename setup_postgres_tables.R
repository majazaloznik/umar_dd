# Load necessary libraries
library(DBI)
library(RPostgres)
library(dplyr)

# Assuming `data_macro` is already created as per the given code

# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "umar_dd",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))

#### table creation ############################################################

create_table_sql <- 'CREATE TABLE time_entries (
        id SERIAL PRIMARY KEY,
        user_id INTEGER NOT NULL,
        date DATE NOT NULL,
        start_time TIME NOT NULL,
        end_time TIME NOT NULL,
        break_start TIME,
        break_end TIME,
        caclulated_time INTERVAL,
        lunch BOOLEAN,
        tasks TEXT NOT NULL,
        notes TEXT,
        is_current BOOLEAN DEFAULT TRUE,
        entry_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        
        -- Add any additional constraints
        CONSTRAINT check_times CHECK (
                start_time < end_time
                AND (break_start IS NULL OR break_end IS NULL OR (break_start < break_end))
                AND (break_start IS NULL OR break_start > start_time)
                AND (break_end IS NULL OR break_end < end_time)
        )
);'
# Execute the table creation command
dbExecute(con, create_table_sql)

create_constraint_sql <- 'ALTER TABLE time_entries 
ADD CONSTRAINT unique_current_entry_per_day 
UNIQUE (user_id, date, is_current, entry_timestamp);'


# Execute the table creation command
dbExecute(con, create_constraint_sql)

create_index_sql <- '
-- Create an index on frequently queried columns
CREATE INDEX idx_time_entries_user_date ON time_entries (user_id, date, is_current);'

# Execute the table creation command
dbExecute(con, create_index_sql)

create_emp_table_sql <- 'CREATE TABLE employees (
        id SERIAL PRIMARY KEY,
        username VARCHAR(50) UNIQUE NOT NULL,
        fullname VARCHAR(50) NOT NULL,
        password VARCHAR(50) NOT NULL,
        contract_type int not null,
        arrival_start time not null,
        arrival_end time not null,
        departure_start time not null,
        departure_end time not null,
        access varchar(20) not null,
        sector int not null,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);'

# Execute the table creation command
dbExecute(con, create_emp_table_sql)



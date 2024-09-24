# run_wfh_report.R

library(shiny)

# Set up error logging
log_file <- file("O:/Avtomatizacija/umar_dd/logs/report_log.txt", open = "a")
sink(log_file, type = "message")
sink(log_file, type = "output", append = TRUE)

# Load the app
source("O:/Avtomatizacija/umar_dd/R/app.R")

# Run the app
runApp(
        appDir = "O:/Avtomatizacija/umar_dd/R",
        host = "0.0.0.0",  # This allows connections from other machines
        port = 3838,
        launch.browser = FALSE
)

# Close log sinks
sink(type = "message")
sink(type = "output")
close(log_file)
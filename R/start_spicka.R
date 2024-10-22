# start_shiny_service.R
library(shiny)

# Log file setup
log_dir <- "D:\\umar_dd\\logs"
log_file <- file.path(log_dir, "spicka_log.txt")

# Set up logging
log_con <- file(log_file, open = "a")
sink(log_con, type = "output")
sink(log_con, type = "message")

# Log start of application
cat(paste("Shiny application starting at", Sys.time(), "\n"))

# Source the app (which will now include log rotation logic)
source("D:\\umar_dd\\R\\app.R")

# Run the app
runApp(
        appDir = "D:\\umar_dd\\R",
        host = "0.0.0.0",
        port = 3838,
        launch.browser = FALSE
)
# Close log sinks
sink(type = "message")
sink(type = "output")
close(log_file)
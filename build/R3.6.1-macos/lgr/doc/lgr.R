## ----setup, include = FALSE----------------------------------------------
library(lgr)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
# the root logger is called "lgr"
lgr$info("Vampire stories are generally located in Styria.")

## ------------------------------------------------------------------------
lgr$error("Vampires generally arrive in carriages drawn by %i black horses.", 2)

## ------------------------------------------------------------------------
tf <- tempfile(fileext = ".info")
lgr$add_appender(AppenderFile$new(tf), name = "file")
lgr$info("You must think I am joking")
readLines(tf)

## ------------------------------------------------------------------------
lgr$appenders$file$set_layout(LayoutFormat$new(timestamp_fmt = "%B %d %T"))
lgr$info("No, I am quite serious")
readLines(tf)

#cleanup
unlink(tf)

## ------------------------------------------------------------------------
# cleanup behind the old Appender
unlink(tf)  
lgr$remove_appender("file")

# setup a JSON appender
lgr$add_appender(AppenderJson$new(tf), name = "json")
lgr$info("We lived in Styria")

## ------------------------------------------------------------------------
cat(readLines(tf))

## ------------------------------------------------------------------------
read_json_lines(tf)

## ------------------------------------------------------------------------
# show is a method and takes some extra arguments, like maximum number of lines
# to show
lgr$appenders$json$show()

# $data always returns a data.frame if available. It is an active binding 
# rather than a method, so no extra arguments are possible
lgr$appenders$json$data  

## ------------------------------------------------------------------------
# The default console appender displays custom fields as pseudo-json after the message
lgr$info("Styria has", poultry = c("capons", "turkeys"))

# JSON can store most R objects quite naturally 
read_json_lines(tf)
read_json_lines(tf)$poultry[[2]]  # works because poultry is a list column

## ----echo = FALSE--------------------------------------------------------
lgr$remove_appender("json")
unlink(tf)

## ---- echo = FALSE-------------------------------------------------------
ll <- data.frame(
  `Level` = c(0, seq(100, 600, by = 100), NA),
  `Name` = c("off", "fatal", "error", "warn", "info", "debug", "trace", "all"),
  `Description` = c(
    "Tells a Logger or Appender to suspend all logging",
    "Critical error that leads to program abort. Should always indicate a `stop()` or similar",
    "A severe error that does not trigger program abort",
    "A potentially harmful situation, like `warning()`",
    "An informational message on the progress of the application",
    "Finer grained informational messages that are mostly useful for debugging",
    "An even finer grained message than debug ([more info](https://softwareengineering.stackexchange.com/questions/279690/why-does-the-trace-level-exist-and-when-should-i-use-it-rather-than-debug))",
    "Tells a Logger or Appender to process all log events"
  )
) 

knitr::kable(ll)

## ------------------------------------------------------------------------
lgr$fatal("This is an important message about %s going wrong", "->something<-")
lgr$trace("Trace messages are still hidden")
lgr$set_threshold("trace")
lgr$trace("Unless we lower the threshold")


## ------------------------------------------------------------------------
lgr$info("The sky was the color of %s, tuned to a dead chanel", "television")

## ------------------------------------------------------------------------
lgr$info("Vampire stories are generally located in Styria")
lgr$last_event  # a summary output of the event
lgr$last_event$values  # all values stored in the event as a list

## ------------------------------------------------------------------------
# bad
lgr$info("Processing track '%s' with %s waypoints", "track.gpx", 32)

# Good
tf <- tempfile()
lgr$add_appender(AppenderJson$new(tf), "json")
lgr$info("Processing track", file = "track.gpx", waypoints = 32)
lgr$appenders$json$data


## ----echo = FALSE--------------------------------------------------------
lgr$remove_appender("json")
unlink(tf)

## ------------------------------------------------------------------------
f1 <- function(event) { grepl("bird", event$msg) }
lgr$set_filters(list(f1))

lgr$info("is it a plane?")
lgr$info("no! is it a bird?")

# since this is not a very useful filter, we better remove it again
lgr$set_filters(NULL)

## ------------------------------------------------------------------------
tf <- tempfile()

# Add a new appender to a logger. We don't have to supply a name, but that
# makes it easier to remove later.
lgr$add_appender(AppenderFile$new(file = tf), name = "file")

# configure lgr so that it logs everything to the file, but only info and above
# to the console
lgr$set_threshold(NA)
lgr$appenders$console$set_threshold("info")
lgr$appenders$file$set_threshold(NA)
lgr$info("Another informational message")
lgr$debug("A debug message not shown by the console appender")

readLines(tf)

# Remove the appender again
lgr$remove_appender("file")
unlink(tf)

## ------------------------------------------------------------------------
# install.packages("glue")

lg <- get_logger_glue("glue/logger")

lg$info(
  "glue automatically ", 
  "pastes together unnamed arguments ",
  "and evaluates arbitray expressions inside braces {Sys.Date()}"
)


## ------------------------------------------------------------------------
lg$info("For more info on glue see {website}", website = "https://glue.tidyverse.org/")

## ------------------------------------------------------------------------
lg$info("Glue is available from {.cran}", .cran = "https://CRAN.R-project.org/package=glue")

## ------------------------------------------------------------------------
lg <- get_logger("test")
lg$config(NULL)  # resets logger to unconfigured state
lg$set_threshold("fatal")

## ------------------------------------------------------------------------
lg$
  set_threshold("info")$
  set_appenders(AppenderConsole$new(threshold = "info"))$
  set_propagate(FALSE)

## ------------------------------------------------------------------------
lg$config(list(
  threshold = "info",
  propagate = FALSE,
  appenders = AppenderConsole$new(threshold = "info")
))

## ----eval = FALSE--------------------------------------------------------
#  lg$config("path/to/config.yaml")
#  lg$config("path/to/config.json")

## ------------------------------------------------------------------------
# Via YAML
cfg <- "
  Logger:
    threshold: info
    propagate: false
    appenders:
      AppenderConsole:
        threshold: info
"
lg$config(cfg)

## ------------------------------------------------------------------------
lg <- get_logger("test")
lg$set_appenders(list(cons = AppenderConsole$new()))
lg$set_propagate(FALSE)


lg$info("the default format")
lg$appenders$cons$layout$set_fmt("%L (%n) [%t] %c(): !! %m !!")
lg$info("A more involved custom format")

## ------------------------------------------------------------------------
# install.packages("glue")
library(glue)
lg$appenders$cons$set_layout(LayoutGlue$new(
  fmt = "{.logger$name} {level_name} {caller}: {toupper(msg)}"
))
lg$info("with glue")

## ------------------------------------------------------------------------
# install.packages("jsonlite")
tf <- tempfile()

lg <- get_logger("test")

lg$set_appenders(list(json = AppenderJson$new(file = tf)))
lg$set_propagate(FALSE)

lg$info("JSON naturally ", field = "custom")
lg$info("supports custom", numbers = 1:3)
lg$info("log fields", use = "JSON")


## ----eval = FALSE--------------------------------------------------------
#  lg$appenders$json$data
#  # same as
#  read_json_lines(tf)

## ----echo = FALSE--------------------------------------------------------
lg$appenders$json$data

## ----eval = FALSE--------------------------------------------------------
#  lg$appenders$json$show()
#  # same as
#  cat(readLines(tf), sep = "\n")

## ----echo = FALSE--------------------------------------------------------
lg$appenders$json$show()

## ------------------------------------------------------------------------
# cleanup
lg$config(NULL)
unlink(tf)

## ------------------------------------------------------------------------
# install.packages("rotor")
tf <- tempfile(fileext = ".log")

lg <- get_logger("test")$
  set_propagate(FALSE)$
  set_appenders(list(rotating = AppenderFileRotating$new(
    file = tf, 
    size = "10 kb",
    max_backups = 5))
  )

for (i in 1:100) lg$info(paste(LETTERS, sep = "-"))

# display info on the backups of tf
lg$appenders$rotating$backups

# manually delete all backups
invisible(lg$appenders$rotating$prune(0))
lg$appenders$rotating$backups

#cleanup
unlink(tf)

## ------------------------------------------------------------------------
# The logger name should be the same as the package name
tf <- tempfile()
lg <- get_logger("mypackage")
lg$add_appender(AppenderFile$new(tf))  

## ------------------------------------------------------------------------
print(lg)

## ------------------------------------------------------------------------
lg$info("A test message for lg")

## ------------------------------------------------------------------------
lg$set_propagate(FALSE)

## ------------------------------------------------------------------------
print(lg)

## ------------------------------------------------------------------------
lg$info("Nothing to see here")

## ------------------------------------------------------------------------
# cleanup
lg$config(NULL)
unlink(tf)

## ------------------------------------------------------------------------
lg <- get_logger("buffer")

lg$
  set_threshold(NA)$
  set_propagate(FALSE)$
  set_appenders(
    AppenderBuffer$new(
    threshold = NA,
    buffer_size = 5, # can hold 5 events, the 6th will trigger flushing
    flush_on_exit = FALSE,
    flush_on_rotate = FALSE,
    flush_threshold = "error",
    appenders = AppenderConsole$new(threshold = NA)
  ))

# The for loop below stores 8 log events in the Buffer
for (nm in month.name[1:8]) lg$debug("%s", nm)

# An event of level 'error' or 'fatal' triggers flushing of the buffer
lg$error("But the days grow short when you reach September")

## ------------------------------------------------------------------------
# install.packages("RSQLite")
lg <- get_logger("db_logger")
lg$
  set_propagate(FALSE)$
  add_appender(
    name = "db", 
    AppenderDbi$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      table = "log",
      buffer_size = 2L
    )
  )

lg$info("Logging to databases uses a buffer")
lg$info("As the buffer size is 2, no insert took place till now")
lg$appenders$db$show()

lg$info("Now as the buffer is rotated, all events are output to the db")
lg$appenders$db$show()

## ------------------------------------------------------------------------
# setup an example function
clean   <- function() lgr$info("cleaning data")
process <- function() lgr$info("processing data")
output  <- function() lgr$info("outputing data")

analyze <- function(){
  clean()
  process()
  output()
}

## ----eval = FALSE--------------------------------------------------------
#  with_log_value(
#    list(dataset_id = "dataset1"),
#    analyze()
#  )

## ----eval = FALSE--------------------------------------------------------
#  analyze <- function(id = "dataset1"){
#    lgr$add_filter(FilterInject$new(dataset_id = id), name = "inject")
#    on.exit(lgr$remove_filter("inject"))
#  
#    clean()
#    process()
#    output()
#  }
#  analyze()

## ---- echo = FALSE-------------------------------------------------------
with_log_value(
  list(dataset_id = "dataset1"), 
  analyze()
)

## ------------------------------------------------------------------------
without_logging({
  lgr$warn("Oh Yeah?")
  lgr$fatal("Oh No")
})

## ------------------------------------------------------------------------
# mypackage/R/mypackage-package.R
.onLoad <- function(...){
  assign(
    "lg",  # the recommended name for a logger object
    lgr::get_logger(name = "mypackage"),  # should be the same as the package name
    envir = parent.env(environment())
  )
}



# Shiny 1.5.0 and later auto-source every file in R/ before app.R runs.
# This app sources them itself, in a deliberate order, so autoloading would
# run each file twice. Its presence is what switches that off.
#
# See ?shiny::loadSupport

library(testthat)

test_dir(
  "tests/testthat/",
  # Run in the app's environment containing all support methods.
  env = shiny::loadSupport(appDir = 'dashboard/'),
  # Display the regular progress output and throw an error if any test error is found
  reporter = c("progress", "fail")
)

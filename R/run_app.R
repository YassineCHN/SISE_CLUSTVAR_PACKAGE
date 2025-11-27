#' Launch the Shiny app for exploring ClusteringVariables R Package
#'
#' This function runs the Shiny application included in the package.
#' Shiny is in Suggests, so it must be installed first.
#'
#' @export
run_app <- function() {
  required_pkgs <- c("shiny", "shinyjs", "bslib")

  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      paste0(
        "Please install the following packages to run the app: ",
        paste(missing_pkgs, collapse = ", "),
        ". Use install.packages() to install them."
      )
    )
  }

  shiny::runApp(system.file("shiny", package = "ClusteringVariables"))
}

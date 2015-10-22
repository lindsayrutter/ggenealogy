#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
mywidget <- function(message, width = NULL, height = NULL) {

  # forward options using x
  x = list(
    message = message
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'mywidget',
    x,
    width = width,
    height = height,
    package = 'ggenealogy'
  )
}

#' Widget output function for use in Shiny
#'
#' @export
mywidgetOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'mywidget', width, height, package = 'ggenealogy')
}

#' Widget render function for use in Shiny
#'
#' @export
renderMywidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, mywidgetOutput, env, quoted = TRUE)
}

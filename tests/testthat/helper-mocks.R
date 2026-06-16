# Shared test helpers

make_sess <- function(df = NULL, label = "tbl") {
  if (is.null(df))
    df <- data.frame(
      month = month.abb[1:12],
      value = as.integer(seq(100, 1200, 100)),
      stringsAsFactors = FALSE
    )
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- setNames(list(df), label)
  sess$items  <- list()
  sess$steps  <- list()
  class(sess) <- "pdfmacro_session"
  sess
}

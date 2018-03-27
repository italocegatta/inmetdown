get_form <- function(session, start, end) {

  code_cript <- session %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr('src') %>%
    stringr::str_extract("(?<==)(.*?)(?==)")

  p1 <- key[key$code == stringr::str_sub(code_cript, 1, 3), "key"][[1]]
  p2 <- key[key$code == stringr::str_sub(code_cript, 4, 6), "key"][[1]]

  form <- rvest::set_values(
    rvest::html_form(session)[[1]],
    `dtaini` = format(start, "%d/%m/%Y"),
    `dtafim` = format(end, "%d/%m/%Y"),
    `aleaNum` = paste0(p1, p2)
  )
}


get_data <- function(session, form) {
  x <- 0
  repeat {
    data <- tryCatch(
      {
        suppressMessages(rvest::submit_form(session, form))
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )

    x <- x + 1
    if (!is.null(data)) break
    if (x > 15) break
  }

  data
}


get_table_aws <- function(nodes_table, start, end, end_hour) {
  if (inherits(nodes_table, "try-error")) {
    table <- as.data.frame(matrix(NA_real_, nrow = 2, ncol = 19))
    table[, 1] <- c(start, end)

    if (end == Sys.Date()) {
      table[, 2] <- c(0, end_hour)
    } else {
      table[, 2] <- c(0, 23)
    }

  } else {
    table <- rvest::html_table(nodes_table, header = TRUE)[-1, ]
  }

  table
}


get_table_cws <- function(nodes_table, start, end, n_row) {
  if (inherits(nodes_table, "try-error")) {
    table <- as.data.frame(matrix(NA_real_, nrow = n_row, ncol = 12))
    table[ , 1] <- rep(seq(start, end, by = "day"), each = 3)
    table[ , 2] <- c(0, 12, 18)

    if (end == Sys.Date()) {
      if (t < 13) {
        table <- table[-c(nrow(table),nrow(table)-1), ]
      } else if (t < 19) {
        table <- table[-nrow(table), ]
      }
    }
  } else {
    table <- rvest::html_table(nodes_table, header = TRUE)[-1, ]
  }

  table
}

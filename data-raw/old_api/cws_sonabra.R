# import_sonabra <- function(id, start, end, stations, proxy) {
#
#   n_row <- as.numeric(end - start + 1) * 3
#   t <- lubridate::hour(lubridate::now("UTC"))
#
#   if (is.null(stations)) {
#     stations <- cws_station() %>%
#       dplyr::filter(id %in% !!id)
#   } else {
#     stations <- stations %>%
#       dplyr::filter(id %in% !!id)
#   }
#
#   seq <- seq_along(stations$id)
#   out <- vector("list", length(seq))
#
#   for (i in seq) {
#
#     session <- suppressWarnings(rvest::html_session(stations$url[i], proxy))
#
#     form <- get_form(session, start, end)
#
#     data <- get_data(session, form)
#
#     nodes_table <- try(rvest::html_nodes(data, "table")[[7]], silent = TRUE)
#
#     if (inherits(nodes_table, "try-error")) {
#       table <- as.data.frame(matrix(NA_real_, nrow = n_row, ncol = 12))
#       table[ , 1] <- rep(seq(start, end, by = "day"), each = 3)
#       table[ , 2] <- c(0, 12, 18)
#
#       if (end == Sys.Date()) {
#         if (t < 13) {
#           table <- table[-c(nrow(table),nrow(table)-1), ]
#         } else if (t < 19) {
#           table <- table[-nrow(table), ]
#         }
#       }
#     } else {
#       table <- rvest::html_table(nodes_table, header = TRUE)[-1, ]
#     }
#
#     names(table) <- c(
#       "data", "hora",
#       "t_med", "ur_med", "pa_med",
#       "v_med", "v_dir",
#       "neb","ins",
#       "t_max", "t_min",
#       "ppt"
#     )
#
#     table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(hora:ppt), as.double))
#
#     table <- table %>%
#       dplyr::rowwise() %>%
#       dplyr::mutate(
#         data = as.Date(ifelse(is.character(data), lubridate::dmy(data), data),  origin = "1970-01-01"),
#         date_time = lubridate::ymd_hms(paste0(data, "-", hora, "-00-00"))
#       ) %>%
#       dplyr::ungroup()
#
#     if (nrow(table) != as.numeric(end - start + 1) * 3) {
#
#       range_dttm <- lubridate::ymd_hms(paste0(c(start, end), "-", "00-00-0"))
#
#       if (range_dttm[2] == Sys.Date()) {
#         lubridate::hour(range_dttm[2]) <- lubridate::hour(lubridate::now(tzone = "UTC"))
#       }
#
#       seq_dttm <- data.frame(date_time = seq.POSIXt(range_dttm[1], range_dttm[2], 'hour')) %>%
#         dplyr::filter(lubridate::hour(date_time) %in% c(0, 12, 18))
#
#       table <- dplyr::full_join(table, seq_dttm, by = "date_time")
#     }
#
#     out[[i]] <- table %>%
#       dplyr::mutate(
#         id = stations$id[i],
#         data = lubridate::date(date_time)
#       ) %>%
#       dplyr::group_by(id, data) %>%
#       dplyr::summarise(
#         ppt = mean(ppt, na.rm = TRUE),
#         t_max = mean(t_max, na.rm = TRUE),
#         t_med = mean(t_med, na.rm = TRUE),
#         t_min = mean(t_min, na.rm = TRUE),
#         ur_med = mean(ur_med, na.rm = TRUE),
#         ins = mean(ins, na.rm = TRUE),
#         neb = mean(neb, na.rm = TRUE),
#         pa_med = mean(pa_med, na.rm = TRUE),
#         v_dir = mean(v_dir, na.rm = TRUE),
#         v_med = mean(v_med, na.rm = TRUE)
#       ) %>%
#       dplyr::ungroup() %>%
#       dplyr::mutate_if(is.double, round, digits = 1) %>%
#       tidyr::replace_na(list(
#         ppt = NA, t_max = NA, t_med = NA, t_min = NA,
#         ur_med = NA, ins = NA, neb = NA,pa_med = NA,
#         v_dir = NA, v_med = NA
#       )) %>%
#       dplyr::arrange(id, data) %>%
#       dplyr::as_tibble()
#   }
#
#   dplyr::bind_rows(out)
# }

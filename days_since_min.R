
add_days_since_min <- function(data, col, group, min = 10, date = Date, prefix = "DaysSinceMin") {
  new_col_name <- paste0(prefix, substitute(col)) %>%
    as.symbol()

  q_col <- enquo(col)
  q_date <- enquo(date)

  data %>%
    group_by(!!enquo(group)) %>%
    mutate(!!new_col_name := case_when(
      !!q_col < min ~ -1,
      TRUE ~ 0
    )) %>%
    filter(!is.na(!!q_col), !!new_col_name == 0) %>%
    mutate(
      !!new_col_name := as.numeric(!!q_date - min(!!q_date), unit = "days")
    ) %>%
    ungroup()
}

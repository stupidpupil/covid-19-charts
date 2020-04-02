library("RcppRoll")

add_rolling_seven_day_sums <- function(data, col_to_be_summed, group, date = Date, suffix = "InLastWeek") {
  new_col_name <- paste0(substitute(col_to_be_summed), suffix) %>%
    as.symbol()

  nest_vars <- rlang::ensyms(group, date)

  data %>%
    complete(nesting(!!!nest_vars)) %>%
    group_by(!!enquo(group)) %>%
    mutate(
      !!new_col_name := roll_sum(!!enquo(col_to_be_summed), 7, align = "right", fill = NA),
    )
}

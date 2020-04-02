library("RcppRoll")

add_rolling_seven_day_sums <- function(data, col_to_be_summed, group, date = Date, suffix = "InLastWeek") {
  new_col_name <- paste0(substitute(col_to_be_summed), suffix) %>%
    as.symbol()

  nest_vars <- rlang::ensyms(group, date)

  q_group = enquo(group)
  q_date = enquo(date)

  data %>%
    complete(!!q_group, !!q_date := full_seq(!!q_date, 1)) %>%
    group_by(!!q_group) %>%
    mutate(
      !!new_col_name := roll_sum(!!enquo(col_to_be_summed), 7, align = "right", fill = NA),
    )
}

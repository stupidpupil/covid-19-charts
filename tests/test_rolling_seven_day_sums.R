source("../rolling_seven_day_sums.R")
library(testthat)

test_that("a complete tibble produces the expected result",{
  test_tibble <- tibble(Order = 1:14, Group='A') %>%
    mutate(Cases = row_number()) %>%
    add_rolling_seven_day_sums(Cases, date = Order, group = Group)

    expect_equal(
      test_tibble$CasesInLastWeek, 
      c(NA, NA, NA, NA, NA, NA, sum(1:7), sum(2:8), sum(3:9), sum(4:10), sum(5:11), sum(6:12), sum(7:13), sum(8:14)))
})


test_that("a tibble with missing rows in the middle treats them as zeroes",{
  test_tibble <- tibble(Order = c(1:6,8:14), Cases = Order, Group='A') %>%
    mutate(Cases = row_number()) %>%
    add_rolling_seven_day_sums(Cases, date = Order, group = Group)

    expect_equal(
      test_tibble$CasesInLastWeek,
      c(NA, NA, NA, NA, NA, NA, sum(1:6), sum(2:7), sum(3:8), sum(4:9), sum(5:10), sum(6:11), sum(7:12), sum(7:13)))
})

test_that("a tibble with missing data at end treats them as NAs",{
  test_tibble <- tibble(Order = c(1:14), Cases = Order, Group='A') %>%
    mutate(Cases = ifelse(Order == 14, NA, row_number())) %>%
    add_rolling_seven_day_sums(Cases, date = Order, group = Group)

    expect_equal(
      test_tibble$CasesInLastWeek,
      c(NA, NA, NA, NA, NA, NA, sum(1:7), sum(2:8), sum(3:9), sum(4:10), sum(5:11), sum(6:12), sum(7:13), NA))
})
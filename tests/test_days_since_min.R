source("../days_since_min.R")
library(testthat)
library(tidyverse)


test_that("a complete tibble produces the expected result",{
  test_tibble_a <- tibble(Order = 1:20, Group='A') %>%
    mutate(Cases = row_number())

  test_tibble_b <- tibble(Order = 1:20, Group='B') %>%
    mutate(Cases = row_number()*2)


  test_tibble <- test_tibble_a %>% rbind(test_tibble_b) %>%
    add_days_since_min(Cases, Group, min=10, date=Order)

  expect_equal(test_tibble %>% filter(Group == 'B') %>% nrow(), 16)
  expect_equal(test_tibble %>% filter(Group == 'A') %>% nrow(), 11)

  expect_equal(test_tibble %>% filter(Group == 'B') %>% pull(DaysSinceMinCases) %>% max, 15)
  expect_equal(test_tibble %>% filter(Group == 'A') %>% pull(DaysSinceMinCases) %>% max, 10)

})


test_that("a tibble with missing data in the middle still tracks number of days correctly",{
  test_tibble_a <- tibble(Order = 1:20, Group='A') %>%
    mutate(Cases = ifelse(Order == 14, NA, row_number()))

  test_tibble_b <- tibble(Order = 1:20, Group='B') %>%
    mutate(Cases = ifelse(Order == 14, NA, row_number()*2))

  test_tibble <- test_tibble_a %>% rbind(test_tibble_b) %>%
    add_days_since_min(Cases, Group, min=10, date=Order)

  expect_equal(test_tibble %>% filter(Group == 'B') %>% pull(DaysSinceMinCases) %>% max, 15)
  expect_equal(test_tibble %>% filter(Group == 'A') %>% pull(DaysSinceMinCases) %>% max, 10)

})
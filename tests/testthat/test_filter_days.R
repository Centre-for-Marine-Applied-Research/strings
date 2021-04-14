context("Filter")
library(strings)
library(lubridate)
library(dplyr)

# set up ------------------------------------------------------------------

data(tidy_data)

thresh <- 18.5

dat <- tidy_data %>%
  filter(VARIABLE == "Temperature") %>%
  select(-SENSOR, -DEPLOYMENT_PERIOD, -VARIABLE)

# dates that should be filtered out for each depth
dates_thresh <- dat %>%
  mutate(
    DATE = as_date(TIMESTAMP),
    EXCEED_THRESH = if_else(VALUE >= thresh, TRUE, FALSE)
  ) %>%
  group_by(DEPTH, DATE) %>%
  summarise(n_obs = sum(EXCEED_THRESH)) %>%
  filter(n_obs > 0) %>%
  ungroup()

DATE_2 <- filter(dates_thresh, DEPTH == "2")
DATE_2 <- DATE_2$DATE

DATE_5 <- filter(dates_thresh, DEPTH == "5")
DATE_5 <- DATE_5$DATE

# run function
dat_filtered <- filter_days(dat, threshold = thresh)

filtered_2 <- dat_filtered %>%
  mutate(DATE = as_date(TIMESTAMP)) %>%
  filter(DEPTH == "2", DATE %in% DATE_2)

filtered_5 <- dat_filtered %>%
  mutate(DATE = as_date(TIMESTAMP)) %>%
  filter(DEPTH == "5", DATE %in% DATE_5)

# should be some rows for DEPTH 5 when filtered for DATE_2
filtered_5_check <- dat_filtered %>%
  mutate(DATE = as_date(TIMESTAMP)) %>%
  filter(DEPTH == "5", DATE %in% DATE_2)


# Case 1: Error message
test_that("more than one variable in dat.tidy throws an error",{

  expect_error(filter_days(tidy_data))

})

# Case 2: No filter required
test_that("if all values are below threshold, original data is not changed", {

  expect_equal(filter_days(dat), dat)

})

# Case 3: correct days were filtered
test_that("days with values that exceed threshold are filtered", {

  expect_equal(nrow(filtered_2), 0)
  expect_equal(nrow(filtered_5), 0)

})

test_that("filter for one depth did not impact other depths", {

  expect_equal(filter(dat_filtered, DEPTH == "15"), filter(dat, DEPTH == "15"))
  expect_equal(nrow(filtered_5_check), 142)

})



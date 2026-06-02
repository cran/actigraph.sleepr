## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE,
  comment = "#>", out.width = "75%", fig.asp = 1 / 1.6, fig.width = 5
)
pckg <- c("actigraph.sleepr", "dplyr", "lubridate", "ggplot2")
inst <- suppressMessages(lapply(pckg, library, character.only = TRUE))
theme_set(theme_light())

## -----------------------------------------------------------------------------
file_10s <- system.file("extdata", "ActiSleepPlus-RawData-Day01.agd",
  package = "actigraph.sleepr"
)
agdb_60s <- read_agd(file_10s) %>%
  collapse_epochs(60) %>%
  mutate(date = lubridate::as_date(timestamp)) %>%
  select(date, timestamp, starts_with("axis")) %>%
  group_by(date)
agdb_60s

## -----------------------------------------------------------------------------
periods_nonwear <- agdb_60s %>% apply_troiano(min_period_len = 45)
periods_nonwear

## -----------------------------------------------------------------------------
tail(attributes(periods_nonwear), 8)

## -----------------------------------------------------------------------------
periods_nonwear %>% filter(length >= 60)

## -----------------------------------------------------------------------------
# Find the periods during which the device was worn
periods_wear <- complement_periods(
  periods_nonwear, agdb_60s,
  period_start, period_end
)
periods_wear

# Label each epoch with the period_id of the wear period it falls in
# or with NA if the epoch falls outside a wear period
agdb_wear <- combine_epochs_periods(
  agdb_60s, periods_wear,
  period_start, period_end
)
agdb_wear

# Once we add the wear/nonwear information, it is straightforward to
# compute summary statistics or join with external minute-by-minute data
agdb_wear %>%
  group_by(period_id, .add = TRUE) %>%
  summarise(
    time_worn = n(),
    ave_counts = mean(axis1),
    .groups = "drop_last"
  )

## -----------------------------------------------------------------------------
periods_nonwear <- agdb_60s %>% apply_choi(
  min_period_len = 45,
  min_window_len = 10
)
periods_nonwear

## -----------------------------------------------------------------------------
tail(attributes(periods_nonwear), 5)


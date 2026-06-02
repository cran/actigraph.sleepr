## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE,
  comment = "#>", out.width = "75%", fig.asp = 1 / 1.6, fig.width = 5
)
pckg <- c("actigraph.sleepr", "dplyr", "tidyr", "lubridate", "ggplot2")
inst <- suppressMessages(lapply(pckg, library, character.only = TRUE))
theme_set(
  theme_light() +
    theme(title = element_text(size = 7))
)

## -----------------------------------------------------------------------------
file_10s <- system.file(
  "extdata", "GT3XPlus-RawData-Day01.agd",
  package = "actigraph.sleepr"
)

## -----------------------------------------------------------------------------
# UTC (Coordinated Universal Time) is the default time zone
agdb_10s_raw <- read_agd_raw(file_10s, tz = "UTC")
names(agdb_10s_raw)

## -----------------------------------------------------------------------------
agdb_10s <- read_agd(file_10s, tz = "UTC")
attributes(agdb_10s)[10:12]

## -----------------------------------------------------------------------------
agdb_10s <- agdb_10s %>%
  select(timestamp, starts_with("axis")) %>%
  mutate(magnitude = sqrt(axis1^2 + axis2^2 + axis3^2))
agdb_10s

## -----------------------------------------------------------------------------
plot_activity(agdb_10s, axis1) +
  labs(
    x = "",
    y = "movement (vertical direction)",
    title = "24 hours of activity measured every 10 seconds"
  ) +
  scale_x_datetime(date_labels = "%I%p")

## -----------------------------------------------------------------------------
# Collapse epochs from 10 sec to 60 sec by summing counts
agdb_60s <- agdb_10s %>% collapse_epochs(60)
agdb_60s

## -----------------------------------------------------------------------------
plot_activity(agdb_60s, axis1) +
  labs(
    x = "",
    y = "movement (vertical direction)",
    title = "24 hours of activity data measured every minute"
  ) +
  scale_x_datetime(date_labels = "%I%p")

## ----eval = TRUE--------------------------------------------------------------
library("purrr")

# Construct a path to the directory which contains the raw AGD files
path <- system.file("extdata", package = "actigraph.sleepr")

list.files(path, pattern = "*.agd", full.names = TRUE) %>%
  map_dfr(
    ~ read_agd(.) %>% collapse_epochs(60),
    .id = ".filename"
  )

## -----------------------------------------------------------------------------
agdb_sadeh <- agdb_60s %>% apply_sadeh()

## -----------------------------------------------------------------------------
plot_activity(agdb_sadeh, axis1, color = "sleep") +
  labs(
    x = "",
    y = "movement (bounded at 300)",
    title = paste(
      "For each epoch, Sadeh infers whether",
      "the subject is asleep (red) or awake (blue)"
    )
  ) +
  scale_x_datetime(date_labels = "%I%p") +
  guides(color = FALSE, fill = FALSE)

## -----------------------------------------------------------------------------
agdb_colekripke <- agdb_60s %>% apply_cole_kripke()

## -----------------------------------------------------------------------------
plot_activity(agdb_colekripke, axis1, color = "sleep") +
  labs(
    x = "",
    y = "movement (bounded at 300)",
    title = paste(
      "For each epoch, Cole-Kripke infers whether",
      "the subject is asleep (red) or awake (blue)"
    )
  ) +
  scale_x_datetime(date_labels = "%I%p") +
  guides(color = FALSE, fill = FALSE)

## -----------------------------------------------------------------------------
table(agdb_sadeh$sleep, agdb_colekripke$sleep)

## -----------------------------------------------------------------------------
periods_sleep <- agdb_sadeh %>% apply_tudor_locke()
periods_sleep

## -----------------------------------------------------------------------------
plot_activity_period(
  agdb_60s, periods_sleep, axis1,
  in_bed_time, out_bed_time,
  fill = "#AAAAAA"
) +
  scale_x_datetime(date_labels = "%I%p") +
  labs(
    x = "",
    y = "movement",
    title = paste(
      "Tudor-Locke detects sleep periods in a series of",
      "sleep-scored epochs (Ws and Ss)\n",
      "Sleep periods, if any, are highlighted as gray rectangles"
    )
  )

## -----------------------------------------------------------------------------
periods_awake <- complement_periods(
  periods_sleep, agdb_sadeh,
  in_bed_time, out_bed_time
)
periods_awake

## -----------------------------------------------------------------------------
# Let's label the epochs with a `period_id`, which indicates the awake period
# that each epoch falls in. The ids are consecutive integers starting from 1.
# If the epoch is outside an awake period, then `period_id` is NA.
agdb_awake <- combine_epochs_periods(
  agdb_sadeh, periods_awake,
  period_start, period_end
)
agdb_awake
agdb_awake %>% count(period_id)


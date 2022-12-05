
library(readr)
co2_data <- readr::read_csv("https://datahub.io/core/co2-ppm/r/co2-mm-mlo.csv")

# ggplot(data = co2_data, aes(x = Date)) +
#   geom_line(aes(y = Interpolated)) +
#   geom_line(aes(y = Trend))

temp_data <- readr::read_csv("https://datahub.io/core/global-temp/r/monthly.csv") %>%
  filter(Source == "GCAG")
lubridate::day(temp_data$Date) <-  1

# ggplot(data = temp_data, aes(x = Date)) +
#   geom_line(aes(y = Mean))

temp_co2_data <- co2_data %>%
  janitor::clean_names() %>%
  select(date, co2 = interpolated) %>%
  inner_join(temp_data %>%
               janitor::clean_names() %>%
               select(date, temp = mean),
             by = "date") %>%
  # mutate(co2 = log(co2)) %>%
  mutate(temp = c(rep(NA,12), diff(temp, 12)),
         co2 = c(rep(NA,12), diff(log(co2), 12))) %>%
  drop_na()

usethis::use_data(temp_co2_data, overwrite = TRUE)

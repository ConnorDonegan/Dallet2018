
library(sf)
library(tidyverse)


years <- seq(2010, 2018, by = 2)
sd1 <- tibble(
  district = 1,
  year = years,
  percent_gop = c(60.1, NA, 61.6, NA, 48.6)
)

ad42 <- tibble(
  district = 42,
  year = years,
  percent_gop = c(NA, 56.6, 57.5, 58.66, 53.2)
)

sd10 <- tibble(
  district = 10,
  year = years,
  percent_gop = c(NA, 59.2, NA, 63.22, 44.2)
)


rbind(sd1, sd10, ad42) %>%
  na.omit() %>%
  mutate(district = factor(district)) %>%
  ggplot(aes(year, percent_gop)) +
  geom_point() +
  geom_path(aes(year, percent_gop, color = district)) +
  theme_bw()

# ad58: republicans ran unopposed; then won with 56.6% Jan. 16, 2018
# ad66: democrats run onopposed here, including in 2018.
sc <- read_csv("data/wisconsin/wi-precinct-data-2012-2018.csv")
sd_sc <- sc %>%
  group_by(SEN) %>%
  summarise(dallet = sum(Dallet, na.rm=T), total = sum(total,na.rm=TRUE)) %>%
  mutate(gop_pct = 1 - (dallet/total)) %>%
  filter(SEN %in% c("1", "10"))

ad_sc <- sc %>%
  group_by(ASM) %>%
  summarise(dallet = sum(Dallet, na.rm=T), total = sum(total,na.rm=TRUE)) %>%
  mutate(gop_pct = 1 - (dallet/total)) %>%
  filter(ASM == "42")





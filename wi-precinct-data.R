

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Wisconsin precinct-level election data
# https://data-ltsb.opendata.arcgis.com/datasets/4743010a3c704cb0b52982e7ab4982ff_0
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load pkgs ====
pkgs <- c("tidyverse", "rgdal", "esri2sf", "tmap", "sf", "RColorBrewer")
lapply(pkgs, library, character.only = TRUE)

# load WI election precinct data ====
wi <- st_read("Wards2017_ED12toED16") %>%
  mutate(white18.pct = WHITE18 / PERSONS18,
         black18.pct = BLACK18 / PERSONS18,
         hispanic18.pct = HISPANIC18 / PERSONS18,
         amindian18.pct = AMINDIAN18 / PERSONS18,
         asian18.pct = ASIAN18 / PERSONS18,
         
         clinton = PREDEM16 / PRETOT16,
         trump = PREREP16 / PRETOT16,
         romney = PREREP12 / PRETOT12) %>%
  mutate(gop_change = trump - romney)
mke <- filter(wi, CNTY_FIPS == "55079")
outa <- filter(wi, CNTY_FIPS == "55087")
plot(mke["gop_change"])
wi <- as(wi, "Spatial")

# map all precincts ====

rdbu <- brewer.pal(n = 8, name = "RdBu")

rdbu_range <- colorRampPalette(rdbu)
brks <- seq(1, .05, -.05)

tm_shape(wi) +
  tm_fill("gop_change",
          style = "fixed",
          showNA = F,
          legend.show = F,
          legend.format = list(digits = 2, text.align = "left"),
          breaks = c(-1*brks, 0, rev(brks)),
          palette = rdbu_range(38)) +
  tm_layout(bg.color = "black")


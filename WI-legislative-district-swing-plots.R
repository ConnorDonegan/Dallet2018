

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Will gerrymandering insulate Wisconsin Republicans from a major Democratic swing this November?
# Get partisan lean and returns from Dallet's race for every senate and assembly district
# Plot swing from historic democratic performance to the April '18 Supreme Court race.
# The precinct dataset is not perfect: 3.4% of 6,635 precincts are missing data on the
# April Supreme Court race due to inability to join the datasets and also, perhaps, due
# to changes in the precint map since 2016.
# Also remember that the partisan lean calculated here will differ (be less extreme) than
# would a measure based only on state legislative races; this is due to split ticket voting.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load packages ====
 
pkgs <- c("tidyverse", "data.table", "esri2sf", "sf", "rvest")
lapply(pkgs, library, character.only = TRUE)

# load precinct data ====

unzip("data/wisconsin/shape_zip.zip", exdir = "data/wisconsin/wi-wards")
wi <- sf::st_read("data/wisconsin/wi-wards/data/wisconsin/sf_file/wi-wards-data.shp", quiet =T)
unlink("data/wisconsin/wi-wards", recursive = TRUE)

# summarise election results by district ====

wi <- as.data.frame(wi)
district_history <- function(DistrictData) {
  
  # calculate REP and DEM percentage in each race
  
  races = c( "PRE", "USH", "USS", "GOV", "WSA", "WSS")
  parties <- c("DEM", "REP")
  years <- c("16", "14", "12")
  
  candidate.id <- vector(mode = "character")
  seat <- vector(mode = "character")
  performance <- vector(mode = "numeric")
  year <- vector(mode = "character")
  party <- vector(mode = "character")
  
  for(r in races) {
    for(y in years) {
      for(p in parties) {
        
        election.id <- paste0(r, p, y)
        total.id <- paste0(r, "TOT", y)
        
        if(!election.id %in% names(DistrictData)) next 
        
        candidate_votes <- sum( DistrictData[,which(names(DistrictData)==election.id)], na.rm=TRUE )
        
        total_votes <- sum( DistrictData[,which(names(DistrictData)==total.id)], na.rm=TRUE )
        
        pct <- candidate_votes / total_votes 
        
        if(length(pct) <1) (pct = NA)
        
        candidate.id <- c(candidate.id, election.id)
        seat <- c(seat, r)
        year <- c(year, y)
        party <- c(party, p)
        performance <- c(performance, pct)
      }
    }
  }
  
  # add April 3, 2018 Wisconsin Supreme Court results with Dallet as Democratic favorite
  total_votes <- sum(DistrictData$total, na.rm = T)
  Dallet_votes <- sum(DistrictData$Dallet, na.rm=T)
  Screnock_votes <- total_votes - Dallet_votes
  
  tibble(
    Candidate = candidate.id,
    Seat = seat,
    Year = 2000 + as.numeric(year),
    Party = party,
    Performance = performance
  ) %>%
    dplyr::select(-Candidate) %>%
    spread(key = Party, value = Performance) %>%
    add_row(
      Seat = "WI Supreme Court",
      Year = 2018,
      DEM = Dallet_votes / total_votes,
      REP = Screnock_votes / total_votes
    ) %>%
    mutate(Dem_Margin = DEM - REP,
           Seat = recode_factor(Seat, 
                                PRE = "President",
                                USS = "US Senate",
                                USH = "US House",
                                GOV = "Governor",
                                WSS = "WI Senate",
                                WSA = "WI Assembly", 
                                .ordered = T)) %>%
    arrange(desc(Year), Seat) %>%
    na.omit()
  
}

# senate history
districts <- unique(wi$SEN)
histories <- lapply(1:length(districts), function(i) {
  
  wi %>%
    filter(SEN == districts[i]) %>%
    district_history()
  
})
names(histories) <- districts
sen_histories <- rbindlist(histories, idcol = "District")

# assembly history
districts <- unique(wi$ASM)
histories <- lapply(1:length(districts), function(i) {
  
  wi %>%
    filter(ASM == districts[i]) %>%
    district_history()
  
})
names(histories) <- districts
asm_histories <- rbindlist(histories, idcol = "District")

# calculate partisan lean ====

get_lean <- function(.data) {
  
  filter(.data, Seat %in% c("Governor", "US Senate", "President")) %>%
    mutate(raw_weight = 1 / (2018 - Year)) %>%
    mutate(Weight = raw_weight / sum(raw_weight)) %>%
    group_by(District) %>%
    summarise(Lean = weighted.mean(Dem_Margin, w = Weight))
}

sen_lean <- get_lean(sen_histories)
asm_lean <- get_lean(asm_histories)

# assembly partisanship ====

url = "https://en.wikipedia.org/wiki/Wisconsin_State_Assembly"
tbl.node = '//*[@id="mw-content-text"]/div/table[5]'

wiki <- read_html(url)
asm_held_by <- wiki %>%
  html_nodes(xpath = tbl.node) %>%
  html_table()
asm_held_by <- asm_held_by[[1]]
names(asm_held_by)[c(2,4)] <- c("District", "held_by")
asm_held_by <- asm_held_by %>%
  arrange(District) %>%
  dplyr::select(District, held_by)

asm_held_by$held_by[which(asm_held_by$held_by == "")] <- "Open"

asm_held_by$Party <- dplyr::recode_factor(asm_held_by$held_by,
                                          Rep = "Republican",
                                          Dem = "Democrat")


# Senate partisanship (as of July 2018) ====

sen_held_by <- tibble(
  District = as.character(1:33),
  # https://en.wikipedia.org/wiki/Wisconsin_Legislature
  Held_By = c("Open", "Republican", "Democrat", "Democrat", "Republican", "Democrat", "Democrat", "Republican", "Republican", # the first on this line is SD"Democrat", open
              "Democrat", "Republican", "Republican", "Republican", "Republican", "Democrat", "Democrat", "Republican", "Republican", 
              "Republican", "Republican", "Republican", "Democrat", "Republican", "Republican", "Democrat", "Democrat", "Democrat",
              "Republican", "Republican", "Democrat", "Democrat", "Democrat", "Republican"))

sen_held_by$Held_By <- factor(sen_held_by$Held_By, 
                              ordered = TRUE,
                              levels = c("Republican", "Democrat", "Open"))

# plot assembly swing ====

gdata <- asm_histories %>%
  filter(Seat == "WI Supreme Court") %>%
  dplyr::select(District, Dem_Margin) %>%
  rename(Dallet_Margin = Dem_Margin) %>%
  merge(asm_lean, by = "District") %>%
  arrange(desc(Lean))

gdata$District <- factor(gdata$District, ordered = T, gdata$District)

gdata <- merge(gdata, asm_held_by, by = "District")

assembly_swing <- ggplot(gdata) +
  geom_hline(aes(yintercept=0), col = "gray55") +
  geom_segment(
    aes(x = District, 
        xend = District,
        y = Lean, 
        yend = ifelse(Dallet_Margin > Lean, 
                      Dallet_Margin-0.015,
                      Dallet_Margin+0.015)),
    arrow = arrow(length = unit(0.25, "cm"),
                  ends="last", type = "closed")) +
  geom_point(aes(District, Dallet_Margin, 
                 fill = Party),            # ifelse(party == 1, "Democrat", "Republican")),
             size = 5, 
             shape = 21) +
  geom_point(aes(District, Lean, 
                 fill = Party),            # ifelse(party == 1, "Democrat", "Republican")),
             size = 5, 
             shape = 21) +
  
  scale_y_continuous(name = "Democratic Margin",
                     breaks = seq(-1, 1, .1)) +
  scale_fill_manual(values = c("red3", "skyblue3", "gray60"),
                    # labels = c("Republican", "Democrat", "Open"),
                    name = "Held by") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = c(.8, .5),
        plot.title = element_text(size = 10),
        plot.caption = element_text(size = 8),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  labs(x = "WI Assembly District",
       caption = "Prior Democratic performance is a weighted average of the difference between Democratic and Republican\n vote shares in the Presidential, US Senate, and Gubernatorial elections of 2012, 2014, and 2016.",
       title = "Swing from prior Democratic performance to April Supreme Court race")

png("figures/wi-assembly-swing.png", width =8, height = 13, units = "in", res = 500)
assembly_swing
dev.off()


# plot senate swing ====

gdata <- sen_histories %>%
  filter(Seat == "WI Supreme Court") %>%
  dplyr::select(District, Dem_Margin) %>%
  rename(Dallet_Margin = Dem_Margin) %>%
  merge(sen_lean, by = "District") %>%
  arrange(desc(Lean))

gdata$District <- factor(gdata$District, ordered = T, gdata$District)

gdata <- merge(gdata, sen_held_by, by = "District")

senate_swing <- ggplot(gdata) +
  geom_hline(aes(yintercept=0), col = "gray55") +
  geom_segment(
    aes(x = District, 
        xend = District,
        y = Lean, 
        yend = ifelse(Dallet_Margin > Lean, 
                      Dallet_Margin-0.015,
                      Dallet_Margin+0.015)),
    arrow = arrow(length = unit(0.25, "cm"),
                  ends="last", type = "closed")) +
  geom_point(aes(District, Dallet_Margin, 
                 fill = Held_By),            # ifelse(party == 1, "Democrat", "Republican")),
             size = 5, 
             shape = 21) +
  geom_point(aes(District, Lean, 
                 fill = Held_By),            # ifelse(party == 1, "Democrat", "Republican")),
             size = 5, 
             shape = 21) +
  
  scale_y_continuous(name = "Democratic Margin",
                     breaks = seq(-1, 1, .1)) +
  scale_fill_manual(values = c("red3", "skyblue3", "gray60"),
                    # labels = c("Democrat", "Republican", "Open"),
                    name = "Held by") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = c(.8, .5),
        plot.title = element_text(size = 10),
        plot.caption = element_text(size = 8),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  labs(x = "WI Senate District",
       caption = "Prior Democratic performance is a weighted average of the difference between Democratic and Republican\n vote shares in the Presidential, US Senate, and Gubernatorial elections of 2012, 2014, and 2016.",
       title = "Swing from prior Democratic performance to the April Supreme Court race")

png("figures/wi-senate-swing.png", width =7, height = 8, units = "in", res = 500)
senate_swing
dev.off()






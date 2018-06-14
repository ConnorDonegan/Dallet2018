



# Can gerrymandering protect Wisconsin Republicans?
# Get partisan lean and return's from Dallet's race for every senate and assembly district
# Plot swing from historic democratic performance to the April '18 Supreme Court race.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# load packages ====

knitr::opts_chunk$set(echo = F, message = F, warning = F)
pkgs <- c("tidyverse", "rgdal", "esri2sf", "sf", "raster", "sp", "classInt",
          "tmap", "sf", "RColorBrewer", "kableExtra", "knitr", "rgeos",
          "rvest", "cleangeo")
lapply(pkgs, library, character.only = TRUE)
# tmap_mode("view")

# prep precinct data ====

url <- 'https://github.com/ConnorDonegan/Dallet2018/raw/master/data/wisconsin/shape_zip.zip'
tmp <- tempfile()
download.file(url, tmp)
unzip(tmp, exdir = "wi-wards")
unlink(tmp)

wi <- sf::st_read("wi-wards/data/wisconsin/sf_file/wi-wards-data.shp", quiet =T)

widf <- wi
st_geometry(widf) <- NULL

get.margins <- function(year, seats = c("GOV", "USH", "USS", "WSA", "WSS")){
  
  if(any(str_length(year)>2)) stop("Year must be two digits.")
  
  tdf <- widf
  
  # get all dem results for this year
  dem <- paste0("[A-Z]{3}DEM\\d?", year)
  dem.votes <- names(tdf)[grep(dem, names(tdf))]
  x <- str_extract(dem.votes, "[A-Z]{3}")
  dem.votes <- dem.votes[ which(x %in% seats) ]
  
  # if there was a second dem candidate, just remove it
  if(any( str_count(dem.votes, "\\d") > 2)){
    dem.votes <- dem.votes[-which(str_count(dem.votes, "\\d") > 2)]
  }
  
  # get all GOP results for this year
  rep <- paste0("[A-Z]{3}REP\\d?", year)
  rep.votes <- names(tdf)[grep(rep, names(tdf))]
  x <- str_extract(rep.votes, "[A-Z]{3}")
  rep.votes <- rep.votes[ which(x %in% seats) ]
  
  # if there was a second GOP candidate, just remove it
  if(any( str_count(rep.votes, "\\d") > 2)){
    rep.votes <- rep.votes[-which(str_count(rep.votes, "\\d") > 2)]
  }
  
  # total votes
  the.seats <- str_extract(dem.votes, "[A-Z]{3}")
  the.seats <- paste0(the.seats, "TOT", year)
  total.votes <- names(tdf)[which(names(tdf) %in% the.seats )]
  
  # check that we're combining votes properly
  dem.seat <- str_extract(rep.votes, "[A-Z]{3}") 
  rep.seat <- str_extract(dem.votes, "[A-Z]{3}")
  tots.seat <- str_extract(total.votes, "[A-Z]{3}")
  if(any(dem.seat != rep.seat) | any(rep.seat != tots.seat)) stop("The seats aren't aligned!")
  
  tryCatch(
    {
      
      dem.pct <- widf[, dem.votes] / widf[, total.votes]
      names(dem.pct) <- paste0(dem.votes, "pct")
      
      rep.pct <- widf[, rep.votes] / widf[, total.votes]
      
      dem.margin <- dem.pct - rep.pct
      names(dem.margin) <- paste0(dem.votes, "margin")
      
      x <- cbind(dem.pct, dem.margin)
      
      return(x)
      
    },
    error=function(cond) {
      # catch seat-year combos that don't exist
      message("something went wrong")
      return(rep(NA, nrow(widf)))
    })
}

res12 <- get.margins(year = "12")
res14 <- get.margins(year = "14")
res16 <- get.margins(year = "16")

widf <- cbind(widf, res12, res14, res16)  

trnsfm <- str_detect(names(widf), "[pct margin lean]")
widf[ , trnsfm] <- purrr::map_df(widf[ , trnsfm], round, 3)

st_geometry(widf) <- st_geometry(wi)

project <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
wi <- as(widf, 'Spatial')
wi <- spTransform(wi, CRS(project))

unlink("wi-wards", recursive = TRUE)

# get senate district stats ====

url <- "https://mapservices.legis.wisconsin.gov/arcgis/rest/services/ArcGIS_Online/Senate_Districts_2011/FeatureServer/0"
senate <- esri2sf::esri2sf(url)
senate <- as(senate, 'Spatial')
  # saving and opening again removes orgphaned hole problems
dir.create("senate-shapefile")
writeOGR(senate, dsn = "senate-shapefile", layer = "senate", driver="ESRI Shapefile")
senate <- senate <- readOGR(dsn = "senate-shapefile", layer = "senate")
senate <- spTransform(senate, CRS(project))

# senate <- maptools::readShapePoly("senate-shapefile/senate.shp")

sds <- as.character(senate$SEN_NUM)

res <- tibble(SEN_NUM = 1:33,
              # https://en.wikipedia.org/wiki/Wisconsin_Legislature
              party = as.character(c(NA, 0, 1, 1, 0, 1, 1, 0, 0, # the first on this line is SD1, open
                        1, 0, 0, 0, 0, 1, 1, 0, 0, 
                        0, 0, 0, 1, 0, 0, 1, 1, 1,
                        0, 0, 1, 1, 1, 0)),
              Lean = NA, 
              Dallet.margin = NA, 
              Dallet.pct = NA,
              Walker14.pct = NA,
              Clinton.pct = NA,
              Trump.pct = NA,
              Obama.pct = NA,
              Romney.pct = NA,
              Lean_pres = NA)

for(i in seq_along(sds)){
  
  district <- sds[i]
  sd.sp <- senate[which(senate$SEN_NUM == district), ]
  sd.sp <- gSimplify(sd.sp, tol = 0.000001)
  sd.sp <- raster::intersect(sd.sp, wi)
  
  sd.df <- tibble::as.tibble(sd.sp@data)
  
  # district lean
  dems <- cbind(sd.df$PREDEM12, 
                sd.df$GOVDEM12,  
                sd.df$USSDEM12, 
                sd.df$GOVDEM14, 
                sd.df$USSDEM14, 
                sd.df$USSDEM16,
                sd.df$PREDEM16)
  dems <- colSums(dems)
  
  reps <- cbind(sd.df$PREREP12, 
                sd.df$GOVREP12,  
                sd.df$USSREP12, 
                sd.df$GOVREP14, 
                sd.df$USSREP14, 
                sd.df$USSREP16,
                sd.df$PREREP16)
  reps <- colSums(reps)
  
  tots <- cbind(sd.df$PRETOT12, 
                sd.df$GOVTOT12,  
                sd.df$USSTOT12, 
                sd.df$GOVTOT14, 
                sd.df$USSTOT14, 
                sd.df$USSTOT16,
                sd.df$PRETOT16)
  tots <- colSums(tots)
  
  margins <- (dems - reps)/tots
  w <- c( .03, .03, .03  , .155, .155, .3, .3)
  sd.lean <- weighted.mean(margins, w)
  
  res[ which(res$SEN_NUM == district), "Lean"] <- sd.lean
  
  # President
  total12 <- sum(sd.df$PRETOT12, na.rm=T)
  total16 <- sum(sd.df$PRETOT16, na.rm=T)
  
  obama12 <- sum(sd.df$PREDEM12, na.rm=T)
  romney12 <- sum(sd.df$PREREP12, na.rm=T)
  clinton16 <- sum(sd.df$PREDEM16, na.rm=T)
  trump16 <- sum(sd.df$PREREP16, na.rm=T)
  
  res[ which(res$SEN_NUM == district), "Obama.pct"] <- obama12 / total12
  res[ which(res$SEN_NUM == district), "Romney.pct"] <- romney12 / total12
  res[ which(res$SEN_NUM == district), "Clinton.pct"] <- clinton16 / total16
  res[ which(res$SEN_NUM == district), "Trump.pct"] <- trump16 / total16
  
  pres.margins <- c((obama12 - romney12)/total12, (clinton16 - trump16)/total16)
  w = c(.25, .75)
  sd.pres.lean = round(weighted.mean(pres.margins, w), 2)
  res[ which(res$SEN_NUM == district), "Lean_pres"] <- sd.pres.lean
  
  # Governor 2014
  res[ which(res$SEN_NUM == district), "Walker14.pct"] <- sum(sd.df$GOVREP14, na.rm=T) / sum(sd.df$GOVTOT14, na.rm=T)
  # Dallet 2018
  total.votes <- sum(sd.df$total, na.rm = T)
  dem.votes <- sum(sd.df$Dallet, na.rm=T)
  rep.votes <- sum(sd.df$total - sd.df$Dallet, na.rm=T)
  
  res[ which(res$SEN_NUM == district), "Dallet.pct"] <- dem.votes / total.votes
  
  res[ which(res$SEN_NUM == district), "Dallet.margin"] <- (dem.votes / total.votes) - (rep.votes / total.votes)
  
}

unlink("senate-shapefile", recursive = TRUE)

write_csv(res, "data/wisconsin/senate-districts-election-profile.csv")


# plot senate swing ====

# res <- read_csv("data/wisconsin/senate-districts-election-profile.csv")

res <- arrange(res, desc(Lean))
res$SEN_NUM <- factor(res$SEN_NUM, ordered = T, res$SEN_NUM)

sen.plot <- res %>%
  gather(key = measure, value = margin, -c(SEN_NUM, party)) %>%
  filter(measure %in% c("Lean", "Dallet.margin")) %>%
  ggplot() +
  geom_hline(aes(yintercept=0), col = "gray55") +
  geom_segment(data = res,
               aes(x = SEN_NUM, xend = SEN_NUM,
                   y = Lean, 
                   yend = ifelse(res$Dallet.margin > res$Lean, 
                                 Dallet.margin-0.015,
                                 Dallet.margin+0.015)),
               arrow = arrow(length = unit(0.25, "cm"),
                             ends="last", type = "closed")) +
  geom_point(aes(SEN_NUM, margin, 
                 fill = ifelse(party == 1, "Democrat", "Republican")),
             size = 5, shape = 21) +

  scale_y_continuous(name = "Democratic Margin",
                     breaks = seq(-1, 1, .1)) +
  scale_fill_manual(values = c("skyblue3", "red3"),
                    labels = c("Democrat", "Republican", "Open"),
                    na.value = "gray60",
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

png("figures/wi-senate-swing.png", width =7, height = 6, units = "in", res = 350)
sen.plot
dev.off()

# identify vulnerable districts ====

  # SD 30 looks like it already swung: its actually really volatile
# Democratic	 Dave Hansen Incumbent	51.30%	40,214
# Republican	Eric Wimberger	48.70%	38,175

# vulnerable Republican seats
senate@data[which((senate$party == 0 | is.na(senate$party)) & 
                    senate$Dallet.margin > -.05 & 
                    (!as.numeric(as.character(senate$SEN_NUM))%%2==0)), ]

# vulnerable Democratic seats
senate@data[which((senate$party == 1 & 
                     senate$Dallet.margin < .05 & 
                     (!as.numeric(as.character(senate$SEN_NUM)))%%2==0)), ]

# any SDs with negative lean hold by Dems?
# notice SD 10; that one already flipped.
senate@data[which(senate@data$party == 1 & senate@data$Lean < 0), ]



# get assembly stats ====

url <- "https://mapservices.legis.wisconsin.gov/arcgis/rest/services/ArcGIS_Online/Assembly_Districts_2012_BvB/FeatureServer/0"
ass <- esri2sf::esri2sf(url)
sf::st_write(ass, "data/wi-assembly/ass-dists.shp")
ass <- read_sf("data/wi-assembly/ass-dists.shp", quiet = T)
ass <- as(ass, 'Spatial')
  # write and read again to remove orgphaned hole problems
dir.create("assembly-shapefile")
writeOGR(ass, dsn = "assembly-shapefile", layer = "ass", driver="ESRI Shapefile")
ass <- readOGR(dsn = "assembly-shapefile", layer = "ass")
ass <- spTransform(ass, CRS(project))
unlink("assembly-shapefile", recursive = TRUE)

  # get partisanship
url = "https://en.wikipedia.org/wiki/Wisconsin_State_Assembly"
tbl.node = '//*[@id="mw-content-text"]/div/table[5]'

wiki <- read_html(url)
parties <- wiki %>%
  html_nodes(xpath = tbl.node) %>%
  html_table()
parties <- parties[[1]]
names(parties)[c(2,4)] <- c("AD", "party")
parties <- arrange(parties, AD)
parties$party[which(parties$party == "")] <- NA

res.ass <- tibble(District_N = parties$AD,
              party = parties$party,
              Lean = NA, 
              Dallet.margin = NA, 
              Dallet.pct = NA,
              Walker14.pct = NA,
              Clinton.pct = NA,
              Trump.pct = NA,
              Obama.pct = NA,
              Romney.pct = NA,
              Lean_pres = NA)
ads <- 1:99
for(i in seq_along(ads)){
  
  district <- ads[i]
  ad.sp <- ass[which(ass$District_N == district), ]
  ad.sp <- gSimplify(ad.sp, tol = 0.00001)
  ad.sp <- raster::intersect(ad.sp, wi)
  
  ad.df <- tibble::as.tibble(ad.sp@data)
  
  # district lean
  dems <- cbind(ad.df$PREDEM12, 
                ad.df$GOVDEM12,  
                ad.df$USSDEM12, 
                ad.df$GOVDEM14, 
                ad.df$USSDEM14, 
                ad.df$USSDEM16,
                ad.df$PREDEM16)
  dems <- colSums(dems)
  
  reps <- cbind(ad.df$PREREP12, 
                ad.df$GOVREP12,  
                ad.df$USSREP12, 
                ad.df$GOVREP14, 
                ad.df$USSREP14, 
                ad.df$USSREP16,
                ad.df$PREREP16)
  reps <- colSums(reps)
  
  tots <- cbind(ad.df$PRETOT12, 
                ad.df$GOVTOT12,  
                ad.df$USSTOT12, 
                ad.df$GOVTOT14, 
                ad.df$USSTOT14, 
                ad.df$USSTOT16,
                ad.df$PRETOT16)
  tots <- colSums(tots)
  
  margins <- (dems - reps)/tots
  w <- c( .03, .03, .03  , .155, .155, .3, .3)
  sd.lean <- weighted.mean(margins, w, na.rm=T)
  
  res.ass[ which(res.ass$District_N == district), "Lean"] <- sd.lean
  
  # Pres.assident
  total12 <- sum(ad.df$PRETOT12, na.rm=T)
  total16 <- sum(ad.df$PRETOT16, na.rm=T)
  
  obama12 <- sum(ad.df$PREDEM12, na.rm=T)
  romney12 <- sum(ad.df$PREREP12, na.rm=T)
  clinton16 <- sum(ad.df$PREDEM16, na.rm=T)
  trump16 <- sum(ad.df$PREREP16, na.rm=T)
  
  res.ass[ which(res.ass$District_N == district), "Obama.pct"] <- obama12 / total12
  res.ass[ which(res.ass$District_N == district), "Romney.pct"] <- romney12 / total12
  res.ass[ which(res.ass$District_N == district), "Clinton.pct"] <- clinton16 / total16
  res.ass[ which(res.ass$District_N == district), "Trump.pct"] <- trump16 / total16
  
  pres.ass.margins <- c((obama12 - romney12)/total12, (clinton16 - trump16)/total16)
  w = c(.25, .75)
  sd.pres.ass.lean = round(weighted.mean(pres.ass.margins, w), 2)
  res.ass[ which(res.ass$District_N == district), "Lean_pres.ass"] <- sd.pres.ass.lean
  
  # Governor 2014
  res.ass[ which(res.ass$District_N == district), "Walker14.pct"] <- sum(ad.df$GOVREP14, na.rm=T) / sum(ad.df$GOVTOT14, na.rm=T)
  # Dallet 2018
  total.votes <- sum(ad.df$total, na.rm = T)
  dem.votes <- sum(ad.df$Dallet, na.rm=T)
  rep.votes <- sum(ad.df$total - ad.df$Dallet, na.rm=T)
  
  res.ass[ which(res.ass$District_N == district), "Dallet.pct"] <- dem.votes / total.votes
  
  res.ass[ which(res.ass$District_N == district), "Dallet.margin"] <- (dem.votes / total.votes) - (rep.votes / total.votes)
  
}

write_csv(res.ass, "data/wisconsin/assembly-districts-election-profile.csv")

# plot assembly swing ====

res.ass <- arrange(res.ass, desc(Lean))
res.ass$District_N <- factor(res.ass$District_N, ordered = T, res.ass$District_N)

ass.plot <- res.ass %>%
  gather(key = measure, value = margin, -c(District_N, party)) %>%
  filter(measure %in% c("Lean", "Dallet.margin")) %>%
 ggplot() +
  geom_segment(data = res.ass,
               aes(x = District_N, xend = District_N,
                   y = Lean, 
                   yend = ifelse(res.ass$Dallet.margin > res.ass$Lean, 
                                 Dallet.margin-0.0125,
                                 Dallet.margin+0.0125)),
               arrow = arrow(length = unit(0.25, "cm"),
                             ends="last", type = "closed")) +
  geom_point(aes(District_N, margin,
                 fill = ifelse(party == "Dem", "Democrat", "Republican")),
             size = 5, shape = 21) +
  
  scale_y_continuous(name = "Democratic Margin",
                     breaks = seq(-1, 1, .1)) +
  scale_fill_manual(values = c("skyblue3", "red3"),
                    labels = c("Democrat", "Republican", "Open"),
                    na.value = "gray60",
                    name = "Held by") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = c(.8, .5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) +
  geom_hline(aes(yintercept=0), col = "gray55") +
  labs(x = "WI Assembly District",
       caption = "Prior Democratic performance is a weighted average of the difference between Democratic and Republican vote shares\nin the Presidential, US Senate, and Gubernatorial elections of 2012, 2014, and 2016.",
       title = "Swing from prior Democratic performance to the April Supreme Court race")

png("figures/wi-assembly-swing.png", width =8, height = 12, units = "in", res = 350)
ass.plot
dev.off()
  
  # for plotting results
# ass.sp <- merge(ass, res.ass, by = "District_N")
#




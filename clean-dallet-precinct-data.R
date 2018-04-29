

# Clean precinct-level data on the Arpil 2018 Wisconsin Supreme Court election.
# The data provided by the State of Wisconsin is in a very unfriendly format. 
# This script converts the precinct results into a tidy .csv file; I also merge
# the new data into a simple features (sf) file provided by the WI Legislative 
#Technology Services Bureau that contains demographic data and previous election 
# results at the ward-level.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load packages ====
pkgs <- c("data.table", "tidyverse", "esri2sf", "sf")
lapply(pkgs, library, character.only = TRUE)

# load data, select the right rows and columns ====

sc <- read_csv('data/wisconsin/wi-supreme-court-2018-wards-raw.csv')
sc$dallet_pct <- sc$Dallet / sc$total

# standardize the ward names: all lowercase, no spaces, but leave punctuation in.
sc$ward <- str_to_lower(sc$ward)
sc$ward <- gsub(pattern = "[[:blank:]]", replacement = "", sc$ward)

# remove rows that report the state and county totals
county.totals <- grep('total', sc$ward)
state.total <- grep('Total', sc$county)

sc <- sc[-c(county.totals, state.total), c('county', 'ward', 'Dallet', 'total', 'dallet_pct')]

# extract ward names, municipality type (city, town, or village), and ward numbers from the combined character string ====

# ward names are of the format 'town of adams wards 1, 2, 4-6'
# remove all spaces, extract the town name, remove 'ward' or 'wards' 
# result is a NAME indicator that should match the WI Legislative Technology Bureau precinct dataset
ward_name <- str_extract(sc$ward, '(?<=of)\\D+')
ward_name <- gsub('ward[s]?', "", ward_name)
sc$ward_name <- ward_name

# city, town, or village? Grab the first letter than appears from each row in sc$ward
sc$CTV <- str_extract(sc$ward, '[c,t,v]')

# ward #s
# isolate the combined wards that are written as a sequence (as in 'wards 1-3')
# once stored, remove them from sc$ward. Notice that some towns have multiple
# such lists, as in 'town of adams wards 1, 6-9, 11-12' or'1-2c'.
# str_extract_all should get all occurences, but it does not. 
# So loop through with str_extract and each time, pick up the first numeric character(s) 
# and then remove them before searching through again
w <- sc$ward
sequenced.wards <- str_extract(w, '\\d+[[:alpha:]]?-\\d+[[:alpha:]]?')
w <- str_remove(w, '\\d+[[:alpha:]]?-\\d+[[:alpha:]]?')

while(any(str_detect(w, '\\d+[[:alpha:]]?-\\d+[[:alpha:]]?'))) {
  
  sequencex <- str_extract(w, '\\d+[[:alpha:]]?-\\d+[[:alpha:]]?')
  sequenced.wards <- cbind(sequenced.wards, sequencex)
  w <- str_remove(w, '\\d+[[:alpha:]]?-\\d+[[:alpha:]]?')
  
}
# now extract comma-separated single ward numbers (as in 'adams wards 2,4'), 
# and loop through for multiples again
wards <- str_extract(w, '\\d+[[:alpha:]]?')
w <- str_remove(w, '\\d+[[:alpha:]]?')

while(any(str_detect(w, '\\d+[[:alpha:]]?'))) {
  
  wx <- str_extract(w, '\\d+[[:alpha:]]?')
  wards <- cbind(wards, wx)
  w <- str_remove(w, '\\d+[[:alpha:]]?')
  
}
# append the new ward lists to the voting results
sc <- cbind(sc, wards, sequenced.wards)


# take sequences (e.g. '1-3') and expand them (to '1 2 3') ====

expand.sequence <- function(x) {
  
  if(is.na(x)) return(NA)
  tryCatch(
    {
      
      x <- str_split(x, '-', simplify = T)
      x <- as.numeric(x)
      x <- seq(x[1], x[2], 1)
      return(x)
      
    },
    error=function(cond) {
      # catch wards with alphabetic sequences ('adams 1-2c')
      return(tibble('seq.fail'))
    })
}

# just take the columns with sequenced.wards
seq.cols <- which(names(sc)=='sequenced.wards'):ncol(sc)
d <- sc[, seq.cols]

# and apply expand.sequence to every cell 
zz <- apply(d, MARGIN = 1:2, FUN =expand.sequence)
zz <- apply(zz, MARGIN = 1, FUN = function(x) as.tibble(matrix(unlist(x),nrow=1)))
zz <- rbindlist(zz, fill = T)

# replace all sequenced.wards with the expanded sequences in the sc dataframe
sc <- sc[, -(which(names(sc) == "sequenced.wards"):ncol(sc))]
sc <- cbind(sc, zz)

# factors cause problems
sc[ , -(3:5)] <- map_df(sc[, -(3:5)], as.character)

# expand alphabetic sequences ====

# See bayside, lagrange, mequon, wolfriver, menasha
# View( sc[which(str_detect(sc$ward, '\\d+[[:lower:]]')),] )
# View(sc[which(sc$ward == 'cityofmenashawards1-2,4,7'),])

# they don't indicate where the sequence ends (as in '5a-6'); must reference 2017 wards
# wi$STR_WARDS <- str_to_lower(wi$STR_WARDS)
# y <- wi[which(str_detect(wi$STR_WARDS, '[a-z]')),]
# View(y)
# rm(y, wi)

alphas <- list(wardx = c(
  'villageofbaysidewards1-1s,3-3s',
  'townoflagrangewards1a-1b',
  'townoflagrangewards2a-2b,3a-3b',
  'cityofmequonwards5-7b',
  'townofwolfriverwards1-2c',
  'cityofmenashawards5a-6,8-9,23-29,31-35,38-42',
  # these final menasha wards were entered incorrectly in the original file: should have been '1a-2'
  'cityofmenashawards1-2,4,7'
),
wards = list(
  c('1', '1s', '3', '3s'),
  c('1a', '1b'),
  c('2a', '2b', '3a', '3b'),
  c('5', '6', '7a', '7b'),
  c('1', '2a', '2b', '2c'),
  c('5a', '5b', '6', 8:9, 23:29, 31:35, 38:42),
  c('1a', '1b', '2', '4', '7')
)
)

# expand sc to hold new data
new.cols <- matrix(NA, ncol = 2, nrow = nrow(sc))
sc <- cbind(sc, new.cols)

for(i in seq_along(alphas$wardx)) {
  
  wardx <- alphas$wardx[i]
  wards <- alphas$wards[[i]]
  # erase original values first---replacement values do not all align with original values
  start.column <- which(names(sc)=="wards")
  sc[which(sc$ward == wardx), start.column:ncol(sc)] <- NA
  sc[which(sc$ward == wardx), start.column:(start.column+length(wards)-1)] <- wards
  
}


# View( sc[which(str_detect(sc$ward, '\\d+[[:lower:]]')),] )
# View(sc[which(sc$ward == 'cityofmenashawards1-2,4,7'),])

# spread vote count among combined wards ====

# count wards per row
start.col <- which(names(sc)=="wards")
denom <- apply(sc[ , start.col:ncol(sc)], MARGIN = 1, FUN =  function(x) sum(!is.na(x)))
# divide vote count by number of wards
sc$Dallet <- sc$Dallet/denom
sc$total <- sc$total / denom

# fill in county names ====

county.idx <- which(!is.na(sc$county))

for(i in seq_along(county.idx)) {
  
  if(i < 72) {
    
    sc$county[county.idx[i]:(county.idx[i+1]-1)] <- sc$county[county.idx[i]]
    
  } else {
    
    sc$county[county.idx[i]:nrow(sc)] <- sc$county[county.idx[i]]
  }
  
}



# put sc into long/tidy format ====

sc_tidy <- sc %>%
  gather(key = delete.me, value = STR_WARDS, 
         -c(county, ward_name, ward, CTV, Dallet, total, dallet_pct)) %>% 
  as.tibble() %>%
  dplyr::select(-c(delete.me, ward)) %>%
  na.omit() %>%
  mutate(STR_WARDS = str_pad(STR_WARDS, side = 'left', width = 4, pad = '0')) %>%
  rename(CNTY_NAME = county, NAME = ward_name)

# test that ward vote count equal the vote share
sum(round(sc_tidy$Dallet / sc_tidy$total,2) == round(sc_tidy$dallet_pct,2)) == nrow(sc_tidy)

# standardize identifiers: lowercase, no spacing, no punctuation
excluded.numeric <- which(names(sc_tidy) %in% c('Dallet', 'total', 'dallet_pct'))
standardize_names <- function(x) {
  
  x <- str_to_lower(as.character(x))
  x <- gsub(pattern = "[[:blank:]]", replacement = "", x)
  x <- gsub(pattern = "[[:punct:]]", replacement = "", x)
  return(x)
  
}
sc_tidy[ , -excluded.numeric] <- map_df(sc_tidy[, -excluded.numeric], standardize_names)
sc_tidy <- sc_tidy[,c('CNTY_NAME', 'NAME', 'STR_WARDS', 'CTV', 'Dallet', 'total', 'dallet_pct')] %>%
  as.tibble()



# Prepare WI Legislative Tech Bureau's sf file to merge with sc_tidy ====

# ward-level wisconsin election and demographic data
# you can download the GEOJSON file as a simple features file this way
# hist.ward.data <- "https://services1.arcgis.com/FDsAtKBk8Hy4cAH0/arcgis/rest/services/Wards_2011_wED_2016_2012/FeatureServer/0"
# wi <- esri2sf(hist.ward.data)

# but the repository includes a copy of the current version
wi <- st_read("data/wisconsin/Wards2017_ED12toED16")
wi$CNTY_FIPS <- as.character(wi$CNTY_FIPS)

# # 376 CNTY_NAMEs are NA, but all CNTY_FIPS are present---all look good now...
wi.fips <- read.table(file = "data/wisconsin/wi-county-fips-codes.txt",
                      sep = "\t",
                      header=T,
                      stringsAsFactors = F)[,-1]

wi.fips$CNTY_FIPS <- as.character(wi.fips$CNTY_FIPS)

wi <- inner_join(wi, wi.fips, by = "CNTY_FIPS") %>%
  rename(CNTY_NAME = CNTY_NAME.y) %>%
  select(-c(CNTY_NAME.x))


# municipal IDs: CTV indicates town, city, or village
# https://data-ltsb.opendata.arcgis.com/datasets/b21fb59c8e8a4acab2b967de34b648ce_0/data
munis <- read_csv('data/wisconsin/WI_Municipal_Wards_Spring_2018.csv') %>%
  dplyr::select(GEOID, CTV)

wi <- left_join(wi, munis, by = 'GEOID', all.x = T)

wi$NAME <- standardize_names(wi$NAME)
wi$CNTY_NAME <- standardize_names(wi$CNTY_NAME)
wi$STR_WARDS <- standardize_names(wi$STR_WARDS)
wi$CTV <- standardize_names(wi$CTV)

# merge sc_tidy with wi ====

wi2 <- left_join(wi, sc_tidy)

# save as .csv and sf ====

write_csv(sc_tidy, 'data/wisconsin/wi-supreme_court-2018-wards-clean.csv')

dir.create("data/wisconsin/sf_file")
shp.path <- "data/wisconsin/sf_file/wi-wards-data.shp"
st_write(wi2, shp.path)

files2zip <- dir("data/wisconsin/sf_file", full.names = T)

zip(zipfile = "data/wisconsin/shape_zip",
    files = files2zip)
unlink('data/wisconsin/sf_file', recursive=TRUE)

# to open the zip file and start using the simple features file, run this:
# unzip("data/wisconsin/shape_zip.zip", exdir = "wi-wards")
# sp <- st_read("wi-wards/data/wisconsin/sf_file/wi-wards-data.shp")
# plot(sp['dallet_pct'])














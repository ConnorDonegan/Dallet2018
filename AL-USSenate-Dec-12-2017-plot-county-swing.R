


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# get precinct and county level election turns for Alabama in the 2016 Presidential race and the special Senate election of 2017
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load packages ====
pkgs <- c("readxl", "tidyverse", "data.table", "scales")
lapply(pkgs, library, character.only = TRUE); rm(list=ls())

# load alabama 2016 presidential election results ====

tf <- tempfile(pattern = "president", tmpdir = tempdir(), fileext = ".zip")
td <- tempdir()
url = "https://sos.alabama.gov/sites/default/files/election-data/2017-06/2016-General-PrecinctLevel.zip"
download.file(url, destfile = tf)
td.pres <- paste0(td, "\\", "president")
dir.create(td.pres)
unzip(tf, exdir = td.pres)

 # each county has its own excel file. 
 # Calculate precinct-level Clint and Trump vote shares and aggregate for each county
al.pres.data <- list.files(td.pres)[grep(".xls", list.files(td.pres))]
al.prec.pres <- list()
al.county.pres <- list()
for(i in seq_along(al.pres.data)){
  
  al.file <- paste0(td.pres,"\\", al.pres.data[i])
  
  x <- read_xls(al.file)  
  
  get.rows <- c(grep("Trump", x$Candidate),
                grep("Hillary Rodham Clinton", x$Candidate),
                grep("Ballots Cast - Total", x$Candidate)) 
  rm.cols <- which(names(x) %in% c("Contest Title", "Party"))
  
  x <- x[get.rows, -rm.cols]
  
  x <- x %>%
    gather(key = precincts, value = votes, -Candidate) %>%
    spread(key = Candidate, value = votes)
  
  names(x)[grep("Trump", names(x))] <- "Trump"
  names(x)[grep("Clinton", names(x))] <- "Clinton"
  names(x)[grep("Total", names(x))] <- "Total"
  names(x)[grep("precincts", names(x))] <- "Precinct"
  
  x <- x %>% 
    mutate(trump_pct = Trump / Total,
           clinton_pct = Clinton / Total) 
  
  x$Precinct <- str_remove(x$Precinct, "^[[:digit:]]{4} - ")
  x$Precinct <- str_remove_all(x$Precinct, "[[:punct:]]")
  
  al.prec.pres[[i]] <- x %>% filter(!Precinct %in% c("ABSENTEE", "PROVISIONAL"))
  
  county <-  al.pres.data[i]
  
  x <- x %>% 
    summarise(County = county,
              Total = sum(Total),
              Trump = sum(Trump),
              Clinton = sum(Clinton)) %>%
    mutate(trump_pct = Trump / Total,
           clinton_pct = Clinton / Total)
  
  al.county.pres[[i]] <- x    
  
 }
al.prec.pres <- data.table::rbindlist(al.prec.pres)
al.county.pres <- data.table::rbindlist(al.county.pres)

# load alabama special election results 2017 ====

tf <- tempfile(pattern = "senate", tmpdir = tempdir(), fileext = ".zip")
td <- tempdir()
url = "https://sos.alabama.gov/sites/default/files/election-data/2018-03/2017-Special-General-PrecinctLevel.zip"
download.file(url, destfile = tf)
td.senate <- paste0(td, "\\", "senate")
dir.create(td.senate)
unzip(tf, exdir = td.senate)

 # each county has its own excel file. 
 # Calculate precinct-level Jones and Moore vote shares and aggregate for each county 
al.sen.data <- list.files(td.senate)[grep(".xls", list.files(td.senate))]
al.prec.senate <- list()
al.county.senate <- list()
for(i in seq_along(al.sen.data)) {
  
  al.file <- paste0(td.senate, "\\", al.sen.data[i])
  
  x <- read_xls(al.file) 
  
  get.rows <- which(x$Candidate %in% c("Roy S. Moore", "Doug Jones", "Ballots Cast - Total"))
  rm.cols <- which(names(x) %in% c("Contest Title", "Party"))
  
  x <- x[get.rows, -rm.cols] 
  
  x <- x %>%
    gather(key = precincts, value = votes, -Candidate) %>%
    spread(key = Candidate, value = votes)
  
  names(x)[grep("precincts", names(x))] <- "Precinct"
  names(x)[grep("Jones", names(x))] <- "Jones"
  names(x)[grep("Moore", names(x))] <- "Moore"
  names(x)[grep("Total", names(x))] <- "Total"
  
  x <- x %>%
    mutate(jones_pct = Jones / Total,
           moore_pct = Moore / Total) 
  
  x$Precinct <- str_remove(x$Precinct, "^PREC [[:digit:]]{4} - ")
  x$Precinct <- str_remove_all(x$Precinct, "[[:punct:]]")
  
  al.prec.senate[[i]] <- x %>% filter(!Precinct %in% c("ABSENTEE", "PROVISIONAL"))
  
  county <-  al.sen.data[i] 
  
  x <- x %>% 
    summarise(County = county,
              Total = sum(Total),
              Moore = sum(Moore),
              Jones = sum(Jones)) %>%
    mutate(moore_pct = Moore / Total,
           jones_pct = Jones / Total)
  
  al.county.senate[[i]] <- x    
  
  }
al.prec.senate <- data.table::rbindlist(al.prec.senate)
al.county.senate <- data.table::rbindlist(al.county.senate)


# merge county results  ====

# extract county names from file names
al.county.senate$County <- str_remove(al.county.senate$County, "2017-General-")
al.county.senate$County <- str_remove(al.county.senate$County, "\\.xls")

al.county.pres$County <- str_remove(al.county.pres$County, "2016-General-")
al.county.pres$County <- str_remove(al.county.pres$County, "\\.xls")

ala.county <- merge(al.county.pres, al.county.senate, by = "County") %>% 
  as.tibble() %>%
  mutate(change_dem = jones_pct - clinton_pct)

# save results in case any changes are made to the Sec. of State webpage
write_csv(ala.county, "data/alabama/county-results-20162017.csv")

# merge precinct results ====

  # note that not all precincts match between elections: 
  # there were 112 more precincts in the presidential race, irregular formating also causes non-matches
   length(intersect(al.prec.pres$Precinct, al.prec.senate$Precinct)) # N = 1,523 precincts 
   length(unique(al.prec.pres$Precinct)) - length(unique(al.prec.senate$Precinct))

 # merge precinct-level 2016 Presidential results with 2017 Senate results; calculate change in % Democrat   
ala.prec <- merge(al.prec.pres, al.prec.senate, by = "Precinct") %>% 
  as.tibble() %>%
  mutate(change_dem = jones_pct - clinton_pct) 

# save results in case any changes are made to the Sec. of State webpage
write_csv(ala.prec, "data/alabama/precinct-results-20162017.csv")

# plot results ====

p <- read_csv("data/alabama/precinct-results-20162017.csv") %>%
  mutate(level = "precinct") %>%
  rename(Place = Precinct)
c <- read_csv("data/alabama/county-results-20162017.csv") %>%
  mutate(level = "county") %>%
  rename(Place = County)

al <- rbind(c,p)

color.vec <- ifelse(al$level == "precinct", "black", "darkblue")
alpha.vec <- ifelse(al$level == "precinct", .2, 1)

al.all <- ggplot(al) +
  geom_point(aes(trump_pct, change_dem, 
                 colour = color.vec,
                 size = Total.x), 
              alpha = alpha.vec,
              shape = 1) +
  scale_color_manual(values = c("gray30", "darkblue")) +
  scale_x_continuous(breaks = seq(-.1, 1, .1),
                     name = "Trump 2016",
                     labels = percent) +
  theme_bw()  +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, colour = "black") +
  stat_smooth(data = subset(al[which(al$level == "precinct"),]), 
              aes(trump_pct, change_dem),
              method="lm", 
              fullrange = TRUE,
              se=TRUE, 
              fill=NA,
              formula=y ~ splines::bs(x, 3),colour="darkblue") 
  
# a few extreme outliers pull down the expected value when % Trump is less than 10%
al.all

# outliers removed and a more meaningful scale
al.plot <- al.all + 
  scale_y_continuous(breaks = seq(-1, 1, .05),
                     labels = percent,
                     name = "Jones 2017 minus Clinton 2016",
                     limits = c(-.1, .3)) +
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        plot.caption = element_text(size = 8)) +
  labs(title = "Democratic swing and prior support for Trump\nfor all Alabama counties (blue) and precincts (gray)",
          caption = "The fit line shows the average Democratic swing for any given level of Trump support in 2016.\nA small number of precincts are beyond the scale of this plot.")

ggsave("figures/alabama-dem-swing-plot.jpg", al.plot, width = 6.25, height = 5.25)


  
  

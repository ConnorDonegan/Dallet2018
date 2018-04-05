pkgs <- c("readxl", "rvest", "tidyverse", "scales")
lapply(pkgs, library, character.only = TRUE); rm(list=ls())


# 2012 election results ====
url <- "http://elections.wi.gov/sites/default/files/County%20by%20County_11.6.12.xls"
download.file(url, destfile = "data/wisconsin/president2012.xls", method = "auto",
              mode = "wb")
pres12 <- read_excel("data/wisconsin/president2012.xls",
                   sheet = 2, skip = 6)[, c(1,3,4,5)]
names(pres12) <- c("County", "Total_2012", "Romney", "Obama")
pres12 <- pres12 %>%
  mutate(romney_pct = Romney / Total_2012,
         obama_pct = Obama / Total_2012,
         County = stringr::str_to_title(County))

# 2016 election results ====
pres.2016 <- "http://elections.wi.gov/sites/default/files/County%20by%20County%20Report%20President%20of%20the%20United%20States%20Recount.xlsx"
download.file(pres.2016, destfile = "data/wisconsin/president2016.xlsx", method = "auto",
              mode = "wb")
pres16 <- read_excel("data/wisconsin/president2016.xlsx", skip = 6)[, c(1,3,4,5)]
names(pres16) <- c("County", "Total_2016", "Trump", "Clinton")
pres16 <- pres16 %>%
  mutate(trump_pct = Trump / Total_2016,
         clinton_pct = Clinton / Total_2016,
         County = stringr::str_to_title(County))

# get WI Supreme Court 2018 election results ====
url <- "https://elections.ap.org/wpr/election_results/2018-04-03/state/WI/race/G/raceid/50888"
columns <- ".vote-percentage , .vote-count , .candidate"
county.names <- ".results-header h2"
wi<- read_html(url)

 # read results
SC <- wi %>%
  html_nodes(columns) %>%
  html_text() %>% 
  matrix(byrow = TRUE, ncol = 3) %>%
  as.tibble() %>%
  slice(-c(1,2))  # remove state totals

 # 
county.names <- wi %>%
  html_nodes(county.names) %>%
  html_text() %>%
  as.matrix(ncol = 1, byrow=T) %>%
  as.tibble() 

 # Lincoln is listed last on the website; make it alphabetically last here too
county.names$V1[grep("Lincoln", county.names$V1)] <- "ZLincoln" 

county.names <- county.names %>%
  rbind(county.names) %>%
  arrange(V1)

SC$County <- county.names$V1
names(SC) <- c("Candidate", "Total_SC", "Percent", "County")
SC$Percent <- as.numeric( str_remove(SC$Percent, " %") )

 # fix Lincoln now
SC$County[grep("ZLincoln", SC$County)] <- "Lincoln"
# SCSave <- SC

SC <- SC %>%
  dplyr::select(Candidate, Percent, County) %>%
  spread(key = Candidate, value = Percent)
names(SC) <- c("County", "dallet_pct", "screnock_pct")
SC$County <- str_to_title(SC$County)
# merge all three ====

wi <- merge(pres12,pres16, by = "County") %>% 
  mutate(gop_change = trump_pct - romney_pct) %>%
  merge(SC, by = "County") %>%
  mutate(dem_change = .01*dallet_pct - clinton_pct) %>%
  as.tibble()

saveRDS(wi, "data/wisconsin/supreme-court-2018.rds")

dem_growth1 <- ggplot(wi) +
  geom_point(aes(trump_pct, dem_change, 
                 col = wi$dem_change > 0,
                 size = Total_2016), 
             shape = 1,
             alpha = .85) +
  geom_smooth(aes(trump_pct, dem_change, weight = Total_2016), 
              span = 2,
              se = F,
              col = "darkblue",
              alpha = .15) +
  geom_smooth(aes(trump_pct, dem_change), 
              span = 2,
              se = F,
              col = "lightblue",
              alpha = .15) +
  scale_color_manual(values = c("firebrick", "darkblue")) +
  scale_x_continuous(breaks = seq(-1, 1, .1),
                     name = "Trump",
                     labels = percent) +
  scale_y_continuous(breaks = seq(-1, 1, .05),
                     labels = percent,
                     # limits = c(0,.20),
                     name = "Dallet - Clinton") +
  theme_bw()  +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, colour = "black")

dem_growth2 <- ggplot(wi) +
  geom_point(aes(gop_change, dem_change, 
                 col = wi$dem_change > 0,
                 size = Total_2016), 
             shape = 1,
             alpha = .85) +
  geom_smooth(aes(gop_change, dem_change, weight = Total_2016), 
              col = "darkblue",
              se = F,
              alpha = .15) +
  geom_smooth(aes(gop_change, dem_change), 
              col = "lightblue",
              se = F,
              alpha = .15) +
  scale_color_manual(values = c("firebrick", "darkblue")) +
  scale_x_continuous(breaks = seq(-1, 1, .05),
                     name = "Trump - Romney",
                     labels = percent) +
  scale_y_continuous(breaks = seq(-1, 1, .05),
                     labels = percent,
                     name = "Dallet - Clinton") +
  theme_bw()  +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, colour = "black")

g <- gridExtra::arrangeGrob(dem_growth1, dem_growth2, ncol = 2)

ggsave("wisconsin-dem-swing-plot.jpg", g, width = 7, height = 4)
  


  
  
  
  







#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# get county level election turns for Wisconsin in the 2016 Presidential race and WI Supreme Court election of 2018
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load packages ====

pkgs <- c("readxl", "rvest", "tidyverse", "scales", "gridExtra", "splines")
lapply(pkgs, function(x) {
  if(!require(x, character.only = TRUE)) install.packages(x)
  library(x, character.only = TRUE)
})

# 2012 election results ====
url <- "http://elections.wi.gov/sites/default/files/County%20by%20County_11.6.12.xls"
download.file(url, destfile = "data/wisconsin/president2012.xls",
              method = "auto",
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

 # fix Lincoln 
SC$County[grep("ZLincoln", SC$County)] <- "Lincoln"

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

write_csv(wi, "data/wisconsin/wi-supreme-court-counties-2018.csv")

files <- paste0('data/wisconsin/president', c('2012.xls', '2016.xlsx'))
file.remove(files)

# plot ====
wi <- read_csv("data/wisconsin/wi-supreme-court-counties-2018.csv")

dem_growth1 <- ggplot(wi) +
  geom_point(aes(trump_pct, dem_change, 
                 col = wi$dem_change > 0,
                 size = Total_2016), 
             shape = 21,
             alpha = .85) 
dem_growth1 <- dem_growth1 +  
  # stat_smooth( # smooth using cubic natural spline as the basis function
  #             aes(trump_pct, dem_change, weight = Total_2016),
  #             method="lm", se=TRUE, fill=NA,
  #             formula=y ~ splines::ns(x, 3),colour="darkblue") +
  scale_color_manual(values = c("firebrick", "darkblue")) +
  scale_x_continuous(breaks = seq(-1, 1, .1),
                     name = "Trump 2016",
                     labels = percent) +
  scale_y_continuous(breaks = seq(-1, 1, .05),
                     labels = percent,
                     # limits = c(-.15, .18),
                     name = "Dallet 2018 minus Clinton 2016") +
  theme_bw()  +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, colour = "black")

dem_growth2 <- ggplot(wi) +
  geom_point(aes(gop_change, dem_change, 
                 col = wi$dem_change > 0,
                 size = Total_2016), 
             shape = 1,
             alpha = .85) 

dem_growth2 <- dem_growth2 + 
  stat_smooth( # smooth using cubic natural spline as the basis function
    aes(gop_change, dem_change, weight = Total_2016),
    method="lm", se=TRUE, fill=NA,
    formula=y ~ splines::ns(x, 3),colour="darkblue") +
  scale_color_manual(values = c("firebrick", "darkblue")) +
  scale_x_continuous(breaks = seq(-1, 1, .05),
                     name = "Trump 2016 minus Romney 2012",
                     labels = percent) +
  scale_y_continuous(breaks = seq(-1, 1, .05),
                     labels = percent,
                     limits = c(-.15, .18),
                     name = NULL) +
  theme_bw()  +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, colour = "black")

g <- gridExtra::arrangeGrob(dem_growth1, dem_growth2, ncol = 2)

ggsave("wisconsin-dem-swing-plot.jpg", g, width = 7, height = 4)
  


  
  
  
  



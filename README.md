# Gathering and visualizing local election results in Wisconsin and Alabama

Local election results for Wisconsin Supreme Court, Apr. 3 2018 and the Alabama special election for Senate (2017).

`Dallet-2018.R` and `alabama-senate-2017` are the source code for the figures in [Is Trump country abandoning the GOP?](https://connordonegan.github.io/portfolio/is-trump-country-abandoning-the-gop/)

The repo also includes a dataset of precinct-level results for the Wisconsin Supreme Court race. Raw precinct data provided by the State of Wisconsin on this race is in an extremely unfriendly format (see `wi-supreme-court-2018-wards-raw.csv`). Many wards were combined for this election, and thus results are reported by groups of wards (e.g. City of Adams in Adams County Wards 1-3a). We need a file that has a row for each ward. The file `clean-dallet-precinct-data.R` cleans the file and saves the results as a `.csv` file as well as simple features (`sf`) file. 

`wi-supreme-court-2018-wards-clean.csv` has the following columns: 

* `CNTY_NAME` county name,
* `NAME` ward name, 
* `STR_WARDS` ward number (may include letters and numbers),
*`CTV` city, town, or village indicator, 
* `Dallet` votes for Rebecca Dallet. For wards that were combined for this election, the total vote count was distributed evenly among each of the combined wards. This often results in fractions of votes being assigned and the numbers may be far from accurate. **This column was included only for the purpose of obtaining aggregate results for state legislative districts. Such aggregate counts are correct but ward-level counts may not be.** 
* `total` total vote count. **This data was handled the same was as `Dallet` and the same considerations and warnings apply.**
* `dallet_pct` vote share of the winning candidate, Rebecca Dallet. The aggregate vote shares of combined wards were simply assigned to all component wards.

The first four columns together define a unique ward. The script also merges the data with the `sf` file from the WI [Legislative Technology Services Bureau](https://www.arcgis.com/home/item.html?id=4743010a3c704cb0b52982e7ab4982ff#overview) which contains ward-level results from a plethora of state elections. 

For quick access to the WI precinct data, make sure you have installed the `sf` package and then run the following code in R:

```r
url <- 'https://github.com/ConnorDonegan/Dallet2018/raw/master/data/wisconsin/shape_zip.zip'
tmp <- tempfile()
download.file(url, tmp)
unzip(tmp, exdir = "wi-wards")
sp <- sf::st_read("wi-wards/data/wisconsin/sf_file/wi-wards-data.shp")
unlink(tmp)
```

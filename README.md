# Gathering and visualizing local election results in Wisconsin and Alabama

Local election results for Wisconsin Supreme Court, Apr. 3 2018 and the Alabama special election for Senate (2017).

This is the source code for the figures used in the blog post https://connordonegan.github.io/2018/04/04/is-trump-country-abandoning-the-gop/

The repo has been updated to include a dataset of precinct-level results for the Wisconsin Supreme Court race. Raw precinct data provided by the State of Wisconsin on this race is in an extremely unfriendly format. The file `clean-dallet-precinct-data.R` cleans the file and saves the results as a `.csv` file as well as simple features (`sf`) file. 

`wi-supreme-court-2018-wards-clean.csv` has the following columns: `CNTY_NAME` (county name), `NAME` (ward name), `STR_WARDS` (ward number, may include letters and numbers), `CTV` (city, town, or village indicator), and `dallet_pct` (vote share of the winning candidate, Rebecca Dallet). The first four columns together define a unique ward and are properly formatted for merging with the `sf` file from the WI [Legislative Technology Services Bureau](https://www.arcgis.com/home/item.html?id=4743010a3c704cb0b52982e7ab4982ff#overview).


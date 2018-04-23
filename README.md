# Gathering and visualizing local election results in Wisconsin and Alabama

Local election results for Wisconsin Supreme Court, Apr. 3 2018 and the Alabama special election for Senate (2017).

This is the source code for the figures used in the blog post https://connordonegan.github.io/2018/04/04/is-trump-country-abandoning-the-gop/

The repo has been updated to include a dataset of precinct-level results for the Wisconsin Supreme Court race. Raw precinct data provided by the State of Wisconsin on this race is in an extremely unfriendly format. The file `clean-dallet-precinct-data.R` cleans the file and saves the results as a .csv file as well as simple features (sf) file.


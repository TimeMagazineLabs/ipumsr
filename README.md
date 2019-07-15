# ipumsr

R scripts for reading extracts from IPUMS.org

## Background

IPUMS.org is a phenomenal resource from the generous folks at the [Minnesota Population Center](https://www.pop.umn.edu/) that, among many other things, allows one to extra *unit-level responses* from the U.S. Census at [IPUMS USA](https://www.pop.umn.edu/).

## Getting the Data

IPUMS has a [helpful User Guide](https://usa.ipums.org/usa/doc.shtml), but the interface is intuitive. In either order, one [selects which surveys to query](https://usa.ipums.org/usa-action/samples)--say, the American Community Survey 5yr samples from 2014-2017--and then [selects variables](https://usa.ipums.org/usa-action/variables/group) from the hundreds of options. It's useful to choose the samples first, because then the search results for variables will indicate whether they're available in the samples you need.

*Important*: When you're ready to submit your extract, select SPSS as the data format, which deliver the data as a `.sav` file, which R can easily import. This format contains both the data and metadata for the variables, so it saves on the agony of harmonizing a flat file with a codebook.

![Selecting SPSS](images/extract_screenshot.png)

You'll receive an email when you extract is ready. Download and unpack it somewhere that R can find it. It's good practice to also download the basic codebook for your extract, a small text file with a `.cbk` extension that contains all the information on your data request.

## Importing your data

The only dependency of `ipumsr` is `foreign`, which you need to install once (`install.packages('foreign`). This converts the SPSS `.sav` file to a data frame in R.

In your R script, you just need to import the library:

  source("ipumsr/ipums.R") #assuming you cloned the repo in the same directory
  
Then import your data:

  ipums_load("./data/usa_[EXTRACT NUMBER].sav")

Depending how much data you asked for, this can take several minutes. Time for a coffee break.

When the data is loaded, there's a good chance you'll get a warning message like this:

  1: In read.spss("./data/usa_00001.sav", to.data.frame = TRUE) :
    Duplicated levels in factor BPLD: Br. Virgin Islands, ns
  2: In read.spss("./data/usa_00001.sav", to.data.frame = TRUE) :
    Duplicated levels in factor YRNATUR: 1925 (1925 or earlier, ACS/PRCS pre 2012)

*Don't panic*. In the 181 extracts I've requested to date, this has never caused an actual problem.
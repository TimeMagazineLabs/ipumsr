source("ipums.R/ipums.R")
ipums <- read.spss("./data/usa_00180.sav", to.data.frame = TRUE)
data <- ipums
data$YEAR <- as.numeric(as.character(data$YEAR))

# When combined with YEAR, DATANUM, and SERIAL, PERNUM uniquely identifies each person within the IPUMS
# https://usa.ipums.org/usa-action/variables/PERNUM#description_section
# Only `YEAR` comes to us as a factor of these four variables
# print(paste(class(data$YEAR), class(data$DATANUM), class(data$SERIAL), class(data$PERNUM)))
# data$IPUMS_ID <- paste(as.character(data$YEAR), as.character(data$DATANUM), as.character(data$SERIAL), as.character(data$PERNUM), sep="_")
# data$IPUMS_LONG_ID <- paste(as.character(data$MULTYEAR), as.character(data$DATANUM), as.character(data$SERIAL), as.character(data$PERNUM), sep="_")

data <- ipums_convert_AGE(data)
data <- ipums_convert_factors(data)
data <- ipums_field_EDUC(data)

total_pop <- sum(ipums$PERWT)

rm(list=intersect(ls(), lsf.str()))
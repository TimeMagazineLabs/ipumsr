# some convenience functions for dealing with IPUMS imports in SPSS format

library(tidyverse)
library(foreign)
library(stringr)

ipums_DIR <- getwd();

# import the SPSS file from IPUMS and check for any problems with factors
# posed to StackOverflow here: http://stackoverflow.com/questions/40987639/how-to-diagnose-duplicated-levels-in-an-r-import
ipums_load <- function(filepath) {
  print("Loading IPUMS file. This may take a few minutes since it's a large extracts, but you only have to do it once.")
  ipums <- read.spss(filepath, to.data.frame = TRUE)
  print(paste("Loaded", NROW(ipums), "rows."));
  
  # loop through each column that's a factor to see if there are duplicates,
  # which produce the red "duplicated levels in factors are deprecated" warning
  # but typically don't present any problems
  
  for (name in names(ipums)) {
    type <- class(ipums[[name]]);
    if (type == "factor") {
      dups <- anyDuplicated(levels(ipums[[name]]))
      if (dups != 0) {
        print(paste("Duplicate factor in", name, "at index", dups))
        fac <- levels(ipums[[name]])
        culprit <- fac[dups]
        matches <- subset(ipums, ipums[[name]]==culprit)
        print(paste("The offending value is", culprit, "which shows up", NROW(matches), "times in the data."))
        if (NROW(matches) == 0) {
          print("Since that value never occurs, I wouldn't worry about this.");
        }
      }
    }
  }
  return (ipums);
}

ipums_field_AGE <- function(data) {
  #data$AGEN <- as.character(data[,"AGE"]);
  #data[!is.na(data$AGEN) & (data$AGEN == "Less than 1 year old" | data$AGEN == "Under 1 year"), "AGEN"] <- "0"

  values <- levels(data$AGE)
  
  # Descriptions of less than 1 is "0"
  values <- replace(values, values == "Less than 1 year old" | values == "Under 1 year", "0")
  
  # Descriptions of an age following by specification is just the year
  # E.g. "90 (90+ in 1980 and 1990)"
  values <- lapply(values, function(x) {
    if (grepl("^[0-9]+ ", x)) {
      x = str_split(x, " ")[[1]][[1]]
    }
    x = str_replace(x, "\\+", "")
    return(x)
  })
  
  valuesAsYears <- as.numeric(values)
  
  levels(data$AGE) <- valuesAsYears  
  
  data$AGE <- as.numeric(data$AGE)

  return(data)
}

ipums_field_AGE_COHORT <- function(data, targetYear, source="CENSUS") {
  if (!"YEAR_BORN" %in% colnames(data)) {
    return(data)
  }
  
  fpath = file.path(ipums_DIR, "variables", "age_cohorts.csv")
  canonical <- as.data.frame(read.csv(fpath, stringsAsFactors = F))
  canonical <- canonical[canonical$Source == source,]
  data$AGE_COHORT <- NA
  
  data$TARGET_AGE <- targetYear - data$YEAR_BORN

  for (i in 1:NROW(canonical)) {
    maxAge = canonical[i,"MAX"]
    cohort = canonical[i,"AGE_COHORT"]
    data[is.na(data$AGE_COHORT) & data$TARGET_AGE <= maxAge, "AGE_COHORT"] <- cohort
    print(paste("Added age cohort", cohort));
  }
  
  data <- subset(data, select = -TARGET_AGE )

  return(data)
}

# STATES
ipums_field_STATEFIP <- function(data) {
  if (!("STATEFIP" %in% colnames(data))) {
    print("Skipping `ipums_field_STATEFIP` since 'STATEFIP' isn't present.")  
    return(data);
  }
  
  print("Adding state FIPs values and abbreviations")
  
  # hand-crafted file that converts the state FIPs values to state names and abbreviations
  fpath = file.path(ipums_DIR, "variables", "states.csv")
  canonical <- as.data.frame(read.csv(fpath,
    colClasses=c("character","character","character","logical")
  ))
  
  data$STATE_NAME <- as.character(data$STATEFIP)
  data$STATE_ABBR <- NA
  data$STATE_FIPS <- NA
  data <- subset(data, select = -STATEFIP )
  
  convertField <- function(row) {
    fips <- row$FIPS
    abbr <- row$ABBR
    name <- row$NAME
    
    data$STATE_FIPS[data$STATE_NAME==name] <- fips
    data$STATE_ABBR[data$STATE_NAME==name] <- abbr
    return (data);
  }
  
  for (i in 1:NROW(canonical)) {
    data <- convertField(canonical[i,])
  }
  
  data$STATE_ABBR <- as.factor(data$STATE_ABBR)
  data$STATE_NAME <- as.factor(data$STATE_NAME)
  data$STATE_FIPS <- as.factor(data$STATE_FIPS)
  
  return(data);
}

ipums_field_COUNTY_FIPS <- function(data) {
  if (!("COUNTYFIP" %in% colnames(data))) {
    print("Skipping `ipums_field_county_FIPS` since 'COUNTYFIP' isn't present.");
    return(data);
  }
  
  if (!("STATE_FIPS" %in% colnames(data))) {
    print("converting states to FIPS")
    data <- ipums_field_STATEFIP(data);
  }    
  
  data$COUNTY_FIPS <- as.factor(paste0(data$STATE_FIPS, str_pad(data$COUNTYFIP, 3, pad="0")))

  return(data)
}

# add names of PUMAs
ipums_field_PUMA_CODE <- function(data) {
  if (!("PUMA" %in% colnames(data))) {
    print("Skipping `ipums_field_PUMA` since 'PUMA' isn't present.");
    return(data);
  }

  if (!("STATE_FIPS" %in% colnames(data))) {
    print("converting states to FIPS")
    data <- ipums_field_STATEFIP(data);
  }

  data$PUMA_CODE <- as.factor(paste0(data$STATE_FIPS, str_pad(data$PUMA, 5, pad="0")))

  return(data);
}

ipums_field_DENSITY <- function(data, popYear=2017) {
  data$PUMA_CODE <- as.character(data$PUMA_CODE)
  
  fpath = file.path(ipums_DIR, "variables", "pumas.csv")
  pumas <- read.csv(fpath, header=TRUE, colClasses = c(rep("character", 3), rep("numeric", 8)), stringsAsFactors = F)
  pumas <- pumas %>% filter(USPS != "PR")
  
  pumaPopulation <- data %>%
    filter(YEAR == popYear) %>%
    group_by(STATE_NAME, STATE_ABBR, STATE_FIPS, PUMA_CODE) %>%
    summarize(
      n = n(),
      POP = sum(PERWT),
      HH = sum(HHWT)
    )
  
  pumas <- merge(pumas, pumaPopulation, by="PUMA_CODE")
  pumas <- pumas[,c("PUMA_CODE", "STATE_ABBR", "PUMA_NAME", "POP", "HH", "ALAND_SQMI")]
  pumas$DENSITY_HH <- pumas$HH / pumas$ALAND_SQMI
  pumas$DENSITY_POP <- pumas$POP / pumas$ALAND_SQMI
  
  #pumas$URBAN_STATUS <- NA  
  #pumas[pumas$METRO=="In metropolitan area: In central/principal city", "URBAN_STATUS"] <- "Urban"
  #pumas[pumas$METRO=="In metropolitan area: Not in central/principal city", "URBAN_STATUS"] <- "Suburban"
  #pumas[pumas$METRO=="Not in metropolitan area", "URBAN_STATUS"] <- "Rural"
  #summary(pumas[pumas$URBAN_STATUS == "Urban", "DENSITY"])
  #summary(pumas[pumas$URBAN_STATUS == "Suburban", "DENSITY"])
  #summary(pumas[pumas$URBAN_STATUS == "Rural", "DENSITY"])

  # Imputed
  #pumas[is.na(pumas$URBAN_STATUS) & pumas$DENSITY > 7500, "URBAN_STATUS"] <- "Urban"
  #pumas[is.na(pumas$URBAN_STATUS) & pumas$DENSITY > 1000, "URBAN_STATUS"] <- "Suburban"
  #summary(pumas[is.na(pumas$URBAN_STATUS), "DENSITY"])
  #pumas[is.na(pumas$URBAN_STATUS), "URBAN_STATUS"] <- "Rural"
  
  data <- left_join(data, pumas[,c("PUMA_CODE", "DENSITY_HH", "DENSITY_POP")], by="PUMA_CODE")

  return(data);
}

# RACE AND ETHNICITY
ipums_field_RACE_ETHNICITY <- function(data) {
  if (!"RACE" %in% colnames(data) | !"HISPAN" %in% colnames(data)) {
    print("Skipping `ipums_field_RACE` since 'RACE' and 'HISPAN' aren't both present.")  
    return(data);
  }

  fpath = file.path(ipums_DIR, "variables", "race.csv")
  canonical <- as.data.frame(read.csv(fpath, stringsAsFactors = F))
  data$IS_HISPANIC <- F
  data$RACE_SIMPLIFIED <- ""
  data$RACE_ETHNICITY <- ""

  data[data$HISPAN != "Not Hispanic", "IS_HISPANIC"] <- T
  data$IS_HISPANIC <- as.logical(data$IS_HISPANIC)

  for (i in 1:NROW(canonical)) {
    originalRace = canonical[i,"RACE"]
    simplifiedRace = canonical[i,"RACE_SIMPLIFIED"]
    
    data[data$RACE_ETHNICITY == "" & data$RACE == originalRace, "RACE_SIMPLIFIED"] <- simplifiedRace
    #print(paste("Converted", originalRace, "to", simplifiedRace));
  }

  data$RACE_ETHNICITY <- ifelse(data$IS_HISPANIC == T, "Hispanic", paste0(data$RACE_SIMPLIFIED, ", not-Hispanic"))
  data$RACE_ETHNICITY <- as.factor(data$RACE_ETHNICITY)
  data$RACE_SIMPLIFIED <- as.factor(data$RACE_SIMPLIFIED)
  
  return(data);
}

# EDUCATION
ipums_field_EDUC <- function(data) {
  if (!("EDUC" %in% colnames(data))) {
    print("Skipping `ipums_field_EDUC` since 'EDUC' isn't present.")  
    return(data);
  }
  
  sourceKey = "EDUCD"

  if (!("EDUCD" %in% colnames(data))) {
    sourceKey = "EDUC"
  }

  # hand-crafted file that converts the many EDUCD values to more general categories
  fpath = file.path(ipums_DIR, "variables", "educd.csv")
  canonical <- as.data.frame(read.csv(fpath, stringsAsFactors = F))
  
  print(paste("Simplifying", sourceKey, "into EDUC_SIMPLIFIED and EDUC_HAS_DEGREE"));
  data$EDUC_SIMPLIFIED <- ""
  data$EDUC_HAS_DEGREE <- ""

  convertField <- function(row) {
    original <- as.character(row[1]);
    data[!is.na(data[[sourceKey]]) & data[[sourceKey]] == original, "EDUC_SIMPLIFIED"] <- as.character(row[2])      
    data[!is.na(data[[sourceKey]]) & data[[sourceKey]] == original, "EDUC_HAS_DEGREE"] <- as.character(row[3])      
    return (data);
  }
    
  for (i in 1:nrow(canonical)) {
    data <- convertField(canonical[i,])
  }
    
  data$EDUC_SIMPLIFIED <- as.factor(data$EDUC_SIMPLIFIED)
  data$EDUC_HAS_DEGREE <- as.logical(data$EDUC_HAS_DEGREE)

  data <- subset(data, select = -EDUC)
  if ("EDUCD" %in% colnames(data)) {
    data <- subset(data, select = -EDUCD)
  }
  
  return(data);
}

# YEARS IN HOUSE
ipums_field_MOVEDIN <- function(data) {
  if (!("MOVEDIN" %in% colnames(data))) {
    print("Skipping `ipums_field_MOVEDIN` since 'MOVEDIN' isn't present.")  
    return(data);
  }
  
  # hand-crafted file that converts the many EDUCD values to more general categories
  fpath = file.path(ipums_DIR, "variables", "movedin.csv")
  canonical <- as.data.frame(read.csv(fpath, stringsAsFactors = F))
  
  #keys = names(data)[grepl("^EDUCD(_[A-Z]+)?$", names(data))];
  
  print("Simplifying MOVEDIN into HOME_TENURE_MIN and HOME_TENURE_MAX");
  data$HOME_TENURE_MIN <- NA
  data$HOME_TENURE_MAX <- NA
  
  convertField <- function(row) {
    original <- as.character(row[1]);
    data[!is.na(data$MOVEDIN) & data$MOVEDIN == original, "HOME_TENURE_MIN"] <- as.numeric(row[3])      
    data[!is.na(data$MOVEDIN) & data$MOVEDIN == original, "HOME_TENURE_MAX"] <- as.numeric(row[4])      
    return (data);
  }
  
  for (i in 1:nrow(canonical)) {
    data <- convertField(canonical[i,])
  }
  
  #data$EDUC_SIMPLIFIED <- as.factor(data$EDUC_SIMPLIFIED)
  data <- subset(data, select = -MOVEDIN)
  
  return(data);
}



# Match occupation names to the OCCSOC variable
ipums_field_OCCSOC <- function(data) {
  if (!("OCCSOC" %in% colnames(data))) {
    print("Skipping `ipums_field_OCCSOC` since 'OCCSOC' isn't present.")  
    return(data);
  }
  
  print("Adding OCCSOC occupation names")
  
  # hand-crafted file that converts the OCCSOC codes to descriptions, including condensed categories
  fpath = file.path(ipums_DIR, "variables", "occsoc.csv")
  canonical <- as.data.frame(read.csv(fpath,
    colClasses=rep("character", 4)
  ))

  data$OCCSOC_TITLE <- ""
  
  convertField <- function(row) {
    title <- row$OCCSOC_TITLE
    occsoc <- row$OCCSOC
    data$OCCSOC_TITLE[data$OCCSOC==occsoc] <- title
    return (data);
  }
  
  for (i in 1:NROW(canonical)) {
    #print(paste(i, canonical[i,"OCCSOC_TITLE"]));
    data <- convertField(canonical[i,])
  }
  
  data$OCCSOC_TITLE <- as.factor(data$OCCSOC_TITLE)
  data <- subset(data, select = -OCCSOC)
  
  return(data)  
}


# BIRTHPLACE
ipums_field_BPL <- function(data, source="ACS") {
  if (!("BPL" %in% colnames(data))) {
    print("Skipping `ipums_field_BPL` since 'BPL' isn't present.")  
    return(data);
  }
  
  print("Adding BORN_US")
  
  # hand-crafted file that converts the OCCSOC codes to descriptions, including condensed categories
  fpath = file.path(ipums_DIR, "variables", "birthplace.csv")
  canonical <- as.data.frame(read.csv(fpath, stringsAsFactors = F))
  canonical <- canonical[grepl(source, canonical$Source),]
  
  convertField <- function(row) {
    location = row$BPL
    bornInUS = row$BORN_US
    data[data$BPL == location, "BORN_US"] <- bornInUS     
    return (data);
  }
  
  data$BORN_US <- NA

  for (i in 1:NROW(canonical)) {
    #print(paste(i, canonical[i,"BPL"]));
    data <- convertField(canonical[i,])
  }
  return(data)
}

# CITIZENSHIP
ipums_field_CITIZEN <- function(data, source="ACS") {
  if (!"CITIZEN" %in% colnames(data)) {
    print("Skipping `ipums_field_CITIZEN` since 'CITIZEN' isn't present.")  
    return(data);
  }
  
  print("Adding IS_CITIZEN")
  
  # hand-crafted file that converts the OCCSOC codes to descriptions, including condensed categories
  fpath = file.path(ipums_DIR, "variables", "citizen.csv")
  canonical <- as.data.frame(read.csv(fpath, stringsAsFactors = F))
  canonical <- canonical[grepl(source, canonical$Source),]
  
  convertField <- function(row) {
    status = row$CITIZEN
    isCitizen = row$IS_CITIZEN
    data[data$CITIZEN == status, "IS_CITIZEN"] <- isCitizen     
    return (data);
  }
  
  data$IS_CITIZEN <- NA
  
  for (i in 1:NROW(canonical)) {
    #print(paste(i, canonical[i,"CITIZEN"]));
    data <- convertField(canonical[i,])
  }
  return(data)
}

# Whether a person is eligible to vote
ipums_field_VOTING_ELIGIBLE <- function(data, electionYear, includeBirthQuarter = F) {
  if ("YEAR_BORN" %in% colnames(data)) {
    data$BIRTHYR <- data$YEAR_BORN
  }
  
  if (!"AGE" %in% colnames(data) & !"BIRTHYR" %in% colnames(data)) {
    print("Skipping `ipums_field_VOTING_ELIGIBLE` since neither 'AGE' or 'BIRTHYR' are present. ('BIRTHYR' is preferred).")  
    return(data);
  }
  
  if (!"CITIZEN" %in% colnames(data)) {
    print("Skipping `ipums_field_VOTING_ELIGIBLE` since 'CITIZEN' isn't present.")  
    return(data);
  }
  
  if (!"BIRTHYR" %in% colnames(data)) {
    print("Computing birth year from AGE");
    if (!"AGEN" %in% colnames(data)) { # if `ipums_convert_AGE` wasn't run
      data <- ipums_convert_AGE(data);
    }
    data$BIRTHYR <- data$YEAR - data$AGEN;
  }
  
  if (!"IS_CITIZEN" %in% colnames(data)) {
    print("Running `ipums_field_CITIZEN`")
    data <- ipums_field_CITIZEN(data);
  }
  
  minimumYear <- electionYear - 18;
  
  keyName = paste0("CAN_VOTE_", electionYear)
  
  # We'll subtract those who can't vote
  data[[keyName]] <- T;
  data[[keyName]][!data$IS_CITIZEN] <- F
  data[[keyName]][data$BIRTHYR > minimumYear] <- F

  # This is the closest we can get to eliminating those who turn 18 after Election Day
  if (includeBirthQuarter) {
    print(paste0("Eliminating late ", minimumYear, " babies."))
    data[[keyName]][data$BIRTHYR == minimumYear & data$BIRTHQTR == "Oct-Nov-Dec"] <- F
  }
  
  # While institutionalized populations can vote in some cases, this appears to be rare
  data[[keyName]][data$GQ == "Group quarters--Institutions"] <- F

  if ("YEAR_BORN" %in% colnames(data)) {
    data <- subset(data, select = -BIRTHYR )
  }
  
  return(data)
}

# Whether a person was eligible to vote in the year surveyed
ipums_field_VOTING_ELIGIBLE_IN_YEAR <- function(data) {
  if ("YEAR_BORN" %in% colnames(data)) {
    data$BIRTHYR <- data$YEAR_BORN
  }
  
  if (!"AGE" %in% colnames(data) & !"BIRTHYR" %in% colnames(data)) {
    print("Skipping `ipums_field_VOTING_ELIGIBLE` since neither 'AGE' or 'BIRTHYR' are present. ('BIRTHYR' is preferred).")  
    return(data);
  }
  
  if (!"CITIZEN" %in% colnames(data)) {
    print("Skipping `ipums_field_VOTING_ELIGIBLE` since 'CITIZEN' isn't present.")  
    return(data);
  }
  
  if (!"BIRTHYR" %in% colnames(data)) {
    print("Computing birth year from AGE");
    if (!"AGEN" %in% colnames(data)) { # if `ipums_convert_AGE` wasn't run
      data <- ipums_convert_AGE(data);
    }
    data$BIRTHYR <- data$YEAR - data$AGEN;
  }
  
  if (!"IS_CITIZEN" %in% colnames(data)) {
    print("Running `ipums_field_CITIZEN`")
    data <- ipums_field_CITIZEN(data);
  }
  
  #minimumYear <- electionYear - 18;
  
  keyName = "VOTING_ELIGIBLE"
  
  # We'll subtract those who can't vote
  data[[keyName]] <- T;
  data[[keyName]][!data$IS_CITIZEN] <- F
  data[[keyName]][data$BIRTHYR > (data$YEAR - 18)] <- F

  # While institutionalized populations can vote in some cases, this appears to be rare
  data[[keyName]][data$GQ == "Group quarters--Institutions"] <- F
  
  if ("YEAR_BORN" %in% colnames(data)) {
    data <- subset(data, select = -BIRTHYR )
  }
  
  return(data)
}

ipums_field_YEAR_NATURALIZED <- function(data) {
  if (!"YRNATUR" %in% colnames(data)) {
    print("Skipping `ipums_field_YEAR_NATURALIZED` since 'YRNATUR' isn't present.")  
    return(data);
  }
  
  values <- levels(data$YRNATUR)
  valuesAsYears <- lapply(values, function(x) {
    isYr = grepl("^[0-9]{4}", x)
    return(ifelse(isYr, substr(x, 1, 4), NA))
  })
  valuesAsYears <- as.numeric(valuesAsYears)
  
  levels(data$YRNATUR) <- valuesAsYears
  data$YRNATUR <- as.numeric(data$YRNATUR)
  return(data)
}

# convert a factors to their correct types. NOT RECOMMENDED
ipums_convert_factors <- function(ipums) {
  ipums$YEAR <- as.numeric(ipums$YEAR)
  
  print("de-factorizing remainder of factored columns into characters")
  types <- lapply(ipums, class)
  factor_columns <- names(types[types=="factor"])
  
  print("Converting factors to the appropriate types")
  for (column in factor_columns) {
    print(paste("Converting", column))
    ipums[[column]] <- as.character(ipums[[column]])
  }
  
  return(ipums);
}


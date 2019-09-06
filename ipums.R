# some convenience functions for dealing with IPUMS imports in SPSS format

#install.packages("foreign");
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
  data$AGEN <- as.character(data[,"AGE"]);
  data[!is.na(data$AGEN) & data$AGEN == "Less than 1 year old", "AGEN"] <- "0"
  data[!is.na(data$AGEN) & data$AGEN == "90 (90+ in 1980 and 1990)", "AGEN"] <- "90"
  data[!is.na(data$AGEN) & data$AGEN == "100 (100+ in 1960-1970)", "AGEN"] <- "100"
  data[!is.na(data$AGEN) & data$AGEN == "112 (112+ in the 1980 internal data)", "AGEN"] <- "112"
  data[!is.na(data$AGEN) & data$AGEN == "115 (115+ in the 1990 internal data) ", "AGEN"] <- "115"

  data$AGE <- as.numeric(data$AGEN)
  data <- subset(data, select=-AGEN)

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
ipums_field_PUMA <- function(data) {
  if (!("PUMA" %in% colnames(data))) {
    print("Skipping `ipums_field_PUMA` since 'PUMA' isn't present.");
    return(data);
  }

  if (!("STATE_FIPS" %in% colnames(data))) {
    print("converting states to FIPS")
    data <- ipums_field_STATEFIP(data);
  }

  data$PUMA_CODE <- as.factor(paste(data$STATE_FIPS, str_pad(data$PUMA, 5, pad="0")))

  return(data);
  
  #fpath = file.path(ipums_DIR, "variables", "pumas.csv")
  #canonical <- read.csv(fpath, header=TRUE, colClasses=c(rep("character", 3), rep("integer", 2), rep("numeric", 6)))
  
  #puma_list$PUMA_DENSITY <- puma_list$POP10 / puma_list$ALAND_SQMI
  #canonical <- setNames(canonical[,c("PUMA_CODE", "PUMA_NAME", "ALAND_SQMI")], c("PUMA_CODE", "PUMA_NAME", "PUMA_SQMI"))
  
  #convertField <- function(row) {
    #code = row$PUMA_CODE
    #return (data);
  #}
  
  #for (i in 1:NROW(canonical)) {
    #print(paste(i, canonical[i,"BPL"]));
    #data <- convertField(canonical[i,])
  #}  
  
  
  #data <- merge(data, puma_names, by="PUMA_CODE")
    
  #return(data);
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
  if (!("EDUCD" %in% colnames(data))) {
    print("Skipping `ipums_field_EDUC` since 'EDUCD' isn't present.")  
    return(data);
  }

  # hand-crafted file that converts the many EDUCD values to more general categories
  fpath = file.path(ipums_DIR, "variables", "educd.csv")
  canonical <- as.data.frame(read.csv(fpath, stringsAsFactors = F))
  
  #keys = names(data)[grepl("^EDUCD(_[A-Z]+)?$", names(data))];
  
  print("Simplifying EDUCD into EDUC_SIMPLIFIED and EDUC_HAS_DEGREE");
  data$EDUC_SIMPLIFIED <- ""
  data$EDUC_HAS_DEGREE <- ""

  convertField <- function(row) {
    original <- as.character(row[1]);
    data[!is.na(data$EDUCD) & data$EDUCD == original, "EDUC_SIMPLIFIED"] <- as.character(row[2])      
    data[!is.na(data$EDUCD) & data$EDUCD == original, "EDUC_HAS_DEGREE"] <- as.character(row[3])      
    return (data);
  }
    
  for (i in 1:nrow(canonical)) {
    data <- convertField(canonical[i,])
  }
    
  data$EDUC_SIMPLIFIED <- as.factor(data$EDUC_SIMPLIFIED)
  data$EDUC_HAS_DEGREE <- as.logical(data$EDUC_HAS_DEGREE)

  data <- subset(data, select = -c(EDUC, EDUCD ))
  
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
    print(paste(i, canonical[i,"OCCSOC_TITLE"]));
    data <- convertField(canonical[i,])
  }
  
  data$OCCSOC_TITLE <- as.factor(data$OCCSOC_TITLE)
  data <- subset(data, select = -OCCSOC)
  
  return(data)  
}


# BIRTHPLACE
ipums_field_BPL <- function(data) {
  if (!("BPL" %in% colnames(data))) {
    print("Skipping `ipums_field_BPL` since 'BPL' isn't present.")  
    return(data);
  }
  
  print("Adding BORN_US")
  
  # hand-crafted file that converts the OCCSOC codes to descriptions, including condensed categories
  fpath = file.path(ipums_DIR, "variables", "birthplace.csv")
  canonical <- as.data.frame(read.csv(fpath, stringsAsFactors = F))

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
ipums_field_CITIZEN <- function(data) {
  if (!"CITIZEN" %in% colnames(data)) {
    print("Skipping `ipums_field_CITIZEN` since 'CITIZEN' isn't present.")  
    return(data);
  }
  
  print("Adding IS_CITIZEN")
  
  # hand-crafted file that converts the OCCSOC codes to descriptions, including condensed categories
  fpath = file.path(ipums_DIR, "variables", "citizen.csv")
  canonical <- as.data.frame(read.csv(fpath, stringsAsFactors = F))
  
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
  
  # We'll subtract those who can't vote
  data$CAN_VOTE <- T;
  data$CAN_VOTE[!data$IS_CITIZEN] <- F
  data$CAN_VOTE[data$BIRTHYR > minimumYear] <- F

  # This is the closest we can get to eliminating those who turn 18 after Election Day
  if (includeBirthQuarter) {
    print(paste0("Eliminating late ", minimumYear, " babies."))
    data$CAN_VOTE[data$BIRTHYR == minimumYear & data$BIRTHQTR == "Oct-Nov-Dec"] <- F
  }
  
  # While institutionalized populations can vote in some cases, this appears to be rare
  data$CAN_VOTE[data$GQ == "Group quarters--Institutions"] <- F

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
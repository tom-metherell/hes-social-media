#######
# Function: filter_psychiatric
# Input: dataframe containing d_diagXX variables
# Method: filters out rows where none of the d_diagXX variables have values F00-F69, F90-F99 or X60-X84
# Output: filtered dataframe
#######
filter_psychiatric <- function(data){
  data %>% filter(if_any(contains("d_diag"), ~str_detect(., "F(?!7[0-9]|8[0-9])|X(?:6[0-9]|7[0-9]|8[0-4])")))
}

#######
# Function: regional_summary
# Input: dataframe containing region of treatment variable
# Method: produces a table of the number of relevant episodes by region of treatment
# Output: summary table
#######
regional_summary <- function(data){
  data %>% summarise(number = n(), .by = rotreat) %>% mutate(region = as_factor(rotreat), .before = number) %>% select(-rotreat)
}

#######
# Function: derive_interview_dates
# Input: dataframes containing interview date information from ages 11, 14 (survey and accelerometry) and 17 sweeps
# Method: converts each date into a single string and uses age 14 sweep accelerometry date if available, otherwise survey date
# Output: dataframe with one interview date per sweep and per MCSID
#######
derive_interview_dates <- function(mcs5_data, mcs6_data, mcs6_accelerometry_data, mcs7_data){
  # For age 11 sweep date information
  mcs5_data <- mcs5_data %>%
    # Convert day/month/year variables to a single date string and remove the former
    mutate(mcs5_date = as.Date(paste0("01/", EINTM0000, "/", EINTY0000), format = "%d/%m/%Y")) %>% 
    select(MCSID, mcs5_date) %>%
    # Keep only the latest date within a household
    group_by(MCSID) %>%
    mutate(mcs5_date = max(mcs5_date)) %>%
    distinct()
  # Repeat for age 14 sweep (survey and accelerometry data)
  mcs6_data <- mcs6_data %>% 
    mutate(mcs6_date = as.Date(paste0("01/", FHINTM00, "/", FHINTY00), format = "%d/%m/%Y")) %>% 
    select(MCSID, mcs6_date) %>%
    group_by(MCSID) %>%
    mutate(mcs6_date = max(mcs6_date)) %>%
    distinct()
  mcs6_accelerometry_data <- mcs6_accelerometry_data %>%
    mutate(mcs6_adate = as.Date(paste0("01/", FCACCMONTH, "/", FCACCYEAR), format = "%d/%m/%Y")) %>%
    select(MCSID, mcs6_adate) %>%
    group_by(MCSID) %>%
    mutate(mcs6_adate = max(mcs6_adate)) %>%
    distinct()
  # If date of accelerometry (and by extension, time use diary) is available, use that, otherwise use survey date
  mcs6_data <- full_join(mcs6_data, mcs6_accelerometry_data, by = "MCSID") %>%
    mutate(mcs6_date = case_when(
      !is.na(mcs6_adate) ~ mcs6_adate,
      TRUE ~ mcs6_date
    )) %>%
    select(MCSID, mcs6_date)
  # Repeat for age 17 sweep
  mcs7_data <- mcs7_data %>%
    mutate(mcs7_date = as.Date(paste0("01/", GHINTM00, "/", GHINTY00), format = "%d/%m/%Y")) %>%
    select(MCSID, mcs7_date) %>%
    group_by(MCSID) %>%
    mutate(mcs7_date = max(mcs7_date)) %>%
    distinct() %>%
    select(MCSID, mcs7_date)
  
  return(reduce(list(mcs5_data, mcs6_data, mcs7_data), full_join, by = "MCSID"))
}

#######
# Function: derive_dates
# Input: `data`, a HES dataset; `type`, a string of either "APC" or "OP" denoting which HES dataset this is and `interview_dates`, the data frame of interview dates for each sweep
# Method: derives the earliest relevant episode within each study period for each participant
# Output: a dataframe detailing participants' earliest recorded psychiatric diagnoses in each study period
#######
derive_dates <- function(data, type, interview_dates){
  if(type == "APC"){
    data %>% 
      # Harmonise ID variable names
      rename(MCSID = mcsid, CNUM = cnum, APC_date = admidate) %>% 
      # Join to interview dates and keeping only relevant variables
      left_join(interview_dates, by = "MCSID") %>% 
      select(MCSID, CNUM, APC_date, mcs5_date, mcs6_date, mcs7_date) %>% 
      # Derive the date of the earliest episode with a psychiatric diagnosis in the dataset in each study period and remove unneeded variables
      group_by(MCSID, CNUM) %>% 
      mutate(APC_date1 = tryCatch(min(APC_date[APC_date >= (if(!any(is.na(mcs5_date))){mcs5_date %m+% months(1)} else {"2013-03-01"}) & APC_date < (if(!any(is.na(mcs6_date))){mcs6_date} else {"2015-01-01"})]), error = function(cond) NA),
             APC_date2 = tryCatch(min(APC_date[APC_date >= (if(!any(is.na(mcs6_date))){mcs6_date %m+% months(1)} else {"2016-05-01"}) & APC_date < (if(!any(is.na(mcs7_date))){mcs7_date} else {"2018-01-01"})]), error = function(cond) NA)) %>%
      select(MCSID, CNUM, APC_date1, APC_date2) %>%
      distinct()
  } else if(type == "OP"){
    # As above
    data %>% 
      rename(MCSID = mcsid, CNUM = cnum, OP_date = apptdate) %>%
      left_join(interview_dates, by = "MCSID") %>%
      select(MCSID, CNUM, OP_date, mcs5_date, mcs6_date, mcs7_date) %>% 
      group_by(MCSID, CNUM) %>% 
      mutate(OP_date1 = tryCatch(min(OP_date[OP_date >= (if(!any(is.na(mcs5_date))){mcs5_date %m+% months(1)} else {"2013-03-01"}) & OP_date < (if(!any(is.na(mcs6_date))){mcs6_date} else {"2015-01-01"})]), error = function(cond) NA),
             OP_date2 = tryCatch(min(OP_date[OP_date >= (if(!any(is.na(mcs6_date))){mcs6_date %m+% months(1)} else {"2016-05-01"}) & OP_date < (if(!any(is.na(mcs7_date))){mcs7_date} else {"2018-01-01"})]), error = function(cond) NA)) %>%
      select(MCSID, CNUM, OP_date1, OP_date2) %>%
      distinct()
  }
}

#######
# Function: clean_tud
# Input: time use diary dataset
# Method: derives average social media use in minutes per day across the diary period (rejecting participants who did only 1 day)
# Output: a dataframe containing average social media use times for each participant
#######
clean_tud <- function(data){
  data <- data %>% 
    # Reject participants with 1 day of data (144 * 10 minutes) instead of 2 (288 * 10 minutes)
    group_by(MCSID, CNUM) %>% 
    mutate(total_slots = n()) %>%
    filter(total_slots == 288) %>%
    # Calculate average time per day on social media (number of slots * 5 minutes, as the diary period is 2 days) and remove unneeded variables
    mutate(sm_slots = sum(FCTUDACT == 34),
           mcs6_sm_per_day = 5 * sm_slots) %>%
    select(MCSID, CNUM, mcs6_sm_per_day) %>%
    distinct()
}

#######
# Function: clean_parent
# Input: parent and proxy partner interview, derived and cognitive assessment datasets from all sweeps
# Method: defines missing values, calculates derived variables, recodes and merges variables across parents according to the procedure outlined in the protocol
# Output: a cleaned parental dataset
#######
clean_parent <- function(mcs1_parent_interview, mcs2_parent_interview, mcs3_parent_interview, mcs4_parent_derived, mcs4_parent_interview, mcs4_proxy_partner_interview, mcs5_parent_derived, mcs5_parent_interview, mcs5_proxy_partner_interview, mcs6_parent_assessment, mcs6_parent_interview, mcs7_parent_interview){
  ### Define missing values
  ## Age 9 months sweep
  # All variables: negative values
  is.na(mcs1_parent_interview) <- mcs1_parent_interview < 0
  
  # Age of leaving full-time education: 0 (still in full-time education)
  is.na(mcs1_parent_interview$APLFTE00) <- mcs1_parent_interview$APLFTE00 == 0
  
  ## Age 3 sweep
  # All variables: negative values
  is.na(mcs2_parent_interview) <- mcs2_parent_interview < 0
  
  ## Age 5 sweep
  # All variables: negative values
  is.na(mcs3_parent_interview) <- mcs3_parent_interview < 0
  
  ## Age 7 sweep
  # All derived variables: negative values
  is.na(mcs4_parent_derived) <- mcs4_parent_derived < 0
  
  # All interview variables: negative values
  is.na(mcs4_parent_interview) <- mcs4_parent_interview < 0
  
  # Partner ever used force: 3 (don't want to answer)
  is.na(mcs4_parent_interview$DPFORC00) <- mcs4_parent_interview$DPFORC00 == 3
  
  # All proxy partner interview variables: negative values
  is.na(mcs4_proxy_partner_interview) <- mcs4_proxy_partner_interview < 0
  
  ## Age 11 sweep
  # All derived variables: negative values
  is.na(mcs5_parent_derived) <- mcs5_parent_derived < 0
  
  # All interview variables: negative values
  is.na(mcs5_parent_interview) <- mcs5_parent_interview < 0
  
  # Number of employees at parent's place of work: 10~14 (don't know/no answer/refused)
  is.na(mcs5_parent_interview$EPEMPN00) <- mcs5_parent_interview$EPEMPN00 > 9
  
  # Longstanding illness: 4~5 (don't know/refused)
  is.na(mcs5_parent_interview$EPLOLM00) <- mcs5_parent_interview$EPLOLM00 > 3
  
  # Partner ever used force: 3 (don't know/don't wish to answer)
  is.na(mcs5_parent_interview$EPFORC00) <- mcs5_parent_interview$EPFORC00 == 3
  
  # 2010 General Election voting status: 3~4 (refused/don't know)
  is.na(mcs5_parent_interview$EPVOTE00) <- mcs5_parent_interview$EPVOTE00 > 2
    
  # Kessler items: 6 (don't know/don't wish to answer)
  is.na(mcs5_parent_interview[, c("EPPHDE00", "EPPHHO00", "EPPHRF00", "EPPHEE00", "EPPHWO00", "EPPHNE00")]) <- mcs5_parent_interview[, c("EPPHDE00", "EPPHHO00", "EPPHRF00", "EPPHEE00", "EPPHWO00", "EPPHNE00")] == 6
  
  # All proxy partner interview variables: negative values
  is.na(mcs5_proxy_partner_interview) <- mcs5_proxy_partner_interview < 0
  
  ## Age 14 sweep
  # All cognitive assessment variables: negative values
  is.na(mcs6_parent_assessment) <- mcs6_parent_assessment < 0
  
  # All interview variables: negative values
  is.na(mcs6_parent_interview) <- mcs6_parent_interview < 0
  
  # Number of employees at parent's place of work: 10~12 (don't know/none)
  is.na(mcs6_parent_interview$FPEMPN00) <- mcs6_parent_interview$FPEMPN00 > 9

  ## Age 17 sweep
  # Housing tenure: 10~13 (other/don't know/refused/no answer)
  is.na(mcs7_parent_interview$GPROOW00) <- mcs7_parent_interview$GPROOW00 > 9
  
  ### Calculating derived variables
  ## Latest age at which parents left full-time education
  # Bind ages recorded at each sweep together and remove irrelevant variables
  left_education <- bind_rows(mcs1_parent_interview, mcs2_parent_interview, mcs3_parent_interview, mcs4_proxy_partner_interview, mcs5_proxy_partner_interview) %>%
    select(MCSID, contains("PLFTE00"), DXEMPA1G, EXPXEL00) %>%
    
    # Derive maximum age for each household at each sweep
    group_by(MCSID) %>%
    mutate_at(c("APLFTE00", "BPLFTE00", "CPLFTE00", "DXEMPA1G", "EXPXEL00"), ~if(sum(!is.na(.)) == 0){NA} else {max(., na.rm = TRUE)}) %>%
    distinct() %>%
    
    # Derive maximum age for each household by age 7 and 11 sweeps separately and remove unneeded variables
    mutate(
      mcs4_left_education = if(
        sum(!is.na(across(c("APLFTE00", "BPLFTE00", "CPLFTE00", "DXEMPA1G")))) == 0){
          NA
        } else {
          max(across(c("APLFTE00", "BPLFTE00", "CPLFTE00", "DXEMPA1G")), na.rm = TRUE)
        },
      mcs5_left_education = if(
        sum(!is.na(across(c("APLFTE00", "BPLFTE00", "CPLFTE00", "DXEMPA1G", "EXPXEL00")))) == 0){
          NA
        } else {
          max(across(c("APLFTE00", "BPLFTE00", "CPLFTE00", "DXEMPA1G", "EXPXEL00")), na.rm = TRUE)
        }
    ) %>%
    select(MCSID, mcs4_left_education, mcs5_left_education)
  
  # Age 9 months, consent to self-completion: recode to binary 0/1 did not consent/consented
  mcs1_parent_interview <- mcs1_parent_interview %>%
    mutate(APSCAC00 = case_match(APSCAC00,
      2 ~ 1,
      3 ~ 0,
      .default = APSCAC00
    )) %>%
    
    # Sum across parents
    group_by(MCSID) %>%
    mutate(mcs1_consents = sum(APSCAC00, na.rm = TRUE)) %>%
    select(MCSID, mcs1_consents) %>%
    distinct()
  
  # Age 7, parental Kessler and social class: find highest across parents
  mcs4_parent_derived <- mcs4_parent_derived %>%
    group_by(MCSID) %>%
    mutate(DDKESSLER = if(sum(!is.na(DDKESSLER)) == 0){NA} else {max(DDKESSLER, na.rm = TRUE)},
           DDD05S00 = if(sum(!is.na(DDD05S00)) == 0){NA} else {min(DDD05S00, na.rm = TRUE)}) %>% # Minimum value -> higher social class
    distinct()
  
  # Age 7, longstanding illness and partner ever used force: if either parent reported return 1, otherwise 2
  mcs4_parent_interview <- mcs4_parent_interview %>%
    group_by(MCSID) %>%
    mutate_at(c("DPLOLM00", "DPFORC00"), ~if(sum(!is.na(.)) == 0){NA} else {min(., na.rm = TRUE)}) %>%
    distinct()
  
  # Age 11, social class: find highest across parents
  mcs5_parent_derived <- mcs5_parent_derived %>%
    group_by(MCSID) %>%
    mutate(ED05S00 = if(sum(!is.na(ED05S00)) == 0){NA} else {min(ED05S00, na.rm = TRUE)}) %>% # Minimum value -> higher social class
    distinct()
  
  # Age 11, other interview variables:
  mcs5_parent_interview <- mcs5_parent_interview %>%
    group_by(MCSID) %>%
    mutate(
      # Kessler: sumscore individual items and find maximum across parents 
      mcs5_kessler = 30 - (EPPHDE00 + EPPHHO00 + EPPHRF00 + EPPHEE00 + EPPHWO00 + EPPHNE00),
      mcs5_kessler = if(any(!is.na(mcs5_kessler))){max(mcs5_kessler, na.rm = TRUE)} else {NA},
      
      # Number of employees at parent's place of work: primary respondent only
      EPEMPN00 = if(length(EPEMPN00[EELIG00 == 1]) == 1){EPEMPN00[EELIG00 == 1]} else {NA},
      
      # Wealth: derive from house value, minus mortgage amount, plus assets/investments
      mcs5_wealth = EPHVAL00 - EPMOPA00 + EPINVT00,
      
      # Partner ever used force: if either parent reported return 1, otherwise 2
      EPFORC00 = if(sum(!is.na(EPFORC00)) == 0){NA} else {min(EPFORC00, na.rm = TRUE)},
      
      # Longstanding illness: if either parent reported any impairment return 1, otherwise 0
      EPLOLM00 = case_when(
        any(EPLOLM00 %in% c(1, 2)) ~ 1,
        any(!is.na(EPLOLM00)) & all(is.na(EPLOLM00) | EPLOLM00 == 3) ~ 0,
        TRUE ~ NA
      ),
        
      # 2010 General Election voting status: if either parent voted return 0, otherwise 1
      EPVOTE00 = case_when(
        any(!is.na(EPVOTE00)) & any(EPVOTE00 == 1) ~ 0,
        any(!is.na(EPVOTE00)) & all(is.na(EPVOTE00) | EPVOTE00 == 2) ~ 1,
        TRUE ~ NA
      )
    ) %>%
    
    # Fill missing wealth values with values from the same household and remove unneeded variables
    fill(mcs5_wealth, .direction = "downup") %>%
    select(-EPPHDE00, -EPPHHO00, -EPPHRF00, -EPPHEE00, -EPPHWO00, -EPPHNE00, -EELIG00, -EPHVAL00, -EPMOPA00, -EPINVT00) %>%
    distinct()
  
  # Age 14, cognitive assessment: keep rows referring to primary respondent only
  mcs6_parent_assessment <- mcs6_parent_assessment %>%
    filter(FRESP00 == 1) %>%
    select(-FRESP00)
  
  # Age 14, parent interview variables:
  mcs6_parent_interview <- mcs6_parent_interview %>%
    group_by(MCSID) %>%
    mutate(
      # Number of employees at parent's place of work: primary respondent only
      FPEMPN00 = if(length(FPEMPN00[FELIG00 == 1]) == 1){FPEMPN00[FELIG00 == 1]} else {NA},
      
      # Household moves: if parents disagree then return missing
      FHADSA00 = if(length(unique(FHADSA00[!is.na(FHADSA00)])) == 1){unique(FHADSA00[!is.na(FHADSA00)])} else {NA},
      
      # Number of rooms: if parents disagree then return missing
      FPROMA00 = if(length(unique(FPROMA00[!is.na(FPROMA00)])) == 1){unique(FPROMA00[!is.na(FPROMA00)])} else {NA}
    ) %>%
    
    # Remove unneeded variable
    select(-FELIG00) %>%
    distinct()
  
  # Age 17, housing tenure: if parents disagree return NA
  mcs7_parent_interview <- mcs7_parent_interview %>%
    group_by(MCSID) %>%
    mutate(GPROOW00 = if(length(unique(GPROOW00[!is.na(GPROOW00)])) == 1){unique(GPROOW00[!is.na(GPROOW00)])} else {NA}) %>%
    distinct()
  
  return(reduce(list(mcs1_parent_interview, mcs4_parent_derived, mcs4_parent_interview, mcs5_parent_derived, mcs5_parent_interview, mcs6_parent_assessment, mcs6_parent_interview, mcs7_parent_interview, left_education), full_join, by = "MCSID"))
}

#######
# Function: clean_parent_cm
# Input: parental interview about cohort member datasets from ages 7 and 11 sweeps
# Method: defines missing values and merges variables across parents according to the procedure outlined in the protocol
# Output: a cleaned parental interview about cohort member dataset
#######
clean_parent_cm <- function(mcs4_parent_cm_interview, mcs5_parent_cm_interview){
  ### Define missing values
  ## Age 7 sweep
  # All variables: negative values
  is.na(mcs4_parent_cm_interview) <- mcs4_parent_cm_interview < 0
  
  # How close to child: 5 (don't want to answer)
  is.na(mcs4_parent_cm_interview$DPSCHC00) <- mcs4_parent_cm_interview$DPSCHC00 == 5
  
  mcs4_parent_cm_interview <- mcs4_parent_cm_interview %>%
    group_by(MCSID, CNUM) %>%
    
    # ADHD/autism/longstanding illness: if parents disagree return NA
    mutate_at(c("DPADHD00", "DPAUTS00", "DPCLSI00"), ~if(length(unique(.[!is.na(.)])) != 1){NA} else {unique(.[!is.na(.)])}) %>%
    
    # How close to child/social interaction: mean of parents' responses
    mutate_at(c("DPSCHC00", "DPVIFR00"), ~if(sum(!is.na(.)) == 0){NA} else {mean(.[!is.na(.)])}) %>%
    
    # Regular bedtime: if both parents say "always" return 1, otherwise 0
    mutate(DPBERE00 = case_when(
      any(DPBERE00 %in% c(1, 2, 3)) ~ 0,
      all(DPBERE00[!is.na(DPBERE00)] == 4) ~ 1,
      TRUE ~ NA
    )) %>%
    
    # Educational achievement/bullying: if either parent reports any difficulty/bullying return 1, otherwise 0
    mutate_at(c("DPAMTH00", "DPARED00", "DPAWRT00", "DPBULS00"), ~case_when(
      any(. %in% c(2, 3, 4)) ~ 1,
      all(.[!is.na(.)] == 1) ~ 0,
      TRUE ~ NA
    )) %>%
    distinct()
  
  ## Age 11 sweep
  # All variables: negative values
  is.na(mcs5_parent_cm_interview) <- mcs5_parent_cm_interview < 0
  
  # Regular bedtime: 5~6 (don't know/refused)
  is.na(mcs5_parent_cm_interview$EPBERE00) <- mcs5_parent_cm_interview$EPBERE00 > 4
  
  # Physical activity/social interaction: 8~9 (don't know/refused)
  is.na(mcs5_parent_cm_interview[, c("EPSEHO00", "EPPLFR00", "EPVIFR00")]) <- mcs5_parent_cm_interview[, c("EPSEHO00", "EPPLFR00", "EPVIFR00")] > 7
  
  # ADHD/autism: 3~4 (refused/don't know)
  is.na(mcs5_parent_cm_interview[, c("EPADHD00", "EPAUTS00")]) <- mcs5_parent_cm_interview[, c("EPADHD00", "EPAUTS00")] > 2
  
  # How close to child: 5 (don't know/don't wish to answer)
  is.na(mcs5_parent_cm_interview$EPSCHC00) <- mcs5_parent_cm_interview$EPSCHC00 == 5
  
  # Bullying: 4 (don't know/don't wish to answer)
  is.na(mcs5_parent_cm_interview$EPSDPB00) <- mcs5_parent_cm_interview$EPSDPB00 == 4
  
  mcs5_parent_cm_interview <- mcs5_parent_cm_interview %>%
    group_by(MCSID, CNUM) %>%
    
    # Regular bedtime: if both parents say "always" return 1, otherwise 0
    mutate(EPBERE00 = case_when(
      any(EPBERE00 %in% c(1, 2, 3)) ~ 0,
      all(EPBERE00[!is.na(EPBERE00)] == 4) ~ 1,
      TRUE ~ NA
    )) %>%
    
    # Physical activity/ADHD/autism/longstanding illness: if parents disagree return NA
    mutate_at(c("EPSEHO00", "EPPLFR00", "EPADHD00", "EPAUTS00", "EPCLSI00"), ~if(length(unique(.[!is.na(.)])) != 1){NA} else {unique(.[!is.na(.)])}) %>%
    
    # How close to child/social interaction: mean of parents' responses
    mutate_at(c("EPSCHC00", "EPVIFR00"), ~if(sum(!is.na(.)) == 0){NA} else {mean(.[!is.na(.)])}) %>%
    mutate(EPSDPB00 = case_when(
      any(EPSDPB00 %in% c(2, 3)) ~ 1,
      all(EPSDPB00[!is.na(EPSDPB00)] == 1) ~ 0,
      TRUE ~ NA
    )) %>%
    distinct()
  
  # Merge datasets and return
  return(full_join(mcs4_parent_cm_interview, mcs5_parent_cm_interview, by = c("MCSID", "CNUM")))
}

#######
# Function: recode_data
# Input: initial merged dataset
# Method: recode variables as needed
# Output: recoded dataset
#######
recode_data <- function(data){
  data <- data %>%
    # Study period eligibility markers: identify whether the participant has lived in England at any sweep up to the measurement of social media use
    mutate(
      mcs5_eligible = if_any(contains("ACTRY00") & !FACTRY00, ~ !is.na(.) & . == 1),
      mcs6_eligible = if_any(contains("CTRY00"), ~ !is.na(.) & . == 1)
    ) %>%
    # Exclude any participants who are ineligible for either period
    filter(mcs5_eligible | mcs6_eligible) %>%
    
    mutate(
      ### Define missing values
      # Most variables: negative values
      across(c("PTTYPE2", "NH2", "ADBWGT00", "AHCSEX00", "BOBFLAG2", contains("CCANC"), "CCOBFLAG3", "DCPCAB00", "DCWRAB00", "DCACDN00", contains("DCANC"), "DDC06E00", "DDDEBDTOT", "DCSC0012", "DCSC020A", "DCOBFLAG", contains("ECCHC"), contains("ECQ"), "EOBFLAG5", contains("EQ"), "EEBDTO_T", contains("FCCHIC"), "FEBDTOT", "FCOBFLG6", "FCCOPY00", "FCSLWK00", "FCSLLN00", "FCWUWK00", "FCPHEX00", "FCPLWE00", "FCPLWK00", "FCYPARCN", "GCOBFLG7", contains("OUTC00"), contains("ROOW00"), "BDRSMB12", "CDRSMB23", "DDOEDE00", contains("ADSA00"), "EOEDE000"), ~ case_when(
        . < 0 ~ NA,
        TRUE ~ .
      )),
      # PSU identifier: the string "-1"
      SPTN00 = case_when(
        SPTN00 == "-1" ~ NA,
        TRUE ~ SPTN00
      ),
      # Regions of residence: 10~13 (regions outside England)
      across(contains("AREGN00"), ~ case_when(
        . > 9 ~ NA,
        TRUE ~ .
      )),
      # Certain consent markers: 4 (and sometimes also 5) (unable to administer)
      across(c(contains("CCANC"), contains("DCANC"), "DCACDN00"), ~ case_when(
        . > 3 ~ NA,
        TRUE ~ .
      )),
      # Age 11, attitudes to pirating: 4 (don't know)
      ECQ52X00 = case_when(
        ECQ52X00 == 4 ~ NA,
        TRUE ~ ECQ52X00
      ),
      # Age 11, exercise: 8~9 (don't know/refused)
      across(c("EPSEHO00", "EPPLFR00"), ~ case_when(
        . > 7 ~ NA,
        TRUE ~ .
      )),
      # Housing tenure: 10 (other) & 11~13 (don't know/don't wish to answer/no answer)
      across(contains("ROOW00"), ~ case_when(
        . > 9 ~ NA,
        TRUE ~ .
      )),
      # Age 17, household moves: 3~4 (don't know/prefer not to answer)
      GHADSA00 = case_when(
        GHADSA00 > 2 ~ NA,
        TRUE ~ GHADSA00
      ),
      # Age 17, organised activities etc., smoking and religion: 7~9 (don't know/don't wish to answer/no answer)
      across(c("GCORGA00", "GCVOLW00", "GCPOLM00", "GCSMOK00", "GCRLSV00"), ~ case_when(
        . > 6 ~ NA,
        TRUE ~ .
      )),
      # Age 17, self-rated health: 6~8 (don't know/don't wish to answer/no answer)
      GCCGHE00 = case_when(
        GCCGHE00 > 5 ~ NA,
        TRUE ~ GCCGHE00
      ),
      # Age 17, social capital: 4~6 (don't know/don't wish to answer/no answer)
      across(c("GCSAFF00", "GCTRSS00", "GCNCLS00"), ~ case_when(
        . > 3 ~ NA,
        TRUE ~ .
      )),
      
      ### Recode variables
      # Study participation: productive = 1, others = 0
      AAOUTC00 = case_match(AAOUTC00,
        2 ~ 1,
        .default = 0
      ),
      across(contains("AOUTC00") & !AAOUTC00, ~ case_match(.,
        1 ~ 1,
        .default = 0
      )),
      # Housing tenure: owner-occupied (with or without mortgage) = 1, others = 0
      across(contains("ROOW00"), ~ case_match(.,
        c(1,2) ~ 1,
        3:9 ~ 0,
        .default = .
      )),
      # Sex at birth and household moves: translate 1/2 to 0/1 coding
      across(c("AHCSEX00", contains("ADSA00")), ~ case_match(.,
        1 ~ 0,
        2 ~ 1,
        .default = .
      )),
      # Ethnicity: collapse to binary (or else imputation doesn't work :-()
      DDC06E00 = case_match(DDC06E00,
        1 ~ 0,
        3:6 ~ 1
      ),
      # SR anxiety: binarise (0 = never and 1 = at least some of the time)
      DCSC0012 = case_match(DCSC0012,
        3 ~ 0,
        c(1, 2) ~ 1,
        .default = DCSC0012
      ),
      # ADHD, autism, domestic violence, longstanding illness (parent and CM), one consent variable: translate 1/2 (yes/no) to 1/0 coding
      across(c("DPADHD00", "DPAUTS00", "DPCLSI00", "DPFORC00", "DPLOLM00", "EPADHD00", "EPAUTS00", "EPCLSI00", "EPFORC00", "FCYPARCN"), ~ case_match(.,
        2 ~ 0,
        .default = .
      )),
      # Rural-urban indicator: swap 5 (town sparse) and 6 (town less sparse), 7 (village & dispersed sparse) and 8 (village & dispersed less sparse)
      across(c("darururb", "EARURURB"), ~ case_match(.,
        5 ~ 6,
        6 ~ 5,
        7 ~ 8,
        8 ~ 7,
        .default = .
      )),
      # Overweight indicators: Group overweight (1) and obese (2) together
      across(c("BOBFLAG2", "CCOBFLAG3", "DCOBFLAG", "EOBFLAG5", "FCOBFLG6", "GCOBFLG7"), ~ case_match(.,
        2 ~ 1,
        .default = .
      )),
      # HES event indicators: if missing set to 0
      across(c("HES_ei1", "HES_ei2"), ~ case_when(
        is.na(.) ~ 0,
        TRUE ~ .
      )),
      # Age 11, social media use: reverse code
      ECQ09X00 = 6 - ECQ09X00,
      # Age 11, spending time with friends: harmonise ECQ18X00, ECQ19X00, FCPLWK00 and FCPLWE00 (already compliant) with the following coding:
      # 1 = most weekends/most days/at least once a week
      # 2 = at least once a month
      # 3 = less often than once a month
      # 4 = never/don't have any friends
      ECQ18X00 = case_match(ECQ18X00,
        5 ~ 4,
        .default = ECQ18X00
      ),
      ECQ19X00 = case_match(ECQ19X00,
        2 ~ 1,
        3 ~ 2,
        4 ~ 3,
        5 ~ 4,
        6 ~ 4,
        .default = ECQ19X00
      ),
      FCPLWK00 = case_match(FCPLWK00,
        2 ~ 1,
        3 ~ 2,
        4 ~ 3,
        5 ~ 4,
        .default = FCPLWK00
      ),
      # Consent variables: if missing, set to 0
      across(c("mcs1_consents", contains("CCANC"), "DCACDN00", contains("DCANC"), contains("ECCHC"), contains("FCCHIC"), "FCYPARCN", "FHSALIW"), ~ case_when(
        is.na(.) ~ 0,
        TRUE ~ .
      )),
      # Variables indicating 3 consents simultaneously: multiply "yes" response by 3
      across(c("ECCHC20A", "ECCHC10A", "FCCHIC0A"), ~ case_match(.,
        1 ~ 3,
        .default = .
      )),
      # Variable indicating 2 consents simultaneously: multiply "yes" response by 2
      FCCHIC2A = case_match(FCCHIC2A,
        1 ~ 2,
        .default = FCCHIC2A
      ),
      # Certain other consent markers: recode as 0 (refused by parent or child) and 1 (consented)
      across(c(contains("CCANC"), contains("DCANC"), "DCACDN00"), ~ case_match(.,
        3 ~ 1,
        2 ~ 0,
        .default = .
      )),
      ## Age 17, social capital: binarise
      # 0 = very true, 1 = partly true/not true at all
      across(c("GCSAFF00", "GCTRSS00"), ~ case_match(.,
        1 ~ 0,
        2:3 ~ 1
      )),
      # 0 = not true at all, 1 = very true/partly true
      GCNCLS00 = case_match(GCNCLS00,
        1:2 ~ 1,
        3 ~ 0
      ),
      # Consent to HES linkage: NA -> 0
      CONSENT = case_match(CONSENT,
        NA ~ 0,
        .default = CONSENT
      ),
      
      ### Derive compound variables
      # Sleep: derived as number of hours between SR average sleep time and wake time, less the SR time taken to fall asleep
      mcs6_sleep = 8 + FCWUWK00 - FCSLWK00 - (FCSLLN00 - 1)/4,
      # Exercise: sum of club/class-based and other
      mcs5_exercise = 14 - (EPSEHO00 + EPPLFR00),
      # Social interaction: sum of weekday and weekend variables as harmonised
      mcs5_social_interaction = 8 - (ECQ18X00 + ECQ19X00),
      mcs6_social_interaction = 8 - (FCPLWE00 + FCPLWK00),
      # Age 7, educational achievement: sum of parent-reported difficulties
      mcs4_edu_achievement = 3 - (DPAMTH00 + DPARED00 + DPAWRT00),
      # Age 11, educational achievement: sum of teacher ratings
      mcs5_edu_achievement = 20 - (EQ2A + EQ2C + EQ2D + EQ2H),
      # Antisocial behaviour: sum of self-reported behaviours
      mcs5_antisocial_beh = 8 - (ECQ25X00 + ECQ26X00 + ECQ27X00 + ECQ28X00),
      # Age 14, household crowding: divide number of residents by number of rooms
      mcs6_crowding = mcs6_residents / FPROMA00,
      # Age 17, organisations etc.: binarise (any participation at least once a month = 1, otherwise 0)
      mcs7_organisations = case_when(
        !is.na(GCORGA00) & GCORGA00 < 4 ~ 1,
        !is.na(GCVOLW00) & GCVOLW00 < 4 ~ 1,
        !is.na(GCPOLM00) & GCPOLM00 < 4 ~ 1,
        !is.na(GCORGA00) & !is.na(GCVOLW00) & !is.na(GCPOLM00) ~ 0,
        TRUE ~ NA
      ),
      # Move derived survey date estimate to the middle of the month, rather than the first day
      ## But this will not be applied to the "default" dates used if no survey date is available, as these represent the latest possible survey date
      mcs5_date = mcs5_date + days(14),
      mcs6_date = mcs6_date + days(14),
      mcs7_date = mcs7_date + days(14),
      # Cox model censoring times: the time between survey dates if both dates are available, otherwise the minimum possible time between surveys (using any dates that are available)
      censtime1 = case_when(
        is.na(mcs6_date) & is.na(mcs5_date) ~ as.integer(as.Date("2015-01-01") - as.Date("2013-03-01")),
        is.na(mcs6_date) & !is.na(mcs5_date) ~ as.integer(as.Date("2015-01-01") - as.Date(mcs5_date)),
        !is.na(mcs6_date) & is.na(mcs5_date) ~ as.integer(as.Date(mcs6_date) - as.Date("2013-03-01")),
        !is.na(mcs6_date) & !is.na(mcs5_date) ~ as.integer(as.Date(mcs6_date) - as.Date(mcs5_date))
      ),
      censtime2 = case_when(
        is.na(mcs7_date) & is.na(mcs6_date) ~ as.integer(as.Date("2018-01-01") - as.Date("2016-05-01")),
        is.na(mcs7_date) & !is.na(mcs6_date) ~ as.integer(as.Date("2018-01-01") - as.Date(mcs6_date)),
        !is.na(mcs7_date) & is.na(mcs6_date) ~ as.integer(as.Date(mcs7_date) - as.Date("2016-05-01")),
        !is.na(mcs7_date) & !is.na(mcs6_date) ~ as.integer(as.Date(mcs7_date) - as.Date(mcs6_date))
      ),
      # Time of HES occurrence: difference in days between HES date and (estimated) date of social media measurement
      HES_time1 = case_when(
        is.na(mcs5_date) ~ as.integer(as.Date(HES_date1) - as.Date("2013-03-01")),
        !is.na(mcs5_date) ~ as.integer(as.Date(HES_date1) - as.Date(mcs5_date))
      ),
      HES_time2 = case_when(
        is.na(mcs6_date) ~ as.integer(as.Date(HES_date2) - as.Date("2016-05-01")),
        !is.na(mcs6_date) ~ as.integer(as.Date(HES_date2) - as.Date(mcs6_date))
      ),
      # Where no HES event occurred, set event time to censoring time
      HES_time1 = case_when(HES_ei1 == 0 ~ censtime1, TRUE ~ HES_time1),
      HES_time2 = case_when(HES_ei2 == 0 ~ censtime2, TRUE ~ HES_time2)
    )
  
  ## The following five variables include alternatives for sensitivity analyses (drawing from pre-exposure variables only)
  # Survey participation: number of sweeps in which the household was productive
  data$participation <- rowSums(data %>% select(contains("OUTC00")))
  data$mcs4_participation <- rowSums(data %>% select("AAOUTC00", "BAOUTC00", "CAOUTC00"))
  data$mcs5_participation <- rowSums(data %>% select("AAOUTC00", "BAOUTC00", "CAOUTC00", "DAOUTC00"))
  
  # Housing tenure: proportion of sweeps in which the household was in owner-occupied accommodation
  data$tenure <- rowMeans(data %>% select(contains("ROOW00")), na.rm = TRUE)
  data$mcs4_tenure <- rowMeans(data %>% select("ADROOW00", "BDROOW00", "CDROOW00", "DDROOW00"), na.rm = TRUE)
  data$mcs5_tenure <- rowMeans(data %>% select("ADROOW00", "BDROOW00", "CDROOW00", "DDROOW00", "EROOW00"), na.rm = TRUE)
  
  # Household moves: proportion of sweeps in which the household had moved since the previous sweep
  data$household_moves <- rowMeans(data %>% select(BDRSMB12, CDRSMB23, contains("ADSA00")), na.rm = TRUE)
  data$mcs4_household_moves <- rowMeans(data %>% select(BDRSMB12, CDRSMB23, DHADSA00), na.rm = TRUE)
  data$mcs5_household_moves <- rowMeans(data %>% select(BDRSMB12, CDRSMB23, DHADSA00, EHADSA00), na.rm = TRUE)
  
  # Other consents: sum of consents obtained
  data$other_consents <- rowSums(data %>% select(c("mcs1_consents", contains("CCANC"), "DCACDN00", contains("DCANC"), contains("ECCHC"), contains("FCCHIC"), "FCYPARCN", "FHSALIW")))
  data$mcs4_other_consents <- rowSums(data %>% select(c("mcs1_consents"), contains("CCANC"), "DCACDN00", contains("DCANC")))
  data$mcs5_other_consents <- rowSums(data %>% select(c("mcs1_consents"), contains("CCANC"), "DCACDN00", contains("DCANC"), contains("ECCHC")))
  
  # Overweight: proportion of sweeps in which the cohort member is marked as overweight or obese
  data$overweight <- rowMeans(data[, c("BOBFLAG2", "CCOBFLAG3", "DCOBFLAG", "EOBFLAG5", "FCOBFLG6", "GCOBFLG7")], na.rm = TRUE)
  is.na(data$overweight) <- is.nan(data$overweight)
  data$mcs4_overweight <- rowMeans(data[, c("BOBFLAG2", "CCOBFLAG3", "DCOBFLAG")], na.rm = TRUE)
  is.na(data$mcs4_overweight) <- is.nan(data$mcs4_overweight)
  data$mcs5_overweight <- rowMeans(data[, c("BOBFLAG2", "CCOBFLAG3", "DCOBFLAG", "EOBFLAG5")], na.rm = TRUE)
  is.na(data$mcs5_overweight) <- is.nan(data$mcs5_overweight)
  
  # Cognitive ability: predictions from confirmatory factor analysis (single factor) of cognitive assessments
  data[, c("CCNVABIL", "CCPSABIL", "DCPCAB00", "DCWRAB00")] <- zap_labels(data[, c("CCNVABIL", "CCPSABIL", "DCPCAB00", "DCWRAB00")])
  data$cm_cog_abil <- as.vector(lavPredict(cfa("lf =~ CCNVABIL + CCPSABIL + DCPCAB00 + DCWRAB00", data, missing = "ml")))

  # Remove unneeded variables
  data <- data %>%
    select(-c(censtime1, censtime2, FAREGN00, FCWUWK00, FCSLWK00, FCSLLN00, EPSEHO00, EPPLFR00, ECQ18X00, ECQ19X00, FCPLWE00, FCPLWK00, contains("EQ"), DPAMTH00, DPARED00, DPAWRT00, ECQ25X00:ECQ28X00, contains("ACTRY00"), mcs5_date, mcs6_date, mcs7_date, HES_date1, HES_date2, contains("OBFL"), CCNVABIL, CCPSABIL, DCPCAB00, DCWRAB00, mcs1_consents, contains("CCANC"), DCACDN00, contains("DCANC"), contains("ECCHC"), contains("FCCHIC"), FCYPARCN, FHSALIW, contains("OUTC00"), contains("ROOW00"), BDRSMB12, CDRSMB23, contains("ADSA00"), GCORGA00, GCVOLW00, GCPOLM00, mcs6_residents, FPROMA00))
  
  # Relabel categorical variables that have been recoded as needed
  val_labels(data$AHCSEX00) <- c(Male = 0, Female = 1)
  val_labels(data[, c("DAREGN00", "EAREGN00")]) <- c(`North East` = 1, `North West` = 2, `Yorkshire and the Humber` = 3, `East Midlands` = 4, `West Midlands` = 5, `East of England` = 6, London = 7, `South East` = 8, `South West` = 9)
  val_labels(data$DCSC0012) <- c(Never = 0, `At least some of the time` = 1)
  val_labels(data[, c("DPADHD00", "DPAUTS00", "DPFORC00", "DPLOLM00", "EPADHD00", "EPAUTS00", "EPFORC00", "EPLOLM00")]) <- c(No = 0, Yes = 1)

  # Lists of variables by type
  continuous_vars <- c("ADBWGT00", "DDDEBDTOT", "ECQ09X00", "ECQ10B00", "ECQ75X00", "EEBDTO_T", "FEBDTOT", "FCPHEX00", "HES_ei1", "HES_ei2", "mcs6_sm_per_day", "DPSCHC00", "DPVIFR00", "EPSCHC00", "EPVIFR00", "WEIGHT1", "DDOEDE00", "darururb", "DIMDSCOE", "EOEDE000", "EARURURB", "EIMDSCOE", "DDKESSLER", "DDD05S00", "ED05S00", "EPEMPN00", "mcs5_kessler", "mcs5_wealth", "FPWRDSCM", "FPEMPN00", "mcs4_left_education", "mcs5_left_education", "mcs6_sleep", "mcs5_exercise", "mcs5_social_interaction", "mcs6_social_interaction", "mcs5_edu_achievement", "participation", "mcs5_antisocial_beh", "tenure", "household_moves", "other_consents", "overweight", "cm_cog_abil", "GCSMOK00", "mcs6_crowding", "GCRLSV00", "GCCGHE00")
  binary_vars <- c("AHCSEX00", "DDC06E00", "DCSC0012", "DCSC020A", "ECQ80X0A", "DPADHD00", "DPAUTS00", "DPBERE00", "DPBULS00", "EPBERE00", "EPADHD00", "EPAUTS00", "EPSDPB00", "DPLOLM00", "DPFORC00", "EPLOLM00", "EPFORC00", "DPCLSI00", "EPCLSI00", "mcs7_organisations", "EPVOTE00", "GCSAFF00", "GCTRSS00", "GCNCLS00")
  ordered_factors <- c("ECQ52X00", "ECQ53A00", "ECQ53B00", "FCCOPY00", "mcs4_edu_achievement")
  unordered_factors <- c("DAREGN00", "EAREGN00")
  
  # Convert discrete variables to factors (with labels as values where available) and drop unused levels
  for(i in c(binary_vars, unordered_factors, ordered_factors)){
    data[[i]] <- as_factor(data[[i]], only_labelled = FALSE)
    data[[i]] <- droplevels(data[[i]])
  }
  
  # Configure ordered factors
  for(i in c("ECQ52X00", "FCCOPY00")){
    data[[i]] <- factor(data[[i]], ordered = TRUE, levels = c("Very wrong", "A bit wrong", "Not wrong"))
  }
  for(i in c("ECQ53A00", "ECQ53B00")){
    data[[i]] <- factor(data[[i]], ordered = TRUE, levels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree"))
  }
  data$mcs4_edu_achievement <- factor(data$mcs4_edu_achievement, ordered = TRUE, levels = c(0, 1, 2, 3))
  
  # Remove Stata-format data labels, drop unused factor levels and convert from tibble to prevent multiple imputation failing
  data <- data %>% zap_labels()
  data <- as.data.frame(data)
  
  return(data)
}

#######
# Function: weighting_mi
# Input: the recoded dataset
# Method: prepares and runs multiple imputation for the purpose of deriving exclusion weights
# Output: imputed datasets
#######
weighting_mi <- function(data){
  # Make vector of imputation methods, setting some variables to not be imputed
  meth <- make.method(data)
  meth[c("MCSID", "CNUM", "INCLUDED1", "INCLUDED2", "participation", "other_consents")] <- ""
  
  # Set up predictor matrix and exclude certain variables from being predictors
  pM <- make.predictorMatrix(data)
  pM[, c("MCSID", "CNUM", "INCLUDED1", "INCLUDED2")] <- 0
  pM[c("MCSID", "CNUM", "INCLUDED1", "INCLUDED2", "participation", "other_consents"),] <- 0
  
  # Run multiple imputation and return mids output
  return(
    mice(
      data,
      m = 50,
      method = meth,
      predictorMatrix = pM,
      visitSequence = "monotone",
      maxit = 10,
      printFlag = FALSE
    )
  )
}

#######
# Function: derive_weights
# Input: imputed datasets
# Method: fits logistic regression models to the imputed data, predicts probabilities of exclusion and calculates weights as the inverse of this probability
# Output: a list of vectors of inverse probability weights to account for selection
#######
derive_weights <- function(data){
  # Fit logistic regression models and make predictions
  models1 <- with(data, predict(glm(INCLUDED1 ~ participation + tenure + household_moves + FPWRDSCM + overweight + ADBWGT00 + other_consents + WEIGHT1 + mcs7_organisations + GCSMOK00 + mcs6_crowding + EPVOTE00 + GCRLSV00 + GCCGHE00 + GCSAFF00 + GCTRSS00 + GCNCLS00 + cm_cog_abil + mcs4_edu_achievement + DCSC0012 + DDC06E00 + DDOEDE00 + DDDEBDTOT + DPADHD00 + DPAUTS00 + DDKESSLER + DDD05S00 + darururb + AHCSEX00, family = binomial(link = "logit")), type = "response"))
  models2 <- with(data, predict(glm(INCLUDED2 ~ participation + tenure + household_moves + FPWRDSCM + overweight + ADBWGT00 + other_consents + WEIGHT1 + mcs7_organisations + GCSMOK00 + mcs6_crowding + EPVOTE00 + GCRLSV00 + GCCGHE00 + GCSAFF00 + GCTRSS00 + GCNCLS00 + cm_cog_abil + mcs4_edu_achievement + DCSC0012 + DDC06E00 + DDOEDE00 + DDDEBDTOT + DPADHD00 + DPAUTS00 + DDKESSLER + DDD05S00 + darururb + AHCSEX00, family = binomial(link = "logit")), type = "response"))
  
  # Calculate and return weights
  return(list(
    period1 = list(
      included = data$data$INCLUDED1,
      weights = 1 / rowMeans(do.call(cbind, models1$analyses))
    ),
    period2 = list(
      included = data$data$INCLUDED2,
      weights = 1 / rowMeans(do.call(cbind, models2$analyses))
    )
  ))
}

#######
# Function: truncate_weights
# Input: raw weights
# Method: truncates the distribution of weights at the 98th percentile
# Output: a list of truncated weights vectors
#######
truncate_weights <- function(weights){
  weights$period1$weights[weights$period1$weights > quantile(weights$period1$weights[as.logical(weights$period1$included)], 0.98)] <- quantile(weights$period1$weights[as.logical(weights$period1$included)], 0.98)
  weights$period2$weights[weights$period2$weights > quantile(weights$period2$weights[as.logical(weights$period2$included)], 0.98)] <- quantile(weights$period2$weights[as.logical(weights$period2$included)], 0.98)
  
  return(weights)
}

#######
# Function: add_weights
# Inputs: recoded dataset and truncated weights vectors
# Method: multiplies the non-consent weights by the design weights and adds this new column to the dataset
# Output: a dataset that includes the final weights
#######
add_weights <- function(data, weights){
  data$final_weight1 <- weights$period1$weights * data$WEIGHT1
  data$final_weight2 <- weights$period2$weights * data$WEIGHT1

  return(data)
}

#######
# Function: multiple_imputation
# Inputs: recoded dataset and model formula
# Method: prepares and performs multiple imputation for the substantive model described by the provided formula
# Output: imputed datasets
#######
multiple_imputation <- function(data, formula, period, type){  
  # Remove non-consenting participants, participants without region specified and participants outside England
  data <- data[!is.na(data$CONSENT) & data$CONSENT,] %>% select(-CONSENT)
  if(period == 1){
    data <- data[!is.na(data$DAREGN00),] %>% select(-EAREGN00, -final_weight2) %>% rename(final_weight = final_weight1) %>% filter(mcs5_eligible)
    if(type == "main"){
      data <- data %>% select(-c(mcs5_eligible, mcs6_eligible, mcs4_participation, mcs4_tenure, mcs4_household_moves, mcs4_other_consents, mcs5_participation, mcs5_tenure, mcs5_household_moves, mcs5_other_consents))
    } else {
      data <- data %>% select(-c(mcs5_eligible, mcs6_eligible, participation, tenure, household_moves, other_consents, mcs5_participation, mcs5_tenure, mcs5_household_moves, mcs5_other_consents))
    }
  } else {
    data <- data[!is.na(data$EAREGN00),] %>% select(-DAREGN00, -final_weight1) %>% rename(final_weight = final_weight2) %>% filter(mcs6_eligible)
    if(type == "main"){
      data <- data %>% select(-c(mcs5_eligible, mcs6_eligible, mcs4_participation, mcs4_tenure, mcs4_household_moves, mcs4_other_consents, mcs5_participation, mcs5_tenure, mcs5_household_moves, mcs5_other_consents))
    } else {
      data <- data %>% select(-c(mcs5_eligible, mcs6_eligible, participation, tenure, household_moves, other_consents, mcs4_participation, mcs4_tenure, mcs4_household_moves, mcs4_other_consents))
    }
  }
  
  # Specify auxiliary variables
  auxil_vars <- if(type == "main"){
    "participation + tenure + household_moves + FPWRDSCM + overweight + ADBWGT00 + other_consents + WEIGHT1 + mcs7_organisations + GCSMOK00 + mcs6_crowding + EPVOTE00 + GCRLSV00 + GCCGHE00 + GCSAFF00 + GCTRSS00 + GCNCLS00"
  } else if(period == 1){
    "mcs4_participation + mcs4_tenure + mcs4_household_moves + mcs4_overweight + ADBWGT00 + mcs4_other_consents + WEIGHT1"
  } else {
    "mcs5_participation + mcs5_tenure + mcs5_household_moves + mcs5_overweight + ADBWGT00 + mcs5_other_consents + WEIGHT1 + EPVOTE00" 
  }
  
  smformula <- as.formula(paste(formula, auxil_vars, sep = " + "))
  
  data <- data %>% select(any_of(c("MCSID", "CNUM", "PTTYPE2", "SPTN00", "NH2", "final_weight", if(period == 1){c("HES_time1", "HES_ei1")} else{c("HES_time2", "HES_ei2")}, names(model.frame(smformula, data)))))
  
  # Create vector of imputation methods, specifying which variables are not to be imputed
  meth <- make.method(data, defaultMethod = c("norm", "logreg", "mlogit", "podds"))

  # Create predictor matrix, specifying which variables are not to be used as predictors
  pM <- make.predictorMatrix(data)
  pM[, c("MCSID", "CNUM", "PTTYPE2", "SPTN00", "NH2", "final_weight")] <- 0
  pM[c("MCSID", "CNUM", "PTTYPE2", "SPTN00", "NH2", if(period == 1){c("DAREGN00", "HES_time1", "HES_ei1")}, if(period == 2){c("EAREGN00", "HES_time2", "HES_ei2")}, "WEIGHT1", "final_weight"),] <- 0 # these are redundant but for completeness...
  if(type == "main"){
    pM[c("participation", "other_consents"), ] <- 0
  } else if(period == 1){
    pM[c("mcs4_participation", "mcs4_other_consents"), ] <- 0
  } else if(period == 2){
    pM[c("mcs5_participation", "mcs5_other_consents"), ] <- 0
  }
  
  # Perform multiple imputation using smcfcs
  return(
    future_map(
      1:16,
      function(x){
        smcfcs(
          originaldata = data,
          smtype = "coxph",
          smformula = smformula,
          method = meth,
          predictorMatrix = pM,
          m = 3,
          numit = 10,
          rjlimit = 10000
      )},
      .options = furrr_options(seed = TRUE)
    )
  )
}

#######
# Function: concatenate_imputations
# Inputs: list of lists of imputed datasets
# Method: combines lists into a single list
# Output: a single imputationList object
#######
concatenate_imputations <- function(imputations){
  implist <- list()
  
  n <- 1
  
  for(i in seq_along(1:length(imputations))){
    for(j in seq_along(1:length(imputations[[i]]$impDatasets))){
      implist[[n]] <- imputations[[i]]$impDatasets[[j]]
      n <- n + 1
    }
  }
  
  return(imputationList(implist))
}

#######
# Function: standardise
# Input: an imputationList object and an argument dictating whether to standardise social media use variables (default TRUE)
# Method: standardises (Z-scores) exposure variables (except attitude to pirating, and sleep regularity for period 1) and interaction variables (except sex assigned at birth and ethnicity) in each imputation, set contrast coding for attitude to pirating and relabel ethnicity
# Output: an imputationList with standardised exposure variables
#######
standardise <- function(data, include_sm = TRUE){
  for(i in seq_along(1:length(data$imputations))){
    for(j in c(if(include_sm) "ECQ09X00", if(include_sm) "mcs6_sm_per_day", "EPEMPN00", "FPEMPN00", "mcs6_sleep", "mcs5_exercise", "FCPHEX00", "mcs5_social_interaction", "mcs6_social_interaction", "DDDEBDTOT", "EEBDTO_T", "FEBDTOT", "DDOEDE00", "EOEDE000", "mcs5_wealth")){
      if(j %in% names(data$imputations[[i]])){
        data$imputations[[i]][, j] <- scale(data$imputations[[i]][, j])
      }
    }
    
    for(j in c("ECQ52X00", "FCCOPY00")){
      if(j %in% names(data$imputations[[i]])){
        contrasts(data$imputations[[i]][, j]) <- contr.treatment(3)
      }
    }
    
    data$imputations[[i]]$DDC06E00 <- recode(data$imputations[[i]]$DDC06E00, `0` = "White", `1` = "Ethnic minority")
  }
  
  return(data)
}

#######
# Function: unweighted_coxph
# Inputs: smcfcs object and model formula
# Method: fits unweighted Cox proportional hazards models to the imputed datasets in the smcfcs object
# Output: list of model outputs
#######
unweighted_coxph <- function(data, formula){
  return(
    with(data, coxph(as.formula(formula)))
  )
}

#######
# Function: weighted_coxph
# Inputs: smcfcs object and model formula
# Method: creates survey design object from the imputed datasets and fits weighted Cox proportional hazards models to that object
# Output: list of model outputs
#######
weighted_coxph <- function(data, formula){
  # Create survey design object
  svyobj <- svydesign(
    ids = ~SPTN00,
    strata = ~PTTYPE2,
    fpc = ~NH2,
    data = data,
    weights = ~final_weight
  )
  
  # Handling strata with only one PSU
  options(survey.lonely.psu = "adjust")
  
  # Fit models
  return(
    with(svyobj, svycoxph(as.formula(formula)))
  )
}

#######
# Function: categorise_smu
# Input: recoded dataset including survey weights
# Method: recodes social media use variables into categorical variables
# Output: a further recoded dataset
#######
categorise_smu <- function(data){
  data_recoded <- data %>%
    mutate(
      ECQ09X00 = case_match(ECQ09X00,
        1 ~ "None", # ECQ09X00 has already been reverse-coded
        2:4 ~ "Occasional",
        5 ~ "Frequent"
      ),
      mcs6_sm_per_day = case_when(
        mcs6_sm_per_day == 0 ~ "None",
        mcs6_sm_per_day > 0 & mcs6_sm_per_day <= 120 ~ "Moderate",
        mcs6_sm_per_day > 120 ~ "High",
        TRUE ~ NA
      )
    )
  
  # Convert recoded variables to ordered factors and use treatment contrast coding
  data_recoded$ECQ09X00 <- factor(data_recoded$ECQ09X00, ordered = TRUE, levels = c("None", "Occasional", "Frequent"))
  contrasts(data_recoded$ECQ09X00) <- contr.treatment(3, base = 2)
  
  data_recoded$mcs6_sm_per_day <- factor(data_recoded$mcs6_sm_per_day, ordered = TRUE, levels = c("None", "Moderate", "High"))
  contrasts(data_recoded$mcs6_sm_per_day) <- contr.treatment(3, base = 2)
  
  return(data_recoded)
}

#######
# Function: categorise_smu2
# Inputs: recoded dataset including survey weights and a threshold social media use time value
# Method: recodes social media use at age 14 into "none", "moderate" (non-zero but less than threshold) and "high" (more than threshold)
# Output: a further recoded dataset
#######
categorise_smu2 <- function(data, threshold){
  data_recoded <- data %>%
    mutate(
      mcs6_sm_per_day = case_when(
        mcs6_sm_per_day == 0 ~ "None",
        mcs6_sm_per_day > 0 & mcs6_sm_per_day <= threshold ~ "Moderate",
        mcs6_sm_per_day > threshold ~ "High",
        TRUE ~ NA
      )
    )
  
  # Convert recoded variables to ordered factors and use treatment contrast coding
  data_recoded$mcs6_sm_per_day <- factor(data_recoded$mcs6_sm_per_day, ordered = TRUE, levels = c("None", "Moderate", "High"))
  contrasts(data_recoded$mcs6_sm_per_day) <- contr.treatment(3, base = 2)
  
  return(data_recoded)
}
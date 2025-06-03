# Load dependencies
library(targets)
library(tarchetypes)
library(dplyr)
library(furrr)
library(future)
library(future.callr)
library(haven)
library(labelled)
library(lavaan)
library(lubridate)
library(mice)
library(mitools)
library(parallel)
library(purrr)
library(quarto)
library(smcfcs)
library(stringr)
library(survey)
library(survival)
library(tidyr)

# Load additional functions
source("R/functions.R")

# Set random seed
set.seed(3214)

# List of references, file paths, variables and groups of variables for HES datasets (SPSS format)
## admidate/apptdate = Date of episode, rotreat = Region of treatment, d_diagXX = ICD-10 diagnostic codes
HES_filelist <- tribble(
  ~name, ~filename, ~vars, ~groupvars,
  "HES_APC", "data/MCS_EngHealth_HES_APC.sav", c("mcsid", "cnum", "admidate", "rotreat"), "d_diag",
  "HES_OP", "data/MCS_EngHealth_HES_OP.sav", c("mcsid", "cnum", "apptdate", "rotreat"), "d_diag"
)
 
# List of references, file paths and variables for survey datasets (Stata format)
## For more information on variables see supplementary information
survey_filelist <- tribble(
  ~name, ~filename, ~vars,
  "mcs1_cm_derived", "data/mcs1_cm_derived.dta", "ADBWGT00",
  "mcs1_family_derived", "data/mcs1_family_derived.dta", c("AACTRY00", "ADROOW00"),
  "mcs1_hhgrid", "data/mcs1_hhgrid.dta", "AHCSEX00",
  "mcs1_parent_interview", "data/mcs1_parent_interview.dta", c("APLFTE00", "APSCAC00"),
  "mcs2_cm_interview", "data/mcs2_cm_interview.dta", "BOBFLAG2",
  "mcs2_family_derived", "data/mcs2_family_derived.dta", c("BACTRY00", "BDROOW00", "BDRSMB12"),
  "mcs2_parent_interview", "data/mcs2_parent_interview.dta", "BPLFTE00",
  "mcs3_cm_cognitive_assessment", "data/mcs3_cm_cognitive_assessment.dta", c("CCNVABIL", "CCPSABIL", "CCANCA00", "CCANCB00", "CCANCC00", "CCANCD00"),
  "mcs3_cm_interview", "data/mcs3_cm_interview.dta", "CCOBFLAG3",
  "mcs3_family_derived", "data/mcs3_family_derived.dta", c("CACTRY00", "CDROOW00", "CDRSMB23"),
  "mcs3_parent_interview", "data/mcs3_parent_interview.dta", "CPLFTE00",
  "mcs4_cm_cognitive_assessment", "data/mcs4_cm_cognitive_assessment.dta", c("DCPCAB00", "DCWRAB00", "DCANCA00", "DCANCB00", "DCANCC00", "DCANCD00"),
  "mcs4_cm_derived", "data/mcs4_cm_derived.dta", c("DDC06E00", "DDDEBDTOT"),
  "mcs4_cm_interview", "data/mcs4_cm_interview.dta", c("DCSC0012", "DCSC020A", "DCOBFLAG", "DCACDN00"),
  "mcs4_family_derived", "data/mcs4_family_derived.dta", c("DACTRY00", "DAREGN00", "DDOEDE00", "DDROOW00"),
  "mcs4_family_interview", "data/mcs4_family_interview.dta", "DHADSA00",
  "mcs4_geographically_linked_data", "data/mcs4_geographically_linked_data.dta", c("darururb", "DIMDSCOE"),
  "mcs4_parent_cm_interview", "data/mcs4_parent_cm_interview.dta", c("DPADHD00", "DPAUTS00", "DPBERE00", "DPSCHC00", "DPAMTH00", "DPARED00", "DPAWRT00", "DPBULS00", "DPCLSI00", "DPVIFR00"),
  "mcs4_parent_derived", "data/mcs4_parent_derived.dta", c("DDKESSLER", "DDD05S00"),
  "mcs4_parent_interview", "data/mcs4_parent_interview.dta", c("DPLOLM00", "DPFORC00"),
  "mcs4_proxy_partner_interview", "data/mcs4_proxy_partner_interview.dta", "DXEMPA1G",
  "mcs5_cm_cognitive_assessment", "data/mcs5_cm_cognitive_assessment.dta", c("ECCHC20A", "ECCHC20B", "ECCHC20C", "ECCHC20D"),
  "mcs5_cm_interview", "data/mcs5_cm_interview.dta", c("ECQ09X00", "ECQ10B00", "ECQ18X00", "ECQ19X00", "ECQ25X00", "ECQ26X00", "ECQ27X00", "ECQ28X00", "ECQ52X00", "ECQ53A00", "ECQ53B00", "ECQ75X00", "ECQ80X0A", "EOBFLAG5", "ECCHC10A", "ECCHC10B", "ECCHC10C", "ECCHC10D"),
  "mcs5_cm_teacher_survey", "data/mcs5_cm_teacher_survey.dta", c("EQ2A", "EQ2C", "EQ2D", "EQ2H", "EEBDTO_T"),
  "mcs5_family_derived", "data/mcs5_family_derived.dta", c("EACTRY00", "EAREGN00", "EOEDE000", "EROOW00"),
  "mcs5_family_interview", "data/mcs5_family_interview.dta", "EHADSA00",
  "mcs5_geographically_linked_data", "data/mcs5_geographically_linked_data.dta", c("EARURURB", "EIMDSCOE"),
  "mcs5_hhgrid", "data/mcs5_hhgrid.dta", c("EINTM0000", "EINTY0000"),
  "mcs5_parent_cm_interview", "data/mcs5_parent_cm_interview.dta", c("EPBERE00", "EPSEHO00", "EPPLFR00", "EPADHD00", "EPAUTS00", "EPCLSI00", "EPSCHC00", "EPSDPB00", "EPVIFR00"),
  "mcs5_parent_derived", "data/mcs5_parent_derived.dta", "ED05S00",
  "mcs5_parent_interview", "data/mcs5_parent_interview.dta", c("EELIG00", "EPEMPN00", "EPLOLM00", "EPFORC00", "EPPHDE00", "EPPHHO00", "EPPHRF00", "EPPHEE00", "EPPHWO00", "EPPHNE00", "EPHVAL00", "EPMOPA00", "EPINVT00", "EPVOTE00"),
  "mcs5_proxy_partner_interview", "data/mcs5_proxy_partner_interview.dta", "EXPXEL00",
  "mcs6_cm_accelerometer_derived", "data/mcs6_cm_accelerometer_derived.dta", c("FCACCMONTH", "FCACCYEAR"),
  "mcs6_cm_cognitive_assessment", "data/mcs6_cm_cognitive_assessment.dta", c("FCCHIC2A", "FCCHIC2B", "FCCHIC2C"),
  "mcs6_cm_derived", "data/mcs6_cm_derived.dta", c("FEBDTOT", "FCOBFLG6"),
  "mcs6_cm_interview", "data/mcs6_cm_interview.dta", c("FCCOPY00", "FCSLWK00", "FCSLLN00", "FCWUWK00", "FCPHEX00", "FCPLWE00", "FCPLWK00", "FCCHIC0A", "FCCHIC0B", "FCCHIC0C", "FCCHIC0D", "FCYPARCN"),
  "mcs6_cm_tud_harmonised", "data/mcs6_cm_tud_harmonised.dta", c("FCTUDAD", "FCTUDSLOT", "FCTUDACT"),
  "mcs6_family_derived", "data/mcs6_family_derived.dta", c("FACTRY00", "FAREGN00", "FDROOW00"),
  "mcs6_hhgrid", "data/mcs6_hhgrid.dta", c("FHINTM00", "FHINTY00", "FHSALIW", "FHBCHK00"),
  "mcs6_parent_assessment", "data/mcs6_parent_assessment.dta", c("FRESP00", "FPWRDSCM"),
  "mcs6_parent_interview", "data/mcs6_parent_interview.dta", c("FELIG00", "FPEMPN00", "FHADSA00", "FPROMA00"),
  "mcs7_cm_derived", "data/mcs7_cm_derived.dta", "GCOBFLG7",
  "mcs7_cm_interview", "data/mcs7_cm_interview.dta", c("GCORGA00", "GCVOLW00", "GCPOLM00", "GCSMOK00", "GCRLSV00", "GCCGHE00", "GCSAFF00", "GCTRSS00", "GCNCLS00"),
  "mcs7_family_interview", "data/mcs7_family_interview.dta", "GHADSA00",
  "mcs7_hhgrid", "data/mcs7_hhgrid.dta", c("GHINTM00", "GHINTY00"),
  "mcs7_parent_interview", "data/mcs7_parent_interview.dta", "GPROOW00",
  "mcs_longitudinal_family_file", "data/mcs_longitudinal_family_file.dta", c("NOCMHH", "PTTYPE2", "SPTN00", "NH2", "AAOUTC00", "BAOUTC00", "CAOUTC00", "DAOUTC00", "EAOUTC00", "FAOUTC00", "GAOUTC00", "WEIGHT1"),
)

# Lists of model identifiers, relevant study periods and specifications
model_spec_list <- tribble(
  ~id, ~period, ~modelformula,
  "sm1", 1, "Surv(HES_time1, HES_ei1) ~ ECQ09X00 + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + AHCSEX00 + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "sm2", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "NCem1", 1, "Surv(HES_time1, HES_ei1) ~ EPEMPN00 + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + AHCSEX00 + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "NCem2", 2, "Surv(HES_time2, HES_ei2) ~ FPEMPN00 + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "NCcd1", 1, "Surv(HES_time1, HES_ei1) ~ ECQ52X00 + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + AHCSEX00 + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "NCcd2", 2, "Surv(HES_time2, HES_ei2) ~ FCCOPY00 + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "CXsl1", 1, "Surv(HES_time1, HES_ei1) ~ EPBERE00 + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + AHCSEX00 + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "CXsl2", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sleep + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "CXex1", 1, "Surv(HES_time1, HES_ei1) ~ mcs5_exercise + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + AHCSEX00 + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "CXex2", 2, "Surv(HES_time2, HES_ei2) ~ FCPHEX00 + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "CXsi1", 1, "Surv(HES_time1, HES_ei1) ~ mcs5_social_interaction + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + AHCSEX00 + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "CXsi2", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_social_interaction + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "PCtd1", 1, "Surv(HES_time1, HES_ei1) ~ EEBDTO_T + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + AHCSEX00 + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "PCtd2", 2, "Surv(HES_time2, HES_ei2) ~ FEBDTOT + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00"
)

sm1_adj_model_spec_list <- tribble(
  ~id, ~modelformula,
  "none", "Surv(HES_time1, HES_ei1) ~ ECQ09X00",
  "mh", "Surv(HES_time1, HES_ei1) ~ ECQ09X00 + DDDEBDTOT",
  "nonmh", "Surv(HES_time1, HES_ei1) ~ ECQ09X00 + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + AHCSEX00 + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education"
)

sm2_adj_model_spec_list <- tribble(
  ~id, ~modelformula,
  "none", "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day",
  "mh", "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day + EEBDTO_T + HES_ei1",
  "nonmh", "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00"
)

cat_model_spec_list <- tribble(
  ~id, ~period, ~modelformula,
  "sm1", 1, "Surv(HES_time1, HES_ei1) ~ ECQ09X00 + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + AHCSEX00 + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "sm2", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00"
)

interax_model_spec_list <- tribble(
  ~id, ~period, ~modelformula,
  "sm1_sex", 1, "Surv(HES_time1, HES_ei1) ~ ECQ09X00*AHCSEX00 + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "sm1_eth", 1, "Surv(HES_time1, HES_ei1) ~ ECQ09X00*DDC06E00 + DDOEDE00 + DDD05S00 + AHCSEX00 + DAREGN00 + DIMDSCOE + darururb + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "sm1_inc", 1, "Surv(HES_time1, HES_ei1) ~ ECQ09X00*DDOEDE00 + DDC06E00 + DDD05S00 + AHCSEX00 + DAREGN00 + DIMDSCOE + darururb + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "sm1_mh", 1, "Surv(HES_time1, HES_ei1) ~ ECQ09X00*DDDEBDTOT + DDC06E00 + DDD05S00 + AHCSEX00 + DAREGN00 + DIMDSCOE + darururb + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDOEDE00 + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "sm2_sex", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day*AHCSEX00 + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "sm2_eth", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day*DDC06E00 + EOEDE000 + mcs5_wealth + ED05S00 + AHCSEX00 + EAREGN00 + EIMDSCOE + EARURURB + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "sm2_inc", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day*EOEDE000 + AHCSEX00 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "sm2_wlt", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day*mcs5_wealth + AHCSEX00 + EOEDE000 + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "sm2_mh", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day*EEBDTO_T + AHCSEX00 + EOEDE000 + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + mcs5_wealth + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00"
)

cat_interax_model_spec_list <- tribble(
  ~id, ~period, ~modelformula,
  "sm1_sex", 1, "Surv(HES_time1, HES_ei1) ~ ECQ09X00*AHCSEX00 + DDOEDE00 + DDD05S00 + DDC06E00 + DAREGN00 + DIMDSCOE + darururb + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "sm1_inc", 1, "Surv(HES_time1, HES_ei1) ~ ECQ09X00*DDOEDE00 + DDC06E00 + DDD05S00 + AHCSEX00 + DAREGN00 + DIMDSCOE + darururb + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDDEBDTOT + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "sm1_mh", 1, "Surv(HES_time1, HES_ei1) ~ ECQ09X00*DDDEBDTOT + DDC06E00 + DDD05S00 + AHCSEX00 + DAREGN00 + DIMDSCOE + darururb + DCSC0012 + DDKESSLER + DPADHD00 + DPAUTS00 + DDOEDE00 + DPCLSI00 + DPLOLM00 + DPBERE00 + DCSC020A + DPSCHC00 + DPFORC00 + cm_cog_abil + mcs4_edu_achievement + DPBULS00 + DPVIFR00 + mcs4_left_education",
  "sm2_sex", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day*AHCSEX00 + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "sm2_inc", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day*EOEDE000 + AHCSEX00 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "sm2_wlt", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day*mcs5_wealth + AHCSEX00 + EOEDE000 + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00",
  "sm2_mh", 2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day*EEBDTO_T + AHCSEX00 + EOEDE000 + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + mcs5_wealth + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00"
)

# Set up parallel computation
plan(callr, workers = 16)

# Increase allowed size of global objects exported to future workers to 1 GiB
options(future.globals.maxSize = 1073741824)

# Begin pipeline
list(
  # Extract data from HES data files, keeping only relevant variables and filtering out non-psychiatric diagnoses
  tar_map(
    values = HES_filelist,
    names = name,
    tar_target(
      data,
      read_spss(filename) %>% 
        select(vars, contains(groupvars)) %>%
        filter_psychiatric()
    )
  ),
  
  # Extract data from survey data files, keeping only relevant variables and harmonising participant identifier variable IDs
  tar_map(
    values = survey_filelist,
    names = name,
    tar_target(
      data,
      read_dta(filename) %>%
        select(contains("mcsid"), contains("cnum"), all_of(vars)) %>%
        rename(any_of(c(MCSID = "mcsid"))) %>%
        rename(any_of(c(CNUM = contains("cnum"))))
    )
  ),
  
  # Extract data detailing consents to HES linkage, harmonising variable IDs and values (i.e. making CNUM coded 1/2/3 instead of 100/200/300)
  tar_target(
    HES_consents,
    read_spss("data/MCS_Health_Consent_Jan2024_184.sav") %>%
      select(-linkage) %>%
      rename(MCSID = "mcsid", CNUM = "pnum", CONSENT = "consent") %>%
      mutate(CNUM = CNUM/100)
  ),
  
  # Make regional summaries of HES APC & OP-recorded psychiatric episodes
  tar_target(
    rs_HES_APC,
    regional_summary(data_HES_APC)
  ),
  
  tar_target(
    rs_HES_OP,
    regional_summary(data_HES_OP)
  ),
  
  # Derive the dates at which cohort members were interviewed in age 11, 14 and 17 sweeps
  tar_target(
    interview_dates,
    derive_interview_dates(data_mcs5_hhgrid, data_mcs6_hhgrid, data_mcs6_cm_accelerometer_derived, data_mcs7_hhgrid)
  ),
  
  # Derive dates of earliest record of psychiatric diagnosis in each study period from HES APC and OP datasets
  tar_target(
    HES_APC_dates,
    derive_dates(data_HES_APC, "APC", interview_dates)
  ),
  
  tar_target(
    HES_OP_dates,
    derive_dates(data_HES_OP, "OP", interview_dates)
  ),
  
  # Derive date of earliest record of psychiatric diagnosis from either dataset and variables indicating whether there was such a record in each study period
  tar_target(
    HES_dates,
    full_join(HES_APC_dates, HES_OP_dates, by = c("MCSID", "CNUM")) %>%
      mutate(HES_date1 = if(!is.na(APC_date1) | !is.na(OP_date1)){min(APC_date1, OP_date1, na.rm = TRUE)} else {NA},
             HES_ei1 = if_else(is.na(HES_date1), 0, 1),
             HES_date2 = if(!is.na(APC_date2) | !is.na(OP_date2)){min(APC_date2, OP_date2, na.rm = TRUE)} else {NA},
             HES_ei2 = if_else(is.na(HES_date2), 0, 1)) %>%
      select(MCSID, CNUM, HES_date1, HES_ei1, HES_date2, HES_ei2)
  ),
  
  # Clean time use diary to produce derived social media use time variable
  tar_target(
    tud_clean,
    clean_tud(data_mcs6_cm_tud_harmonised)
  ),
  
  # Clean and merge parent interview datasets from all sweeps
  tar_target(
    parent_clean,
    clean_parent(data_mcs1_parent_interview, data_mcs2_parent_interview, data_mcs3_parent_interview, data_mcs4_parent_derived, data_mcs4_parent_interview, data_mcs4_proxy_partner_interview, data_mcs5_parent_derived, data_mcs5_parent_interview, data_mcs5_proxy_partner_interview, data_mcs6_parent_assessment, data_mcs6_parent_interview, data_mcs7_parent_interview)
  ),
  
  # Clean and merge parent-reported cohort member interview datasets from ages 7 and 11 sweeps
  tar_target(
    parent_cm_clean,
    clean_parent_cm(data_mcs4_parent_cm_interview, data_mcs5_parent_cm_interview)
  ),
  
  # Manipulate longitudinal family file to give one row per cohort member who has ever taken part
  tar_target(
    lff_clean,
    data_mcs_longitudinal_family_file %>%
      filter(NOCMHH > 0) %>%
      left_join(data.frame(NOCMHH = c(1, 2, 2, 3, 3, 3), CNUM = c(1, 1, 2, 1, 2, 3)), by = "NOCMHH", relationship = "many-to-many") %>%
      select(-NOCMHH)
  ),
  
  # Remove non-cohort members from age 9 months sweep household grid
  tar_target(
    mcs1_hhgrid_clean,
    data_mcs1_hhgrid %>%
      filter(CNUM > 0)
  ),
  
  # Sum members of household and remove non-cohort members and interview date variables from age 14 sweep household grid 
  tar_target(
    mcs6_hhgrid_clean,
    data_mcs6_hhgrid %>%
      group_by(MCSID) %>%
      mutate(mcs6_residents = sum(FHBCHK00 == 1, na.rm = TRUE)) %>%
      filter(CNUM > 0) %>%
      select(MCSID, CNUM, FHSALIW, mcs6_residents)
  ),

  # Merge datasets
  tar_target(
    initial_data,
    reduce(
      list(
        reduce(
          list(
            data_mcs1_cm_derived,
            mcs1_hhgrid_clean,
            data_mcs2_cm_interview,
            data_mcs3_cm_cognitive_assessment,
            data_mcs3_cm_interview,
            data_mcs4_cm_cognitive_assessment,
            data_mcs4_cm_derived,
            data_mcs4_cm_interview,
            data_mcs5_cm_cognitive_assessment,
            data_mcs5_cm_interview,
            data_mcs5_cm_teacher_survey,
            data_mcs6_cm_cognitive_assessment,
            data_mcs6_cm_derived,
            data_mcs6_cm_interview,
            mcs6_hhgrid_clean,
            data_mcs7_cm_derived,
            data_mcs7_cm_interview,
            HES_dates,
            tud_clean,
            parent_cm_clean,
            lff_clean,
            HES_consents
          ),
          full_join,
          by = c("MCSID", "CNUM")
        ),
        data_mcs1_family_derived,
        data_mcs2_family_derived,
        data_mcs3_family_derived,
        data_mcs4_family_derived,
        data_mcs4_family_interview,
        data_mcs4_geographically_linked_data,
        data_mcs5_family_derived,
        data_mcs5_family_interview,
        data_mcs5_geographically_linked_data,
        data_mcs6_family_derived,
        data_mcs7_family_interview,
        interview_dates,
        parent_clean
      ),
      full_join, 
      by = "MCSID"
    )
  ),
  
  tar_quarto(
    descriptives_report,
    "descriptives_report.qmd"
  ),
  
  # Recode variables as needed
  tar_target(
    data_recoded,
    recode_data(initial_data)
  ),
  
  # Produce a dataset for descriptive analysis
  tar_target(
    descriptive_dataset,
    data_recoded %>% select(MCSID, CNUM, DAREGN00, EAREGN00, ECQ09X00, mcs6_sm_per_day, HES_ei1, HES_ei2, CONSENT, AHCSEX00, DDC06E00, DDOEDE00, EOEDE000, mcs5_wealth)
  ),
  
  # Create dataset for calculating weights by removing unnecessary variables
  tar_target(
    weighting_data,
    data_recoded %>% 
      # Make indicators of whether participants are excluded in each study period and remove unneeded variables
      # Period 1: excluded if didn't consent to HES linkage or if region of residence at age 7 is missing
      mutate(INCLUDED1 = if_else(!CONSENT | is.na(DAREGN00), 0, 1),
      # Period 2: excluded if didn't consent to HES linkage or if region of residence at age 11 is missing
      INCLUDED2 = if_else(!CONSENT | is.na(EAREGN00), 0, 1)) %>%
      select(MCSID, CNUM, INCLUDED1, INCLUDED2, participation, tenure, household_moves, FPWRDSCM, overweight, ADBWGT00, other_consents, WEIGHT1, mcs7_organisations, GCSMOK00, mcs6_crowding, EPVOTE00, GCRLSV00, GCCGHE00, GCSAFF00, GCTRSS00, GCNCLS00, cm_cog_abil, mcs4_edu_achievement, DCSC0012, DDC06E00, DDOEDE00, DDDEBDTOT, DPADHD00, DPAUTS00, DDKESSLER, DDD05S00, darururb, AHCSEX00)
  ),
  
  # Multiple imputation for deriving weights
  tar_target(
    weighting_data_imputed,
    weighting_mi(weighting_data)
  ),
  
  # Derive weights and add to dataset
  tar_target(
    exclusion_weights_raw,
    derive_weights(weighting_data_imputed)
  ),
  
  # Truncate weights distribution
  tar_target(
    exclusion_weights_truncated,
    truncate_weights(exclusion_weights_raw)
  ),
  
  # Produce Quarto report on weights derivation
  tar_quarto(
    weights_derivation,
    "weights_derivation.qmd"
  ),
  
  # Append final weights to dataset
  tar_target(
    data_weights,
    add_weights(data_recoded, exclusion_weights_truncated)
  ),
  
  tar_map(
    values = model_spec_list,
    names = id,
    
    # Perform multiple imputation separately for each substantive model
    tar_target(
      data_imputed_list,
      multiple_imputation(data_weights, modelformula, period, "main")
    ),
    
    tar_target(
      data_imputed,
      concatenate_imputations(data_imputed_list)
    ),
    
    # Standardise exposure variables where possible
    tar_target(
      data_imputed_standardised,
      standardise(data_imputed)
    ),
    
    # Fit unweighted Cox proportional hazards models
    tar_target(
      unweighted_models,
      unweighted_coxph(data_imputed_standardised, modelformula)
    ),
    
    # Summarise model outputs
    tar_target(
      unweighted_model_summary,
      MIcombine(unweighted_models)
    ),
    
    # Fit weighted Cox proportional hazards models
    tar_target(
      weighted_models,
      weighted_coxph(data_imputed_standardised, modelformula)
    ),
    
    # Summarise model outputs
    tar_target(
      weighted_model_summary,
      MIcombine(weighted_models)
    ),
    
    # Multiple imputation for sensitivity analyses
    tar_target(
      sens_data_imputed_list,
      multiple_imputation(data_weights, modelformula, period, "sens")
    ),
    
    tar_target(
      sens_data_imputed,
      concatenate_imputations(sens_data_imputed_list)
    ),
    
    tar_target(
      sens_data_imputed_standardised,
      standardise(sens_data_imputed)
    ),
    
    # Fit unweighted sensitivity analysis models
    tar_target(
      sens_unweighted_models,
      unweighted_coxph(sens_data_imputed_standardised, modelformula)
    ),
    
    # Summarise model outputs
    tar_target(
      sens_unweighted_model_summary,
      MIcombine(sens_unweighted_models)
    ),
    
    # Fit weighted sensitivity analysis models
    tar_target(
      sens_weighted_models,
      weighted_coxph(sens_data_imputed_standardised, modelformula)
    ),
    
    # Summarise model outputs
    tar_target(
      sens_weighted_model_summary,
      MIcombine(sens_weighted_models)
    )
  ),
  
  # Create dataset with categorical social media use variables
  tar_target(
    data_categorical,
    categorise_smu(data_weights)
  ),
  
  tar_target(
    data_categorical_1h,
    categorise_smu2(data_weights, 60)
  ),
  
  tar_target(
    data_categorical_3h,
    categorise_smu2(data_weights, 180)
  ),
  
  tar_target(
    data_categorical_4h,
    categorise_smu2(data_weights, 240)
  ),
  
  tar_map(
    values = cat_model_spec_list,
    names = id,
    
    # Imputations and models for categorical data
    tar_target(
      cat_data_imputed_list,
      multiple_imputation(data_categorical, modelformula, period, "main")
    ),
    
    tar_target(
      cat_data_imputed,
      concatenate_imputations(cat_data_imputed_list)
    ),
    
    tar_target(
      cat_data_imputed_standardised,
      standardise(cat_data_imputed, include_sm = FALSE)
    ),
    
    tar_target(
      cat_weighted_models,
      weighted_coxph(cat_data_imputed_standardised, modelformula)
    ),
    
    tar_target(
      cat_weighted_model_summary,
      MIcombine(cat_weighted_models)
    )
  ),
  
  # Categorised with other threshold values of "high" SMU at age 14
  tar_target(
    cat1h_data_imputed_list_sm2,
    multiple_imputation(data_categorical_1h, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00", 2, "main")
  ),
  
  tar_target(
    cat1h_data_imputed_sm2,
    concatenate_imputations(cat1h_data_imputed_list_sm2)
  ),
  
  tar_target(
    cat1h_data_imputed_standardised_sm2,
    standardise(cat1h_data_imputed_sm2, include_sm = FALSE)
  ),
  
  tar_target(
    cat1h_weighted_models_sm2,
    weighted_coxph(cat1h_data_imputed_standardised_sm2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00")
  ),
  
  tar_target(
    cat1h_weighted_model_summary_sm2,
    MIcombine(cat1h_weighted_models_sm2)
  ),
  
  tar_target(
    cat3h_data_imputed_list_sm2,
    multiple_imputation(data_categorical_3h, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00", 2, "main")
  ),
  
  tar_target(
    cat3h_data_imputed_sm2,
    concatenate_imputations(cat3h_data_imputed_list_sm2)
  ),
  
  tar_target(
    cat3h_data_imputed_standardised_sm2,
    standardise(cat3h_data_imputed_sm2, include_sm = FALSE)
  ),
  
  tar_target(
    cat3h_weighted_models_sm2,
    weighted_coxph(cat3h_data_imputed_standardised_sm2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00")
  ),
  
  tar_target(
    cat3h_weighted_model_summary_sm2,
    MIcombine(cat3h_weighted_models_sm2)
  ),
  
  tar_target(
    cat4h_data_imputed_list_sm2,
    multiple_imputation(data_categorical_4h, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00", 2, "main")
  ),
  
  tar_target(
    cat4h_data_imputed_sm2,
    concatenate_imputations(cat4h_data_imputed_list_sm2)
  ),
  
  tar_target(
    cat4h_data_imputed_standardised_sm2,
    standardise(cat4h_data_imputed_sm2, include_sm = FALSE)
  ),
  
  tar_target(
    cat4h_weighted_models_sm2,
    weighted_coxph(cat4h_data_imputed_standardised_sm2, "Surv(HES_time2, HES_ei2) ~ mcs6_sm_per_day + EOEDE000 + mcs5_wealth + ED05S00 + DDC06E00 + EAREGN00 + EIMDSCOE + EARURURB + AHCSEX00 + ECQ75X00 + mcs5_kessler + ECQ10B00 + EPADHD00 + EPAUTS00 + HES_ei1 + EEBDTO_T + EPCLSI00 + EPLOLM00 + EPBERE00 + ECQ80X0A + EPSCHC00 + EPFORC00 + cm_cog_abil + mcs5_edu_achievement + mcs5_antisocial_beh + EPSDPB00 + EPVIFR00 + mcs5_left_education + ECQ53A00 + ECQ53B00")
  ),
  
  tar_target(
    cat4h_weighted_model_summary_sm2,
    MIcombine(cat4h_weighted_models_sm2)
  ),
  
  tar_map(
    values = sm1_adj_model_spec_list,
    names = id,
    
    # Models with differential adjustment (period 1)
    tar_target(
      adj_weighted_models_sm1,
      weighted_coxph(data_imputed_standardised_sm1, modelformula)
    ),
    
    tar_target(
      adj_weighted_model_summary_sm1,
      MIcombine(adj_weighted_models_sm1)
    ),
    
    tar_target(
      cat_adj_weighted_models_sm1,
      weighted_coxph(cat_data_imputed_standardised_sm1, modelformula)
    ),
    
    tar_target(
      cat_adj_weighted_model_summary_sm1,
      MIcombine(cat_adj_weighted_models_sm1)
    )
  ),
  
  tar_map(
    values = sm2_adj_model_spec_list,
    names = id,
    
    # Models with differential adjustment (period 2)
    tar_target(
      adj_weighted_models_sm2,
      weighted_coxph(data_imputed_standardised_sm2, modelformula)
    ),
    
    tar_target(
      adj_weighted_model_summary_sm2,
      MIcombine(adj_weighted_models_sm2)
    ),
    
    tar_target(
      cat_adj_weighted_models_sm2,
      weighted_coxph(cat_data_imputed_standardised_sm2, modelformula)
    ),
    
    tar_target(
      cat_adj_weighted_model_summary_sm2,
      MIcombine(cat_adj_weighted_models_sm2)
    )
  ),
  
  tar_map(
    values = interax_model_spec_list,
    names = id,
    
    # Imputations and models for interactions
    tar_target(
      interax_data_imputed_list,
      multiple_imputation(data_weights, modelformula, period, "main")
    ),
    
    tar_target(
      interax_data_imputed,
      concatenate_imputations(interax_data_imputed_list)
    ),
    
    tar_target(
      interax_data_imputed_standardised,
      standardise(interax_data_imputed)
    ),
    
    tar_target(
      interax_weighted_models,
      weighted_coxph(interax_data_imputed_standardised, modelformula)
    ),
    
    tar_target(
      interax_weighted_model_summary,
      MIcombine(interax_weighted_models)
    )
  ),
  
  tar_map(
    values = cat_interax_model_spec_list,
    names = id,
    
    # Interactions for categorical data
    tar_target(
      cat_interax_data_imputed_list,
      multiple_imputation(data_categorical, modelformula, period, "main")
    ),
    
    tar_target(
      cat_interax_data_imputed,
      concatenate_imputations(cat_interax_data_imputed_list)
    ),
    
    tar_target(
      cat_interax_data_imputed_standardised,
      standardise(cat_interax_data_imputed, include_sm = FALSE)
    ),
    
    tar_target(
      cat_interax_weighted_models,
      weighted_coxph(cat_interax_data_imputed_standardised, modelformula)
    ),
    
    tar_target(
      cat_interax_weighted_model_summary,
      MIcombine(cat_interax_weighted_models)
    )
  ),
  
  # Produce Quarto report on multiple imputation
  tar_quarto(
    multiple_imputation_report,
    "multiple_imputation.qmd"
  ),
  
  tar_quarto(
    models_report,
    "model_outputs.qmd"
  )
)
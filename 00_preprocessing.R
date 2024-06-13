
# ----------------------------------------------------------
# R Script for Data Preprocessing
# Author: Clievins Selva
# Date: [20.09.2023]
# ----------------------------------------------------------

# Outline:
# 1. Loading Packages
# 2. Setting Working Directory
# 3. Data Import
# 4. Data Processing
#    - Missing Value Handling
#    - Duplicate Handling
#    - Variable Renaming
#    - Reverse Coding
# 5. Descriptive Statistics
# 6. Data Export
# 7. Final Remarks
# ----------------------------------------------------------

# packages
library(openxlsx)    
library(tidyverse)
library(writexl)
library(psych)
library(rstudioapi)


# setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


crdata_h <- read.xlsx(xlsxFile = "input/preprocessing/data_copingundresilienz_2023-04-04_13-54.xlsx",
                       skipEmptyRows = FALSE)%>% 
  slice(-1)

crdata_v <- read.xlsx(xlsxFile = "input/preprocessing/data_cselva_rc_2023-08-14_14-07.xlsx",
                       skipEmptyRows = FALSE)%>% 
  slice(-1)

descriptives <- describe(crdata_h)



crdata_h <- crdata_h[rowSums(is.na(crdata_h)) <= 300, ] #delets rows were half variables are NA
crdata_v <- crdata_v[rowSums(is.na(crdata_v)) <= 300, ] #delets rows were half variables are NA


# Convert character columns starting with "RC" in crdata_h to numeric
crdata_h[grep("^RC", names(crdata_h))] <- lapply(crdata_h[grep("^RC", names(crdata_h))], as.numeric)

# Convert character columns starting with "RC" in crdata_v to numeric
crdata_v[grep("^RC", names(crdata_v))] <- lapply(crdata_v[grep("^RC", names(crdata_v))], as.numeric)


############Explanation of missing varaibles##########

# Extract variable names from each dataset
variables_crdata_h <- names(crdata_h)
variables_crdata_v <- names(crdata_v)

# Find variable names that are in crdata_h but not in crdata_v
missing_in_v <- setdiff(variables_crdata_h, variables_crdata_v)

# Find variable names that are in crdata_v but not in crdata_h
missing_in_h <- setdiff(variables_crdata_v, variables_crdata_h)

# Print results
print(missing_in_v)
print(missing_in_h)

' 
#Missings in v:
Regarding duplicates:
"RC10_37"  "RC10_39"  "RC10_42"  "RC10_43"  "RC10_45"  "RC10_47"  "RC10_53"  "RC10_54"  "RC10_55"  "RC10_58"  "RC10_60" 

degtime (Maluspunkte für schnelles Ausfüllen), somehow not present
"DEG_TIME"



#Missings in h:
Regarding participant results (were not implemented in crdata_h)
"BG07"      	"BG06"
FilterDSGVO	Filterseite

Ragarding that none responsed to this filter variable
"SD04_06"
Familienstand: Sonstiges

Regarding page time on page 3 and page 29. (changes duo to small questionnaire adaption)
"TIME003"  "TIME029""
'



############hannah data reversing ##############
# RSA and BFI are already reversed in questionnaire for V data and H data.

# further reversing is done below in the combined df

############Documentation of duplicates and mean correction##############
# specifying function that treats dublicates
combine_and_clean <- function(data, var1, var2) {
  # Check if both variables exist in the dataset
  if (var1 %in% names(data) & var2 %in% names(data)) {
    
    # Calculate combined value based on conditions
    combined_value <- ifelse(is.na(data[[var1]]) | data[[var1]] < 0, data[[var2]], 
                             ifelse(is.na(data[[var2]]) | data[[var2]] < 0, data[[var1]], 
                                    (data[[var1]] + data[[var2]]) / 2))
    
    # Round the combined values
    combined_value <- round(combined_value)
    
    # Ensure values do not exceed the upper bound of 7
    data[[var1]] <- pmin(combined_value, 7)
    
    # Drop the second variable
    data[[var2]] <- NULL
  }
  return(data)
}


print(class(crdata_h$RC10_07))
print(class(crdata_h$RC10_37))

# Apply function
crdata_h <- combine_and_clean(crdata_h, "RC10_07", "RC10_37")
crdata_h <- combine_and_clean(crdata_h, "RC10_09", "RC10_39")
crdata_h <- combine_and_clean(crdata_h, "RC10_12", "RC10_42")
crdata_h <- combine_and_clean(crdata_h, "RC10_13", "RC10_43")
crdata_h <- combine_and_clean(crdata_h, "RC10_15", "RC10_45")
crdata_h <- combine_and_clean(crdata_h, "RC10_17", "RC10_47")
crdata_h <- combine_and_clean(crdata_h, "RC10_24", "RC10_54")
crdata_h <- combine_and_clean(crdata_h, "RC10_25", "RC10_55")
crdata_h <- combine_and_clean(crdata_h, "RC10_28", "RC10_58")
crdata_h <- combine_and_clean(crdata_h, "RC10_30", "RC10_60")


# Duplicates "RC10_23", "RC10_53" are awarenesscheck items and treated differently than the other dublicates:
# Update RC10_23 to 4 if either RC10_23 or RC10_53 has a value of 4
crdata_h$RC10_23[crdata_h$RC10_23 == 4 | crdata_h$RC10_53 == 4] <- 4
crdata_h$RC10_53 <- NULL #Remove the RC10_53 column


#drop further variables that are not necessary 
crdata_h[["DEG_TIME"]] <- NULL

crdata_v$BG07 <- NULL
crdata_v$BG06 <- NULL
crdata_v$SD04_06 <- NULL
crdata_v$TIME003 <- NULL
crdata_v$TIME029 <- NULL


############combine dataset##########
crdata <- rbind(crdata_h, crdata_v)


############match with codebook##########
codebook <- read.xlsx(xlsxFile = "input/2024-01-05_Rc_items.xlsx")

# Create the list with "oldnames" and "newnames"
rename_list <- list(
  oldnames = codebook$item_name_soscisurvey,
  newnames = codebook$item_name_auswertung
)

# Identify matching positions
matched_positions <- match(rename_list$oldnames, names(crdata))

# Identify valid positions (excluding NAs)
valid_positions <- which(!is.na(matched_positions))

# Rename the valid columns
names(crdata)[matched_positions[valid_positions]] <- rename_list$newnames[valid_positions]
names(crdata)

############apply duplicate function to _dopp items##########
crdata <- combine_and_clean(crdata, "akzep_2_018", "akzep_2_018_dopp")
crdata <- combine_and_clean(crdata, "akzep_3_020", "akzep_3_020_dopp")
crdata <- combine_and_clean(crdata, "ablen_2_062_r", "ablen_2_062_r_dopp")
crdata <- combine_and_clean(crdata, "relig_2_122_r", "relig_2_122_r_dopp")
crdata <- combine_and_clean(crdata, "planu_1_094_r", "planu_1_094_r_dopp")
crdata <- combine_and_clean(crdata, "planu_1_095", "planu_1_095_dopp")
crdata <- combine_and_clean(crdata, "vorbi_1_230", "vorbi_1_230_dopp")
crdata <- combine_and_clean(crdata, "aktiv_1_177", "aktiv_1_177_dopp")
crdata <- combine_and_clean(crdata, "behav_1_132_r", "behav_1_132_r_dopp")
crdata <- combine_and_clean(crdata, "wunsc_1_151_r", "wunsc_1_151_r_dopp")
crdata <- combine_and_clean(crdata, "umgan_1_222", "umgan_1_222_dopp")
crdata <- combine_and_clean(crdata, "relig_1_116", "relig_1_116_dopp")
crdata <- combine_and_clean(crdata, "moral_1_240", "moral_1_240_dopp")
crdata <- combine_and_clean(crdata, "umgan_1_223", "umgan_1_223_dopp")

############apply awarenesschecks##########

crdata <- crdata %>%
  filter(VE02 == 1,
         aufm_check_1 == 7,
         aufm_check_2 == 1,
         aufm_check_3 == 4)


crdata$VE02 <- NULL
crdata$aufm_check_1 <- NULL
crdata$aufm_check_2 <- NULL
crdata$aufm_check_3 <- NULL




descriptives <- describe(crdata)

############reverse rc items##########
# Identify columns to reverse, trimming whitespace and converting to lowercase for comparison
cols_to_reverse <- codebook$item_name_auswertung[trimws(tolower(codebook$zu_rekodieren)) == "x"]

# Reverse values for identified columns
for (col in cols_to_reverse) {
  if (col %in% names(crdata)) {
    # Only apply transformation to non-negative values and not NA values
    positive_indices <- which(crdata[[col]] >= 0 & !is.na(crdata[[col]]))
    crdata[positive_indices, col] <- 8 - crdata[positive_indices, col]
  }
}


############reverse remaining scale items##########
descriptives <- describe(crdata)
# is done below




############rename demografic variables##########

#First, adjust the values for SD15 (Soziooekonomischen_Status) in the data frame. -> here something went wrong 
#with the specification
# Adjusting values for SD15
crdata$SD15[crdata$SD15 == 7] <- 2
crdata$SD15[crdata$SD15 == 2] <- 3
crdata$SD15[crdata$SD15 == 3] <- 4
crdata$SD15[crdata$SD15 == 4] <- 5
crdata$SD15[crdata$SD15 == 6] <- -1



# Remove variables VA06_01 through VA06_09 from crdata
crdata <- crdata[, !(names(crdata) %in% paste0("VA06_0", 1:9))] #RC items has 11 items. The additional 9 items are MCCS items (dublicates by mistake)

# Renaming columns
crdata <- crdata %>%
  rename(
    age = SD12_01,
    gender = SD02,
    Bildungsabschluss = SD03,
    Familienstand = SD04,
    Taetigkeit = SD05,
    Semester = SD06,
    Studienrichtung = SD07,
    Alleinlebend = SD08,
    Bewohner_Haushalt = SD09,
    Ehrenamt = SD10,
    Religiositaet = SD11,
    Psychotherapeuten_Unterstuetzung = SD13,
    Filter_Psycht = SD14,
    Soziooekonomischen_Status = SD15
  )


  # Renaming columns
  crdata_as_character <- crdata %>%
  # Recoding values
  mutate(
    gender = case_when(
      gender == 1 ~ "Männlich",
      gender == 2 ~ "Weiblich",
      gender == 3 ~ "Divers",
      gender == -1 ~ "Keine Angabe",
      gender == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(gender)
    ),
    Bildungsabschluss = case_when(
      Bildungsabschluss == 1 ~ "kein Bildungsabschluss",
      Bildungsabschluss == 2 ~ "Hauptschulabschluss",
      Bildungsabschluss == 3 ~ "Mittlere Reife",
      Bildungsabschluss == 4 ~ "Fachabitur",
      Bildungsabschluss == 5 ~ "Abgeschlossene Ausbildung",
      Bildungsabschluss == 6 ~ "Abitur",
      Bildungsabschluss == 7 ~ "Bachelor",
      Bildungsabschluss == 8 ~ "Master/Diplom",
      Bildungsabschluss == 9 ~ "Magister",
      Bildungsabschluss == 10 ~ "Promotion",
      Bildungsabschluss == 11 ~ "Sonstiges",
      Bildungsabschluss == -1 ~ "Keine Angabe",
      Bildungsabschluss == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Bildungsabschluss)
    ),
    Familienstand = case_when(
      Familienstand == 1 ~ "Ledig",
      Familienstand == 2 ~ "In einer festen Beziehung",
      Familienstand == 3 ~ "Verheiratet",
      Familienstand == 4 ~ "Geschieden",
      Familienstand == 5 ~ "Verwitwet",
      Familienstand == 6 ~ "Sonstiges",
      Familienstand == -1 ~ "Keine Angabe",
      Familienstand == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Familienstand)
    ),
    Taetigkeit = case_when(
      Taetigkeit == 1 ~ "Vollzeit erwerbstaetig",
      Taetigkeit == 2 ~ "Teilzeit erwerbstaetig",
      Taetigkeit == 3 ~ "Minijob (nicht zusaetzlich zum Studium oder Ausbildung)",
      Taetigkeit == 4 ~ "Auszubildende:r",
      Taetigkeit == 5 ~ "Student:in",
      Taetigkeit == 6 ~ "Arbeitssuchend",
      Taetigkeit == 7 ~ "Rentner:in",
      Taetigkeit == 8 ~ "Sonstiges",
      Taetigkeit == -1 ~ "Keine Angabe",
      Taetigkeit == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Taetigkeit)
    ),
    Semester = case_when(
      Semester == 1 ~ "1. Semester",
      Semester == 2 ~ "2. Semester",
      Semester == 3 ~ "3. Semester",
      Semester == 4 ~ "4. Semester",
      Semester == 5 ~ "5. Semester",
      Semester == 6 ~ "6. Semester",
      Semester == 7 ~ "7. Semester",
      Semester == 8 ~ "8. oder hoeheres Semester",
      Semester == -1 ~ "Keine Angabe",
      Semester == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Semester)
    ),
    Studienrichtung = case_when(
      Studienrichtung == 1 ~ "Geisteswissenschaften (z.B.: Germanistik...)",
      Studienrichtung == 2 ~ "Kunst (z.B.: Architektur, Design, Musik...)",
      Studienrichtung == 3 ~ "Recht (z.B.: Jura...)",
      Studienrichtung == 4 ~ "Sozialwissenschaften (z.B.: Soziale Arbeit, Soziologie...)",
      Studienrichtung == 5 ~ "Humanwissenschaften (z.B.: Wirtschafts-/Psychologie, Paedagogik)",
      Studienrichtung == 6 ~ "Medizin (z.B.: Zahnmedizin, Humanmedizin)",
      Studienrichtung == 7 ~ "Technik (z.B.: Informatik, Maschinenbau, Sicherheitstechnik...)",
      Studienrichtung == 8 ~ "Wirtschaft (z.B.: BWL, Immobilienmanagement, Wirtschaftswissenschaften...)",
      Studienrichtung == -1 ~ "Keine Angabe",
      Studienrichtung == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Studienrichtung)
    ),
    Alleinlebend = case_when(
      Alleinlebend == 1 ~ "Nein",
      Alleinlebend == 2 ~ "Ja",
      Alleinlebend == -1 ~ "Keine Angabe",
      Alleinlebend == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Alleinlebend)
    ),
    Bewohner_Haushalt = case_when(
      Bewohner_Haushalt == 1 ~ "2",
      Bewohner_Haushalt == 2 ~ "3",
      Bewohner_Haushalt == 3 ~ "4",
      Bewohner_Haushalt == 4 ~ "5",
      Bewohner_Haushalt == 5 ~ "6",
      Bewohner_Haushalt == 6 ~ "7",
      Bewohner_Haushalt == 7 ~ "8",
      Bewohner_Haushalt == 8 ~ "mehr als 8 Bewohner*innen",
      Bewohner_Haushalt == -1 ~ "Keine Angabe",
      Bewohner_Haushalt == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Bewohner_Haushalt)
    ),
    Ehrenamt = case_when(
      Ehrenamt == 1 ~ "0-mal",
      Ehrenamt == 2 ~ "1-mal",
      Ehrenamt == 3 ~ "2-mal",
      Ehrenamt == 4 ~ "3-mal",
      Ehrenamt == 5 ~ "4-mal",
      Ehrenamt == 6 ~ "5-mal",
      Ehrenamt == 7 ~ "6-mal",
      Ehrenamt == 8 ~ "mehr als 6-mal",
      Ehrenamt == -1 ~ "Keine Angabe",
      Ehrenamt == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Ehrenamt)
    ),
    Religiositaet = case_when(
      Religiositaet == 1 ~ "0-mal",
      Religiositaet == 2 ~ "1-mal",
      Religiositaet == 3 ~ "2-mal",
      Religiositaet == 4 ~ "3-mal",
      Religiositaet == 5 ~ "4-mal",
      Religiositaet == 6 ~ "5-mal",
      Religiositaet == 7 ~ "6-mal",
      Religiositaet == 8 ~ "mehr als 6-mal",
      Religiositaet == -1 ~ "Keine Angabe",
      Religiositaet == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Religiositaet)
    ),
    Psychotherapeuten_Unterstuetzung = case_when(
      Psychotherapeuten_Unterstuetzung == 1 ~ "Ja, ich habe mir schonmal psychotherapeutische Unterstützung gesucht.",
      Psychotherapeuten_Unterstuetzung == 2 ~ "Ja, ich habe schonmal darüber nachgedacht, mir psychotherapeutische Unterstützung zu suchen.",
      Psychotherapeuten_Unterstuetzung == 3 ~ "Nein, ich habe noch nicht darüber nachgedacht und hatte noch keine psychotherapeutische Unterstützung.",
      Psychotherapeuten_Unterstuetzung == -1 ~ "Keine Angabe",
      Psychotherapeuten_Unterstuetzung == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Psychotherapeuten_Unterstuetzung)
    ),
    Filter_Psycht = case_when(
      Filter_Psycht == 1 ~ "Aengste, Sorgen",
      Filter_Psycht == 2 ~ "Depressionen/Burnout",
      Filter_Psycht == 3 ~ "Sonstiges",
      Filter_Psycht == -1 ~ "Keine Angabe",
      Filter_Psycht == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Filter_Psycht)
    ),
    Soziooekonomischen_Status = case_when(
      Soziooekonomischen_Status == 1 ~ "Unterschicht",
      Soziooekonomischen_Status == 2 ~ "Arbeiterschicht",
      Soziooekonomischen_Status == 3 ~ "Mittelschicht",
      Soziooekonomischen_Status == 4 ~ "Obere Mittelschicht",
      Soziooekonomischen_Status == 5 ~ "Oberschicht",
      Soziooekonomischen_Status == -1 ~ "Keine Angabe",
      Soziooekonomischen_Status == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Soziooekonomischen_Status)
    )
  )
  
  
############outlier detection##########
threshold_missing = 0.25
num_columns = ncol(crdata)
num_missing = rowSums(is.na(crdata))
incomplete_responses = crdata[num_missing > threshold_missing * num_columns, ]


threshold_negative = 0.25
num_negative = rowSums(crdata < 0, na.rm = TRUE)
negative_responses = crdata[num_negative > threshold_negative * num_columns, ]

# Get indices of rows to remove
incomplete_indices <- which(rownames(crdata) %in% rownames(incomplete_responses))
negative_indices <- which(rownames(crdata) %in% rownames(negative_responses))

# Combine indices
all_indices_to_remove <- unique(c(incomplete_indices, negative_indices))

# Remove rows
crdata <- crdata[-all_indices_to_remove, ]



############rename and reverse full data##########
# Convert variables starting with 'VA05_' to numeric
va05_cols <- grep("^VA05_", colnames(crdata), value = TRUE)
crdata[va05_cols] <- lapply(crdata[va05_cols], function(col) {
  as.numeric(as.character(col))
})

# Convert variables starting with 'RA' to numeric
ra_cols <- grep("^RA", colnames(crdata), value = TRUE)
crdata[ra_cols] <- lapply(crdata[ra_cols], function(col) {
  as.numeric(as.character(col))
})

  
# Named vector for RA variables
ra_old_names <- c("RA01_01", "RA02_01", "RA03_01", "RA04_01", "RA05_01", "RA06_01", "RA07_01", "RA08_01", "RA09_01", "RA10_01",
                    "RA11_01", "RA12_01", "RA13_01", "RA14_01", "RA15_01", "RA16_01", "RA17_01", "RA18_01", "RA19_01", "RA20_01",
                    "RA21_01", "RA22_01", "RA23_01", "RA24_01", "RA25_01", "RA26_01", "RA27_01", "RA28_01", "RA29_01", "RA30_01",
                    "RA31_01", "RA32_01", "RA33_01")

ra_new_names <- c("rsa_1_01", "rsa_2_02", "rsa_3_03_r", "rsa_5_04", "rsa_6_05", "rsa_4_06_r", "rsa_1_07_r", "rsa_2_08_r", "rsa_3_09",
                  "rsa_5_10_r", "rsa_6_11_r", "rsa_4_12", "rsa_1_13", "rsa_2_14_r", "rsa_3_15_r", "rsa_5_16", "rsa_6_17", "rsa_4_18_r",
                  "rsa_1_19_r", "rsa_2_20", "rsa_3_21", "rsa_5_22_r", "rsa_6_23_r", "rsa_4_24", "rsa_1_25", "rsa_3_26_r", "rsa_5_27",
                  "rsa_6_28_r", "rsa_1_29_r", "rsa_3_30", "rsa_5_31_r", "rsa_6_32", "rsa_6_33_r")

# Named vector for VA05 variables
va05_old_names <- c("VA05_01", "VA05_02", "VA05_03", "VA05_04", "VA05_05", "VA05_06", "VA05_07", "VA05_08", "VA05_09", "VA05_10",
                    "VA05_11", "VA05_12", "VA05_13", "VA05_14", "VA05_15", "VA05_16", "VA05_17", "VA05_18", "VA05_19", "VA05_20",
                    "VA05_21", "VA05_22", "VA05_23", "VA05_24", "VA05_25", "VA05_26", "VA05_27", "VA05_28")

va05_new_names <- c("cope_01_01", "cope_02_02", "cope_03_03_r", "cope_04_04_r", "cope_05_05", "cope_07_06_r", "cope_02_07", "cope_03_08_r",
                    "cope_08_09", "cope_06_10", "cope_04_11_r", "cope_09_12", "cope_14_13_r", "cope_10_14", "cope_05_15", "cope_07_16_r",
                    "cope_09_17", "cope_11_18", "cope_01_19", "cope_12_20", "cope_08_21", "cope_13_22", "cope_06_23", "cope_12_24",
                    "cope_10_25", "cope_14_26_r", "cope_13_27", "cope_11_28")

# Rename columns in crdata
colnames(crdata)[colnames(crdata) %in% ra_old_names] <- ra_new_names
colnames(crdata)[colnames(crdata) %in% va05_old_names] <- va05_new_names

# recode items starting with "cope_/rsa_" and ending with "_r"
#Here i again checked that cope items are correctly reversed. I was afraid that in 
# on of the projekts the reversing was done in questionnaire. As the alpha
# function detectes negative correlations with the total scale. So I inspected v and h 
# data. I check both questionnaires in Sosci, i checked the values. it appears to be all correct. 
# that individual facets are uncorrelated with the total scale could be an 
# attribute of the scale. 



# Identify columns to reverse for the 4-point scale (cope_)
cope_cols_to_reverse <- grep("^cope_.*_r$", names(crdata), value = TRUE)

# Reverse values for identified columns on a 4-point scale
for (col in cope_cols_to_reverse) {
  # Only apply transformation to non-negative values and not NA values
  positive_indices <- which(crdata[[col]] >= 0 & !is.na(crdata[[col]]))
  crdata[positive_indices, col] <- 5 - crdata[positive_indices, col]
}






'
#RSA is already reversed in questionnaire!
# BFI is already revsed in questionnaire!

# -> FFA ,gpsk (both needs reverse). #complete the code later

4 point
ffa_1_13_r		VA07_13

4 point
gpsk_1_5_r		VA09_05
gpsk_1_6_r		VA09_06
gpsk_1_7_r		VA09_07
gpsk_1_9_r		VA09_09
'


# Identify columns to reverse for the 4-point scale ()
cols_to_reverse <- c("VA07_13","VA09_05","VA09_06","VA09_07", "VA09_09")


# Convert specified columns to numeric using base R functions
crdata[cols_to_reverse] <- lapply(crdata[cols_to_reverse], as.numeric)


# Reverse values for identified columns on a 4-point scale
for (col in cols_to_reverse) {
  # Only apply transformation to non-negative values and not NA values
  positive_indices <- which(crdata[[col]] >= 0 & !is.na(crdata[[col]]))
  crdata[positive_indices, col] <- 5 - crdata[positive_indices, col]
}




#Check for reversing. In study 1, only BFI and RSA do not recoding, as this is done in sosci. Study 2
#wont need any recoding as everything will be done in sosci. 

############export datasets##########

rcitems <- crdata[, names(crdata) %in% rename_list$newnames]
write_xlsx(crdata, "input/rcall.xlsx")
write_xlsx(rcitems, "input/rcitems.xlsx")
write_xlsx(crdata_as_character, "input/rcitems_as_character.xlsx")





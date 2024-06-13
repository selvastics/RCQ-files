############notes ##########
#Note that different data contributions have different preprocessing. 
#--> NEAT data is reversed in questionnaire.

#-> datanew has: non reversed RC items. 

# NOTE FACTORS: 
#Codebook and Klauer scheine:
#Kognitive und emotionale Flexibilität (F22): ist  "Kognitive Rekonstruierung"
#Soziale Kompetenz (F26): Als subfacette in Soziale Unterstützung                

# ------------------------------------------------------------------------------
# packages
# ------------------------------------------------------------------------------
library(mirt)         # Multidimensional Item Response Theory (MIRT) analysis
library(rstudioapi)   # Sets directory and some more
library(lattice)      # Trellis graphics for R, used for plotting grid graphics
library(stats4)       # Provides basic statistical functions
library(tidyverse)    # A collection of packages for data manipulation and visualization
library(openxlsx)     # Reads and writes Excel files without Java dependency
library(readxl)       # Reads Excel files
library(psych)        # Toolbox for psychometric analysis and data visualization
library(lavaan)       # Latent variable analysis, including SEM (Structural Equation Modeling)
library(pheatmap)
library(EGAnet)     # For exploratory graph analysis
library(apaTables) 

# ------------------------------------------------------------------------------
# data input
# ------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
# olddata (study 1)
data <- read.xlsx(xlsxFile = "input/rcall.xlsx",
                     skipEmptyRows = FALSE)
datanew1 <- read.xlsx(xlsxFile = "input/newsoscies/data_RC-IRT-cs_2024-05-21_16-06.xlsx",
                      skipEmptyRows = FALSE)
datanew2 <- read.xlsx(xlsxFile = "input/newsoscies/data_cselva_rc02_2024-03-22_11-33.xlsx",
                      skipEmptyRows = FALSE)

codebook <- read.xlsx(xlsxFile = "input/2024-01-05_Rc_items.xlsx")

# ------------------------------------------------------------------------------
# preprocess newdata
# ------------------------------------------------------------------------------

#preprocess datanew2
datanew2 <- datanew2[-1, ]
columns_to_replace <- grep("^RE0", colnames(datanew2), value = TRUE) # Identify columns starting with "RE0"
new_column_names <- sub("^RE0", "RF0", columns_to_replace) # Replace "RE0" with "RF0" in the identified columns
colnames(datanew2)[colnames(datanew2) %in% columns_to_replace] <- new_column_names # Rename the columns in datanew2


## combine ###
# Find matching columns
matching_columns <- intersect(colnames(datanew1), colnames(datanew2))
# setdiff controlled: valid matching
# Combine only the matching columns from datanew1 and datanew2
datanew <- rbind(datanew1[, matching_columns], datanew2[, matching_columns])
datanew$CASE <- seq(1000, length.out = nrow(datanew)) # assign new case no
glimpse(datanew)





first_row <- datanew[1, ] # Preserving the first row
filtered_datanew <- datanew %>% #Filtering rows based on 'QUESTNNR'
  filter(grepl("CR01_M03", QUESTNNR) | grepl("CR01_2", QUESTNNR) | row_number() == 1)
final_datanew <- rbind(first_row, filtered_datanew[-1, ]) # Ensuring the first row is always included
final_datanew <- final_datanew %>% # Removing columns with only NAs
  select_if(~any(!is.na(.)))
subset_datanew <- final_datanew %>% #Creating a subset with items starting with 'RF' and 'SD'
  select(matches("^RF"), matches("^SD"),  matches("QUESTNNR"), matches("VE02"),matches("CASE"))
print(subset_datanew)

first_row <- first_row %>%
  select(matches("^RF0"))




# Drop the First 6 Letters in Every Cell of 'first_row'
first_row_transformed <- apply(first_row, 2, function(x) substr(x, 7, nchar(x)))
first_row_transformed <- as.data.frame(first_row_transformed, stringsAsFactors = FALSE) # Convert back to a dataframe (if needed)
first_row_transformed <- rownames_to_column(first_row_transformed, var = "variable_name") # Convert row names of 'first_row_transformed' to a new column
str(first_row_transformed)
rm(first_row)

# Assuming 'first_row_transformed' and 'codebook' are already defined
# Select only the relevant columns from 'codebook'
codebook_subset <- codebook[, c("item_beschriftung_soscisurvey", "zu_rekodieren", "item_name_auswertung")]
# Merging 'first_row_transformed' with the subset of 'codebook'
merged_df <- merge(first_row_transformed, codebook_subset, by.x = "first_row_transformed", by.y = "item_beschriftung_soscisurvey", all.x = TRUE)
rm(first_row_transformed)

# Prepare old and new names for renaming
oldnames <- merged_df$variable_name
newnames <- merged_df$item_name_auswertung
names_vector <- setNames(newnames, oldnames) # Create a named vector where names are oldnames and values are newnames
# Loop over each column in 'subset_datanew'
for (col in names(subset_datanew)) {
  # Check if the column name is in 'oldnames'
  if (col %in% oldnames) {
    # Rename the column to its corresponding 'newname'
    names(subset_datanew)[names(subset_datanew) == col] <- names_vector[col]
  }
}
print(names(subset_datanew)) # View the renamed columns in 'subset_datanew'

demovariables <- c("SD12_01", "SD02","SD03","SD03_11","SD04" ,"SD04_06","SD05" ,
                   "SD05_08", "SD06" ,"SD07","SD08","SD09","SD10" ,"SD11",
                   "SD13","SD14","SD14_03","SD15","QUESTNNR","VE02" ,"CASE")        
demonames <- as.data.frame(head(subset_datanew[demovariables], 1))
# Rename the 38th and 64th columns to 'check1' and 'check2'
if (ncol(subset_datanew) >= 64) {
  # Rename the 38th and 64th columns to 'check1' and 'check2'
  names(subset_datanew)[38] <- "check1"
  names(subset_datanew)[64] <- "check2"
} 
names(subset_datanew)

# Filter rows based on specified conditions
filtered_datanew <- subset_datanew[
  subset_datanew$QUESTNNR == "CR01_M03" & 
    subset_datanew$VE02 == "1" &
    subset_datanew$check1 == "7" &
    subset_datanew$check2 == "1", 
]

# Drop the entire rows where any NA value is present in the specified columns
filtered_datanew <- filtered_datanew[complete.cases(filtered_datanew[, c("QUESTNNR", "VE02", "check1", "check1")]), ]
filtered_datanew$QUESTNNR <- NULL #Remove columns
filtered_datanew$VE02 <- NULL #Remove columns
filtered_datanew$check1 <- NULL #Remove columns
filtered_datanew$check2 <- NULL #Remove columns
filtered_datanew <- filtered_datanew %>%
  mutate_at(vars(1:105), as.numeric)

############reverse rc items##########
# Identify columns to reverse, trimming whitespace and converting to lowercase for comparison
cols_to_reverse <- codebook_subset$item_name_auswertung[trimws(tolower(codebook$zu_rekodieren)) == "x"]

# Reverse values for identified columns
for (col in cols_to_reverse) {
  if (col %in% names(filtered_datanew)) {
    # Only apply transformation to non-negative values and not NA values
    positive_indices <- which(filtered_datanew[[col]] >= 0 & !is.na(filtered_datanew[[col]]))
    filtered_datanew[positive_indices, col] <- 8 - filtered_datanew[positive_indices, col]
  }
}

# Reverse values for identified columns (from previous analysis) (I do this further down as well)
cols_to_reverse <- c( "optim_2_211_r")
filtered_datanew <- filtered_datanew %>%
  mutate(across(all_of(cols_to_reverse), ~ ifelse(. >= 0 & !is.na(.), 8 - ., .)))

# Match with valicases #######
# Extracting required columns
cols_BF01 <- grep("^BF01_", colnames(datanew), value = TRUE)
cols_RA <- grep("^RA", colnames(datanew), value = TRUE)
cols_cope <- grep("^cope_", colnames(datanew), value = TRUE)
cols_VA01_to_VA10 <- grep("^VA0[1-9]|^VA10", colnames(datanew), value = TRUE)
all_cols <- c(cols_BF01, cols_RA,cols_cope,cols_VA01_to_VA10, "CASE" ) # Combine all column names

# Extract the data
selected_data <- datanew[, all_cols]
merge_data <- datanew %>%
   select(!all_cols)
selected_data[selected_data %in% c(-1, -9)] <- NA # Handle NA values

# Convert all columns to numeric if they are not already
selected_data[] <- lapply(selected_data, function(col) {
  if (is.character(col)) {
    return(ifelse(grepl("^[0-9]+$", col), as.numeric(col), NA))
  } else {
    return(col)
  }
})

selected_data[is.na(selected_data)] <- NA
selected_data <- as.data.frame(selected_data)
# Merge selected_data with filtered_datanew based on the 'CASE' variable
combi2 <- merge(filtered_datanew, selected_data, by = "CASE", all.x = TRUE)

# Renaming columns
combi2 <- combi2 %>%
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
combi2$SD04_06 <- NULL #Remove column


# Named vector for VA05 variables
va05_old_names <- c("VA05_01", "VA05_02", "VA05_03", "VA05_04", "VA05_05", "VA05_06", "VA05_07", "VA05_08", "VA05_09", "VA05_10",
                    "VA05_11", "VA05_12", "VA05_13", "VA05_14", "VA05_15", "VA05_16", "VA05_17", "VA05_18", "VA05_19", "VA05_20",
                    "VA05_21", "VA05_22", "VA05_23", "VA05_24", "VA05_25", "VA05_26", "VA05_27", "VA05_28")

va05_new_names <- c("cope_01_01", "cope_02_02", "cope_03_03_r", "cope_04_04_r", "cope_05_05", "cope_07_06_r", "cope_02_07", "cope_03_08_r",
                    "cope_08_09", "cope_06_10", "cope_04_11_r", "cope_09_12", "cope_14_13_r", "cope_10_14", "cope_05_15", "cope_07_16_r",
                    "cope_09_17", "cope_11_18", "cope_01_19", "cope_12_20", "cope_08_21", "cope_13_22", "cope_06_23", "cope_12_24",
                    "cope_10_25", "cope_14_26_r", "cope_13_27", "cope_11_28")

# Rename columns in combi2
colnames(combi2)[colnames(combi2) %in% va05_old_names] <- va05_new_names

# Identify columns to reverse for the 4-point scale (cope_)
cope_cols_to_reverse <- grep("^cope_.*_r$", names(combi2), value = TRUE)

# Reverse values for identified columns on a 4-point scale
for (col in cope_cols_to_reverse) {
  # Only apply transformation to non-negative values and not NA values
  positive_indices <- which(combi2[[col]] >= 0 & !is.na(combi2[[col]]))
  combi2[positive_indices, col] <- 5 - combi2[positive_indices, col]
}

# Rename back to old names for simpler matching 
colnames(combi2)[colnames(combi2) %in% va05_new_names] <- va05_old_names

# ------------------------------------------------------------------------------
# preprocess olddata
# ------------------------------------------------------------------------------

# Replace negative values with NA
data <- data %>% 
  mutate(across(where(is.numeric), ~ifelse(. < 0, NA_real_, .)))
# Replace NA values with mean
data <-data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
# Reverse values for identified columns (from previous analysis) (i do this in both sets)
cols_to_reverse <- c( "optim_2_211_r")
data <- data %>%
  mutate(across(all_of(cols_to_reverse), ~ ifelse(. >= 0 & !is.na(.), 8 - ., .)))

# for simpler matching i rename cope and rsa items back 
valicodebook <- read.xlsx(xlsxFile = "input/2022-12-11_validation_questionnaires.xlsx",
                          skipEmptyRows = FALSE,sheet=2)
oldnamesVALI <- valicodebook$Oldnames
newnamesVALI <- valicodebook$ItemID.als.Text

# Loop through each column in your dataset
for (col in names(data)) {
  if (col %in% newnamesVALI) {  # Check if the column name matches any entry in 'newnamesVALI'
    old_name <- oldnamesVALI[newnamesVALI == col] # Find the corresponding old name
    names(data)[names(data) == col] <- old_name # Rename the column using the old name
  }
}


# ------------------------------------------------------------------------------
# preprocess alla data
# ------------------------------------------------------------------------------

# import (move up)


library(haven)


# Define the file paths with corrected extensions (if they are data files)
file_path_emotions_sav <- "input/newsoscies/alla/rc_emotionsregulation/t1.sav"
file_path_psychiche_sav <- "input/newsoscies/alla/rc_psychichebelastung/t2.sav"
file_path_emotions_codebook <- "input/newsoscies/alla/rc_emotionsregulation/codebook_empstudpr7_20240202_1609.xlsx"
file_path_psychiche_codebook <- "input/newsoscies/alla/rc_psychichebelastung/codebook_tutorial387996_2024-01-30_12-04.xlsx"

# Read data files
t1_emotions_data <- read_sav(file_path_emotions_sav)
t2_psychiche_data <- read_sav(file_path_psychiche_sav)
t1_emotions_codebook <- read_excel(file_path_emotions_codebook)
t2_psychiche_codebook <- read_excel(file_path_psychiche_codebook)
rm(file_path_emotions_sav,file_path_psychiche_sav,file_path_emotions_codebook,file_path_psychiche_codebook)


# Display the first few rows of each data frame to verify successful import
head(t1_emotions_data)
head(t2_psychiche_data)
head(t1_emotions_codebook)
head(t2_psychiche_codebook)


## t1_emotions_data
# Define the columns to be selected
selected_columns <- c("CASE", "A201_01", "A202", "A203", "A204", 
                      paste0("A402_", sprintf("%02d", 1:50)), 
                      paste0("A406_", sprintf("%02d", 2:56)), 
                      "FINISHED", "Q_VIEWER", "MAXPAGE", "MISSREL", "TIME_RSI")

# Create a subset with the selected columns
t1_emotions_data <- t1_emotions_data %>% select(all_of(selected_columns))

# Extract labels for the selected columns
column_labels <- sapply(t1_emotions_data, function(col) {
  if (is.labelled(col)) {
    attr(col, "label")
  } else {
    NA
  }
})

# Remove "Resilienz 1: " and "Resilienz 2: " from the beginning of the labels if present
column_labels <- sapply(column_labels, function(label) {
  if (!is.na(label)) {
    label <- sub("^Resilienz 1: ", "", label)
    label <- sub("^Resilienz 2: ", "", label)
  }
  label
})

# Create a description row with the labels
description_row <- as.data.frame(t(column_labels))
colnames(description_row) <- colnames(t1_emotions_data)

# Combine the description row with the subset data
t1_emotions_data <- rbind(description_row, t1_emotions_data)


#reversing controll (all items were reversed in questionnaire: Note no additional reversing from prior analysis is applied and is needed to be perfomed)
# Loop through every column in the first row
for (col in 1:ncol(t1_emotions_data)) {
  # Get the value in the first row of the current column
  value <- t1_emotions_data[1, col]
  
  # Check if the value ends with " (umgepolt)"
  if (grepl(" \\(umgepolt\\)$", value)) {
    # Remove " (umgepolt)" from the value
    t1_emotions_data[1, col] <- sub(" \\(umgepolt\\)$", "", value)
  }
}

#Update matching erros manually
# Update the first row content with the provided replacements

# Update the column names based on the provided replacements
colnames(t1_emotions_data)[colnames(t1_emotions_data) == "A402_30"] <- "instr_1_031" # [4] "Ich fühle mich wohl dabei, mir Unterstützung durch andere zu holen, wenn es mir seelisch nicht gut geht."                   
colnames(t1_emotions_data)[colnames(t1_emotions_data) == "A402_35"] <- "kogni_1_285"
colnames(t1_emotions_data)[colnames(t1_emotions_data) == "A406_02"] <- "relig_1_111"
colnames(t1_emotions_data)[colnames(t1_emotions_data) == "A406_24"] <- "sinnz_2_310"
colnames(t1_emotions_data)[colnames(t1_emotions_data) == "A406_38"] <- "umgan_1_221"
colnames(t1_emotions_data)[colnames(t1_emotions_data) == "A406_35"] <- "subst_1_187_r"
colnames(t1_emotions_data)[colnames(t1_emotions_data) == "A402_43"] <- "optim_2_211_r"
colnames(t1_emotions_data)[colnames(t1_emotions_data) == "A406_42"] <- "wunsc_1_154_r"
colnames(t1_emotions_data)[colnames(t1_emotions_data) == "A406_43"] <- "wunsc_1_157_r"
colnames(t1_emotions_data)[colnames(t1_emotions_data) == "A406_51"] <- "zukun_1_341"


# Assuming t1_emotions_data and codebook are already loaded

# Extract the first row of t1_emotions_data
first_row <- t1_emotions_data[1, ]

# Extract the itemtext from the first row
itemtext <- as.character(first_row)

# Create a named vector from codebook for easy lookup
beschriftung_to_auswertung <- setNames(codebook$item_name_auswertung, codebook$item_beschriftung_soscisurvey)

# Match itemtext with item_beschriftung_soscisurvey and get item_name_auswertung
new_variable_names <- beschriftung_to_auswertung[itemtext]

# Ensure that names stay the same if not matching
new_variable_names <- ifelse(is.na(new_variable_names), colnames(t1_emotions_data), new_variable_names)

# Update the column names of t1_emotions_data
colnames(t1_emotions_data) <- new_variable_names
t1_emotions_data <- t1_emotions_data[-1, ]


glimpse(t1_emotions_data)









start_col <- which(colnames(t1_emotions_data) == "ableh_1_068")
end_col <- which(colnames(t1_emotions_data) == "zukun_2_351")
# Subset the data frame using base R indexing
t1_emotions_data[, start_col:end_col] <- lapply(t1_emotions_data[, start_col:end_col], as.numeric)
t1_emotions_data <- as.data.frame(t1_emotions_data)

# correct the reversing
t1_emotions_data <- t1_emotions_data %>%
  mutate(across(all_of(c("optim_2_211_r")), ~ ifelse(. >= 0 & !is.na(.), 8 - ., .)))








## t2_psych_data (does need reversing)
# Define the columns to be selected
start_col <- which(colnames(t2_psychiche_data) == "ableh_1_068")
end_col <- which(colnames(t2_psychiche_data) == "zukun_2_351")
# Subset the data frame using base R indexing
subset_data <- t2_psychiche_data[, start_col:end_col]
additional_columns <- t2_psychiche_data %>%
  select(all_of(c("CASE", "Geschlecht", "SD02_01", "Bildung", "FINISHED", "Q_VIEWER", "MAXPAGE", "MISSREL", "TIME_RSI")))
t2_psychiche_data <- cbind(additional_columns, subset_data)




# Replace prefixes in column names
new_names <- str_replace_all(names(t2_psychiche_data), "^selbs_", "selbw_")
new_names <- str_replace_all(new_names, c("selbw_1_194_r" = "selbv_1_194_r", "selbw_1_198_r" = "selbv_1_198_r"))
names(t2_psychiche_data) <- new_names




#reversing
# Reverse values for identified columns (from previous analysis) (I do this further down as well)
# cols_to_reverse <- c( "optim_2_211_r") # this item is correctly reversed. So we set all items to reverse that end with _r but expect "optim_2_211_r"
cols_to_reverse <- colnames(t2_psychiche_data)[grepl("_r$", colnames(t2_psychiche_data)) & colnames(t2_psychiche_data) != "optim_2_211_r"]
t2_psychiche_data <- t2_psychiche_data%>%
  mutate(across(all_of(cols_to_reverse), ~ ifelse(. >= 0 & !is.na(.), 8 - ., .)))





# Assuming t1_emotions_data and t2_psychiche_data are already loaded

# Identify common columns
common_columns <- intersect(colnames(t1_emotions_data), colnames(t2_psychiche_data))
common_columns <- setdiff(common_columns, c("CASE", "FINISHED", "Q_VIEWER", "MAXPAGE", "MISSREL", "TIME_RSI"))

# Subset the data frames to include only the common columns
t1_common <- t1_emotions_data %>% select(all_of(common_columns))
t2_common <- t2_psychiche_data %>% select(all_of(common_columns))

# Sort columns based on the first 5 letters of their names
sorted_columns <- common_columns[order(substr(common_columns, 1, 5))]
t1_common <- t1_common %>% select(all_of(sorted_columns))
t2_common <- t2_common %>% select(all_of(sorted_columns))

# Calculate the correlation matrices for the common columns
correlation_matrix_t1 <- cor(t1_common, use = "complete.obs")
correlation_matrix_t2 <- cor(t2_common, use = "complete.obs")

# Convert correlation matrices to data frames for export
correlation_matrix_t1_df <- as.data.frame(correlation_matrix_t1)
correlation_matrix_t2_df <- as.data.frame(correlation_matrix_t2)

# Export correlation_matrix_t1 to Excel
write.xlsx(correlation_matrix_t1_df, file = "output/corrmatrix/correlation_matrix_t1.xlsx", rowNames = TRUE)

# Export correlation_matrix_t2 to Excel
write.xlsx(correlation_matrix_t2_df, file = "output/corrmatrix/correlation_matrix_t2.xlsx", rowNames = TRUE)








# Check for reversing errors
check_reversing_errors <- function(data, prefix_length = 5) {
  prefixes <- substr(colnames(data), 1, prefix_length)
  unique_prefixes <- unique(prefixes)
  
  reversing_errors <- list()
  
  for (prefix in unique_prefixes) {
    cols_with_prefix <- data %>% select(starts_with(prefix))
    if (ncol(cols_with_prefix) > 1) {
      cor_matrix <- cor(cols_with_prefix, use = "complete.obs")
      if (any(cor_matrix[upper.tri(cor_matrix)] < 0)) {
        reversing_errors[[prefix]] <- cor_matrix
      }
    }
  }
  
  return(reversing_errors)
}

# Check for reversing errors in both datasets
reversing_errors_t1 <- check_reversing_errors(t1_common)
reversing_errors_t2 <- check_reversing_errors(t2_common)

# Print reversing errors
print("Reversing errors in t1_emotions_data:")
print(reversing_errors_t1)

print("Reversing errors in t2_psychiche_data:")
print(reversing_errors_t2)

# -> this looks fine











#t1_emotions_data
# Rename columns
t1_emotions_data <- t1_emotions_data %>%
  rename(
    age = A201_01,
    gender = A202,
    Bildung = A203,
    Taetigkeit = A204
  )

# Transform the variables using case_when
t1_emotions_data <- t1_emotions_data %>%
  mutate(
    gender = case_when(
      gender == 1 ~ "Männlich",
      gender == 2 ~ "Weiblich",
      gender == 3 ~ "Divers",
      gender == -1 ~ "Möchte ich nicht angeben.",
      gender == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(gender)
    ),
    Bildung = case_when(
      Bildung == 1 ~ "Mittlere Reife",
      Bildung == 2 ~ "(Fach)Abitur",
      Bildung == 3 ~ "Bachelor/Meister/Techniker",
      Bildung == 4 ~ "Master/Diplom/vergleichbarer anderer Abschluss",
      Bildung == 5 ~ "Promotion",
      Bildung == 6 ~ "Sonstiges",
      Bildung == -1 ~ "Möchte ich nicht angeben.",
      Bildung == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Bildung)
    ),
    Taetigkeit = case_when(
      Taetigkeit == 1 ~ "Vollzeitig erwerbstätig",
      Taetigkeit == 2 ~ "Teilzeit erwerbstätig",
      Taetigkeit == 3 ~ "Minijob (nicht zusätzlich zum Studium oder Ausbildung)",
      Taetigkeit == 4 ~ "Auszubildende:r",
      Taetigkeit == 5 ~ "Student:in",
      Taetigkeit == 6 ~ "Arbeitssuchend",
      Taetigkeit == 7 ~ "Rentner:in",
      Taetigkeit == 8 ~ "Sonstiges",
      Taetigkeit == -1 ~ "Möchte ich nicht angeben.",
      Taetigkeit == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Taetigkeit)
    )
  )



#Quality criteria
t1_emotions_data <- t1_emotions_data %>%
  mutate(MAXPAGE = as.numeric(MAXPAGE), MISSREL = as.numeric(MISSREL)) %>%
  filter(MAXPAGE > 4, MISSREL <= 2)
# we checked here additioanlly for lower outliers and report none




#t2_psychiche_data
t2_psychiche_data$Taetigkeit <- NA
t2_psychiche_data <- t2_psychiche_data %>%
  rename(
    age = SD02_01,
    gender = Geschlecht,
    Bildung = Bildung  )


# Transform the variables using case_when
t2_psychiche_data <- t2_psychiche_data %>%
  mutate(
    gender = case_when(
      gender == 1 ~ "Weiblich",
      gender == 2 ~ "Männlich",
      gender == 3 ~ "Divers",
      gender == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(gender)
    ),
    Bildung = case_when(
      Bildung == 1 ~ "Keinen Schulabschluss",
      Bildung == 2 ~ "Hauptschulabschluss",
      Bildung == 3 ~ "Mittlere Reife",
      Bildung == 4 ~ "Fachabitur/Abitur",
      Bildung == 5 ~ "Bachelor",
      Bildung == 6 ~ "Master/Diplom",
      Bildung == 7 ~ "Promotion",
      Bildung == -9 ~ "nicht beantwortet",
      TRUE ~ as.character(Bildung)
    )
  )


#Quality criteria
t2_psychiche_data <- t2_psychiche_data %>%
  mutate(MAXPAGE = as.numeric(MAXPAGE), MISSREL = as.numeric(MISSREL)) %>%
  filter(MAXPAGE > 4, MISSREL <= 2)
# we checked here additioanlly for lower outliers and report none





# Replace NA values with mean
t1_emotions_data <-t1_emotions_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

t2_psychiche_data <-t2_psychiche_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))





# ------------------------------------------------------------------------------
# preprocess combined data
# ------------------------------------------------------------------------------

common_cols <- intersect(names(combi2), names(data)) # Find columns that are common between combi2 and data
unmatched_cols <- setdiff(names(data), common_cols) # Find columns in data that are not in combi2
unmatched_cols # Here helper variables, old items, and times are unmatched. which is fine
# Append rows from data to combi2 only for matching columns
combined_df <- combi2
if (length(common_cols) > 0) {
  combined_df <- rbind(combi2, data[, common_cols])
}

# Take out "case". We later give new cases. In this way, we can important using "1:105"
combined_df <- combined_df %>%
  select(!CASE)

# Renaming columns
combined_df <- combined_df %>%
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



## add allas data



combined_df








# Corr Prior to combination

# Subset the data frames to include only the common columns
t1_common <- combined_df %>% select(all_of(sorted_columns))

# Calculate the correlation matrices for the common columns
correlation_matrix_combined_df <- as.data.frame(cor(t1_common, use = "complete.obs"))

# Export correlation_matrix_t1 to Excel
write.xlsx(correlation_matrix_combined_df, file = "output/corrmatrix/correlation_matrix_prior_to_combined_df.xlsx", rowNames = TRUE)







# Ensure all columns from combined_df are present in t1_emotions_data and t2_psychiche_data
all_columns <- colnames(combined_df)
'
# Align t1_emotions_data columns to all_columns
t1_emotions_data <- t1_emotions_data %>%
  select(all_of(all_columns)) %>%
  mutate(across(all_of(setdiff(all_columns, colnames(t1_emotions_data))), ~ NA))

# Align t2_psychiche_data columns to all_columns
t2_psychiche_data <- t2_psychiche_data %>%
  select(all_of(all_columns)) %>%
  mutate(across(all_of(setdiff(all_columns, colnames(t2_psychiche_data))), ~ NA))
  
  '


# Ensure numeric columns are correctly formatted
t1_emotions_data <- t1_emotions_data %>%
  mutate(across(c(CASE, age, FINISHED, Q_VIEWER, TIME_RSI), as.numeric))

t2_psychiche_data <- t2_psychiche_data %>%
  mutate(across(c(CASE, age, FINISHED, Q_VIEWER, TIME_RSI), as.numeric))

combined_df <- combined_df %>%
  mutate(age = as.numeric(age))

# Combine t1_emotions_data and t2_psychiche_data
t1_t2 <- bind_rows(t1_emotions_data, t2_psychiche_data)


'
# Align t1_t2 columns to all_columns
t1_t2 <- t1_t2 %>%
  select(all_of(all_columns)) %>%
  mutate(across(all_of(setdiff(all_columns, colnames(t1_t2))), ~ NA))
'

# Calculate the correlation matrices to Export to Excel for control
correlation_matrix_t1_t2 <- as.data.frame(cor(t1_t2[sorted_columns], use = "complete.obs"))
write.xlsx(correlation_matrix_t1_t2, file = "output/corrmatrix/correlation_matrix_t1_t2.xlsx", rowNames = TRUE)


# Bind the rows of combined_df and t1_t2
final_combined_df <- bind_rows(combined_df, t1_t2)

# Print the final combined dataframe to verify
print(final_combined_df)



combined_df <- final_combined_df




# #demos
demos <- combined_df %>%
  select("age", "gender", "Taetigkeit") %>%
  mutate(
    age = ifelse(is.na(age), "keine Angabe", age),
    gender = case_when(
      gender == "4" ~ "keine Angabe",
      is.na(gender) ~ "keine Angabe",
      TRUE ~ gender
    ),
    Taetigkeit = case_when(
      Taetigkeit == "9" ~ "keine Angabe",
      Taetigkeit == "-9" ~ "nicht beantwortet",
      TRUE ~ Taetigkeit
    )
  )

# Convert age back to numeric for summary statistics, excluding "keine Angabe"
age_numeric <- demos %>%
  filter(age != "keine Angabe") %>%
  mutate(age = as.numeric(age))


str(age_numeric)

# Standardize the Taetigkeit values to handle duplicates
age_numeric <- age_numeric %>%
  mutate(Taetigkeit = case_when(
    Taetigkeit == "Minijob (nicht zusätzlich zum Studium oder Ausbildung)" ~ "Minijob (nicht zusaetzlich zum Studium oder Ausbildung)",
    Taetigkeit == "Teilzeit erwerbstätig" ~ "Teilzeit erwerbstaetig",
    Taetigkeit == "Vollzeitig erwerbstätig" ~ "Vollzeit erwerbstaetig",
    TRUE ~ Taetigkeit
  ))


# Handle Psychotherapeuten_Unterstuetzung replacements
subdemos <- combined_df %>%
  select("Psychotherapeuten_Unterstuetzung") %>%
  mutate(
    Psychotherapeuten_Unterstuetzung = case_when(
      Psychotherapeuten_Unterstuetzung == "4" ~ "keine Angabe",
      Psychotherapeuten_Unterstuetzung == "xml:space=\"preserve\"> -9" ~ NA,
      TRUE ~ Psychotherapeuten_Unterstuetzung
    )
  )

# Add Psychotherapeuten_Unterstuetzung to demos
demos <- demos %>%
  bind_cols(subdemos)

# Calculate summary statistics for age
age_summary <- age_numeric %>%
  summarize(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE)
  )

# Calculate counts and percentages for gender
gender_summary <- demos %>%
  group_by(gender) %>%
  summarize(
    count = n(),
    percentage = round((n() / nrow(demos)) * 100)
  ) %>%
  ungroup()

# Calculate counts and percentages for Taetigkeit
taetigkeit_summary <- demos %>%
  group_by(Taetigkeit) %>%
  summarize(
    count = n(),
    percentage = round((n() / nrow(demos)) * 100)
  ) %>%
  ungroup()

# Calculate counts and percentages for Psychotherapeuten_Unterstuetzung
psychotherapeuten_summary <- demos %>%
  group_by(Psychotherapeuten_Unterstuetzung) %>%
  summarize(
    count = n(),
    percentage = round((n() / nrow(demos)) * 100)
  ) %>%
  ungroup()

# Combine all summaries into one report data frame
report <- list(
  age_summary = age_summary,
  gender_summary = gender_summary,
  taetigkeit_summary = taetigkeit_summary,
  psychotherapeuten_summary = psychotherapeuten_summary
)

# Display the report
report







#### nearst discrete number #### (can be done way simpler using integer, but this does the trick too)
# Define the range of discrete values
discrete_range <- 1:7
is_not_discrete <- function(x) sum(!x %in% discrete_range) # Function to check if a value is not in the specified range
replace_non_discrete <- function(x) { # Function to replace non-discrete values with the nearest discrete value
  sapply(x, function(value) {
    if (value %in% discrete_range) {
      return(value)  # Value is already discrete
    } else {
      nearest_discrete <- discrete_range[which.min(abs(discrete_range - value))] # Find the nearest discrete value within the range
      return(nearest_discrete)
    }
  })
}

# Apply the functions to the first 105 columns of 'combined_df'
first_105_cols <- names(combined_df)[1:105]
# Checking for non-discrete values in the first 105 columns
non_discrete_counts <- sapply(combined_df[first_105_cols], is_not_discrete)
print("Count of non-discrete values per variable in the first 105 columns:")
print(non_discrete_counts)
# Replacing non-discrete values in the first 105 columnss
combined_df[first_105_cols] <- sapply(combined_df[first_105_cols], replace_non_discrete)
# Convert the affected columns back to numeric
combined_df[first_105_cols] <- as.data.frame(lapply(combined_df[first_105_cols], as.numeric))
non_discrete_counts <- sapply(combined_df[first_105_cols], is_not_discrete)
print("Count of non-discrete values per variable in the first 105 columns:")
print(non_discrete_counts)





#clean env
rm("codebook_subset","col","cols_to_reverse", "common_itemslist",
   "demonames","discrete_range",
   "first_105_cols","is_not_discrete","merged_df",
   "names_vector","newnames" ,"non_discrete_counts","oldnames","positive_indices",
   "replace_non_discrete","subset_datanew" )


# ------------------------------------------------------------------------------
# Correlative analysis  (item level)
# ------------------------------------------------------------------------------
'

#objectives:

- Find unassociated items on the item Level 
- We test the remaining items and use validity scales to facets that tend to not correlate with the valitdiy scale. 

i test here on the item and factor level. In publication we use the item level as we stick consistently on the item level thoughout the studies
  
 
 '

#items associated with
#"Moral, Ethik und Altruismus", "Strukturiertheit","Vorbilder"
#is already dropped in study 1

# Following chunck drops items associated with religion, substance abuse and planning

'+++++++++++++++++++++++++++++++++++++++++++++'
########+++++++++++++++++++++++++++++++++++++++++++++########
'+++++++++++++++++++++++++++++++++++++++++++++'





# Assuming 'combined_df_numeric' is your data frame with the data to be plotted
# Ensure that combined_df_numeric contains only numeric data

# Define resilience and coping factors
resilience_factors <- c("famil_", "menta_", "optim_", "physi_", "sinnz_", "selbw_", "umgan_", "zukun_","relig_")
coping_factors <- c("kogni_", "akzep_", "instr_", "posit_", "selbv_", "ablen_", "ableh_", "behav_", "humor_", "aktiv_", "subst_", "planu_", "wunsc_", "sozia_")

# Combine factors for easier processing
all_factors <- c(resilience_factors, coping_factors)

# Ensure that combined_df contains only numeric data
combined_df_numeric <- combined_df[sapply(combined_df, is.numeric)]

# Aggregate data by factor prefixes to calculate mean for each factor
factor_means <- sapply(all_factors, function(prefix) {
  cols <- grep(paste0("^", prefix), names(combined_df_numeric))
  factor_data <- as.matrix(combined_df_numeric[, cols])  # Explicitly convert to a matrix
  rowMeans(factor_data, na.rm = TRUE)  # Calculate the mean for each row (factor)
})


# Convert the list to a data frame
factor_means_df <- as.data.frame(factor_means)

# Calculate the correlation matrix for the factor means
cor_matrix <- cor(factor_means_df, method = "pearson", use = "pairwise.complete.obs")

# Create the heatmap with the dendrogram using pheatmap
p <- pheatmap(cor_matrix, 
         cluster_rows = TRUE, 
         cluster_cols = TRUE,
         display_numbers = TRUE, 
         border_color = NA,
         annotation_col = NULL,
         annotation_row = NULL,
         main = "Heatmap factorwise mean correlation")
p 

#-> Religion, planu, physi, subst raus nehmen. 


# Save the plot
ggsave("output/heatmaps/heatmap_resilience_coping.png", plot = p, width = 10, height = 8)  # Adjusted height




##### between validity items #####
# Extracting required columns
cols_BF01 <- grep("^BF01_", colnames(combined_df), value = TRUE)
cols_RA <- grep("^RA", colnames(combined_df), value = TRUE)
cols_cope <- grep("^VA05", colnames(combined_df), value = TRUE)
cols_VA01_to_VA10 <- grep("^VA0[1-9]|^VA10", colnames(combined_df), value = TRUE)

# Combine all column names
all_cols <- c(cols_BF01, cols_RA,cols_cope,cols_VA01_to_VA10)

# Extract the data
selected_data <- combined_df[, all_cols]

# Handle NA values
selected_data[selected_data %in% c(-1, -9)] <- NA

selected_data
# Convert all columns to numeric if they are not already
selected_data[] <- lapply(selected_data, function(col) {
  if (is.character(col)) {
    return(ifelse(grepl("^[0-9]+$", col), as.numeric(col), NA))
  } else {
    return(col)
  }
})

selected_data[is.na(selected_data)] <- NA
selected_data <- as.data.frame(selected_data)




valicodebook <- read.xlsx(xlsxFile = "input/2022-12-11_validation_questionnaires.xlsx",
                          skipEmptyRows = FALSE)

oldnamesVALI <- valicodebook$Oldnames
newnamesVALI <- valicodebook$ItemID.als.Text


# Create a named vector where names are oldnames and values are newnames
names_vector <- setNames(oldnamesVALI,newnamesVALI)
names_vector
str(names_vector)
# Loop over each old name in 'oldnamesVALI'
for (i in seq_along(oldnamesVALI)) {
  # Find the index of the old name in 'valicombi'
  old_name_index <- which(names(selected_data) == oldnamesVALI[i])
  # Check if the old name is found in 'valicombi'
  if (length(old_name_index) > 0) {
    # Rename the column using the corresponding new name
    names(selected_data)[old_name_index] <- newnamesVALI[i]
  }
}

# View the renamed columns in 'valicombi'
print(names(selected_data))


# Remove variables starting with "VA05" from selected_data
selected_data <- selected_data %>%
  select(-starts_with("VA05")) #cope items are duplicates here VA05 is cope


vali_factors <- c("brcs", "rs11", "rsa", "cope", "mccs", "swls", "ffa", "bfi_e" ,"bfi_v","bfi_g","bfi_n","bfi_o", "gse", "ppse", "gpsk", "bokx")

# Combine factors for easier processing
all_factors <- c(vali_factors)

# Ensure that combined_df contains only numeric data
valicombi1_numeric <- selected_data[sapply(selected_data, is.numeric)]

# Aggregate data by factor prefixes to calculate mean for each factor
factor_means <- sapply(all_factors, function(prefix) {
  cols <- grep(paste0("^", prefix), names(valicombi1_numeric))
  factor_data <- as.matrix(valicombi1_numeric[, cols])  # Explicitly convert to a matrix
  rowMeans(factor_data, na.rm = TRUE)  # Calculate the mean for each row (factor)
})

factor_means



# Convert the list to a data frame
factor_means_df <- as.data.frame(factor_means)

# Calculate the correlation matrix for the factor means
cor_matrix <- cor(factor_means_df, method = "pearson", use = "pairwise.complete.obs")

# Create the heatmap with the dendrogram using pheatmap
g <- pheatmap(cor_matrix, 
              cluster_rows = TRUE, 
              cluster_cols = TRUE,
              display_numbers = TRUE, 
              border_color = NA,
              annotation_col = NULL,
              annotation_row = NULL,
              main = "Heatmap factorwise mean correlation")
g



# Save the plot
ggsave("output/heatmaps/heatmap_validityscales.png", plot = g, width = 10, height = 8)  # Adjusted height











# Extracting required columns for resilience and coping factors
cols_resilience <- grep(paste0("^", paste(resilience_factors, collapse = "|")), colnames(combined_df), value = TRUE)
cols_coping <- grep(paste0("^", paste(coping_factors, collapse = "|")), colnames(combined_df), value = TRUE)

# Combine all column names including those for validity scales
all_cols <- c(cols_resilience, cols_coping, cols_BF01, cols_RA, cols_VA01_to_VA10)

# Extract the data
selected_data <- combined_df[, all_cols]
glimpse(combined_df)

# Handle NA values
selected_data[selected_data %in% c(-1, -9)] <- NA
glimpse(selected_data)
# Convert all columns to numeric if they are not already
selected_data[] <- lapply(selected_data, function(col) {
  if (is.character(col)) {
    return(ifelse(grepl("^[0-9]+$", col), as.numeric(col), NA))
  } else {
    return(col)
  }
})



# Assuming valicodebook is already loaded with oldnamesVALI and newnamesVALI
# Create a named vector where names are oldnames and values are newnames
names_vector <- setNames(oldnamesVALI, newnamesVALI)

# Loop over each old name in 'oldnamesVALI'
for (i in seq_along(oldnamesVALI)) {
  # Find the index of the old name in 'selected_data'
  old_name_index <- which(names(selected_data) == oldnamesVALI[i])
  # Check if the old name is found in 'selected_data'
  if (length(old_name_index) > 0) {
    # Rename the column using the corresponding new name
    names(selected_data)[old_name_index] <- newnamesVALI[i]
  }
}

# Rename columns in combi2
colnames(selected_data)[colnames(selected_data) %in% va05_old_names] <- va05_new_names
names(selected_data)

# Combine factors for easier processing
all_factors <- c(vali_factors, resilience_factors, coping_factors)

# Ensure that selected_data contains only numeric data
selected_data_numeric <- selected_data[sapply(selected_data, is.numeric)]




# Aggregate data by factor prefixes to calculate mean for each factor
factor_means <- sapply(all_factors, function(prefix) {
  cols <- grep(paste0("^", prefix), names(selected_data_numeric), value = TRUE)
  if (length(cols) > 0) {
    factor_data <- selected_data_numeric[, cols, drop = FALSE]  # Keep it as a data frame/matrix
    return(rowMeans(factor_data, na.rm = TRUE))  # Calculate the mean for each row (factor)
  } else {
    return(NA)  # Return NA if no columns match the prefix
  }
})

# Convert the list to a data frame
factor_means_df <- as.data.frame(factor_means)
factor_means_df



'+++++++++++++++++++++++++++++++++++++++++++++'
#Side quest: Full Data 
exportdata <- combined_df %>%
  cbind(factor_means_df)

#write.xlsx(exportdata, file="testomesto.xlsx")
'+++++++++++++++++++++++++++++++++++++++++++++'

# Calculate the correlation matrix for the factor means
cor_matrix <- cor(factor_means_df, method = "pearson", use = "pairwise.complete.obs")

# Subset the correlation matrix
subset_cor <- cor_matrix[c(resilience_factors, coping_factors), vali_factors]
subset_cor
# Reorder rows and columns
subset_cor <- subset_cor[, order(colnames(subset_cor))]
subset_cor <- subset_cor[order(rownames(subset_cor)), ]

# Create the heatmap with the dendrogram using pheatmap
l <- pheatmap(subset_cor, 
              cluster_rows = TRUE, 
              cluster_cols = TRUE,
              display_numbers = TRUE, 
              border_color = NA,
              annotation_col = NULL,
              annotation_row = NULL,
              main = "Heatmap factorwise mean correlation",
              legend = TRUE  # Add legend
)
l



# Save the plot
ggsave("output/heatmaps/heatmap_validityscalesxrcscales.png", plot = l, width = 10, height = 8)  # Adjusted height






# Define factors
resilience_factors <- c("famil_", "menta_", "optim_", "physi_", "sinnz_", "selbw_", "umgan_", "zukun_", "relig_")
coping_factors <- c("kogni_", "akzep_", "instr_", "posit_", "selbv_", "ablen_", "ableh_", "behav_", "humor_", "aktiv_", "subst_", "planu_", "wunsc_", "sozia_")
vali_factors <- c("brcs", "rs11", "rsa", "cope", "mccs", "swls", "ffa", "bfi_e", "bfi_v", "bfi_g", "bfi_n", "bfi_o", "gse", "ppse", "gpsk", "bokx")

# Ensure that valicombi1_numeric contains only numeric data
valicombi1_numeric <- selected_data[sapply(selected_data, is.numeric)]

# Extract individual resilience and coping items
resilience_items <- grep(paste0("^", paste(resilience_factors, collapse="|")), names(valicombi1_numeric), value = TRUE)
coping_items <- grep(paste0("^", paste(coping_factors, collapse="|")), names(valicombi1_numeric), value = TRUE)

# Aggregate only vali_factors
vali_means <- sapply(vali_factors, function(prefix) {
  cols <- grep(paste0("^", prefix), names(valicombi1_numeric))
  factor_data <- as.matrix(valicombi1_numeric[, cols, drop = FALSE])  # Explicitly convert to a matrix
  rowMeans(factor_data, na.rm = TRUE)  # Calculate the mean for each row (factor)
})

# Combine individual items and vali factor means
combined_data <- cbind(valicombi1_numeric[, c(resilience_items, coping_items)], as.data.frame(vali_means))

# Calculate the correlation matrix
cor_matrix <- cor(combined_data, method = "pearson", use = "pairwise.complete.obs")

# Subset the correlation matrix
subset_cor <- cor_matrix[c(1:105), vali_factors]
subset_cor
# Reorder rows and columns
subset_cor <- subset_cor[, order(colnames(subset_cor))]
subset_cor <- subset_cor[order(rownames(subset_cor)), ]


# Create the heatmap with the dendrogram using pheatmap
l <- pheatmap(subset_cor, 
              cluster_rows = TRUE, 
              cluster_cols = TRUE,
              display_numbers = TRUE, 
              border_color = NA,
              annotation_col = NULL,
              annotation_row = NULL,
              main = "Heatmap of Individual Items vs. Vali Factors Correlation",
              legend = TRUE
)
l



# Save the plot
ggsave("output/heatmaps/heatmap_itemwise_means.png", plot = l, width = 15, height = 10)  # Adjusted height





# ------------------------------------------------------------------------------
# factorial testing
# ------------------------------------------------------------------------------

all_items_ff <- c("akzep_2_014", "akzep_3_019", "akzep_3_022", "akzep_3_024", "akzep_3_025", "optim_1_200", "optim_1_201_r", "optim_1_203_r", "optim_1_204", "optim_2_211_r", "optim_2_214", "posit_1_166", "posit_1_167", "posit_1_168", "selbw_1_316", "selbw_1_317_r", "selbw_1_319", "selbw_1_321", "selbw_1_322", "selbw_2_325", "selbw_2_327", "selbw_2_328", "selbw_2_329", "selbw_2_331", "sinnz_1_299", "sinnz_1_300", "sinnz_1_301", "sinnz_1_303_r", "sinnz_2_307", "sinnz_2_309", "sinnz_2_310", "sinnz_2_311", "sinnz_2_312", "zukun_1_334", "zukun_1_335_r", "zukun_1_336_r", "zukun_1_337", "zukun_1_338", "zukun_1_339", "zukun_1_341", "zukun_2_344", "zukun_2_346_r", "zukun_2_347", "zukun_2_350", "zukun_2_351", "humor_1_082", "humor_1_085_r", "humor_1_087", "instr_1_028_r", "instr_1_029", "instr_1_031", "famil_1_371", "famil_1_373", "famil_1_374", "famil_1_378", "sozia_1_038", "sozia_1_352", "sozia_1_353_r", "sozia_1_354", "sozia_1_355", "sozia_1_356_r", "sozia_1_358_r", "ablen_1_048", "ablen_1_051", "ablen_1_054", "kogni_1_136", "kogni_1_281_r", "kogni_1_283_r", "kogni_1_284", "kogni_1_285", "kogni_2_292_r", "kogni_2_296", "menta_1_272", "physi_1_263", "physi_1_264", "aktiv_1_175_r", "aktiv_1_176", "aktiv_1_177", "aktiv_1_180", "umgan_1_219_r", "umgan_1_220", "umgan_1_221", "umgan_1_222", "ableh_1_068", "ableh_2_081_r", "behav_1_128", "behav_1_129_r", "behav_1_132_r", "behav_1_133_r", "behav_1_135_r", "behav_1_136_r", "selbv_1_194_r", "selbv_1_198_r", "wunsc_1_145_r", "wunsc_1_152_r", "wunsc_1_154_r", "wunsc_1_157_r", "wunsc_1_161_r")
# Performed with 98 Items

# Since beginning, we droped these factors:
#"Moral, Ethik und Altruismus", "Strukturiertheit","Vorbilder"
#"Religion","Planung" , "Substanzenmissbrauch (-)" 

# --------
# Model 1: Exploratory graph analysis Model 
# --------
#newest:
#Use the graphical least absolute shrinkage and selection operator (GLASSO) or the triangulated maximally filtered graph (TMFG) estimators
#to estimate the dimensionality and structure of the data. Use the bootEGA function to assess the stability of the EGA results across many bootstrap samples. Use the largest item redundancies found in the first step to repeat the EGA analysis and assess its stability.


data_EGA <- combined_df[,all_items_ff]
# Truncate item names to the first 5 letters and ensure uniqueness
unique_names <- function(names) {
  truncated <- substr(names, 1, 5)
  # Create a unique identifier for each name
  ids <- ave(rep(1, length(truncated)), truncated, FUN = seq_along)
  # Combine truncated names with their identifiers
  paste0(truncated, ids)
}

# Apply function to the column names of data
colnames(data_EGA) <- unique_names(colnames(data_EGA))

# Now proceed with EGA analysis
myEGA <- ega.HSQ <- EGA(
  data_EGA,
  uni.method = "LE",
  corr = "cor_auto",
  model = "tmfg",
  algorithm = "walktrap",
  plot.EGA = TRUE,
  plot.type = "qgraph"
) # finere cluster #boris


table(ega.HSQ$wc)
#1  2  3  4  5  6  7  8  9 
#7  7 15 28 12 13  7  3  6 


communalities <- unname(ega.HSQ$wc)
cat(communalities)
data_EGA <- combined_df[,all_items_ff]


model_egabifactor <- c(1,2,2,2,3,4,5,6,5,4,4,4,4,4,4,5,4,5,4,4,4,4,5,4,3,1,4,1,4,1,5,4,4,5,5,6,4,4,4,4,5,5,4,4,3,3,3,3,6,3,3,7,7,7,7,3,7,6,3,3,6,5,8,2,3,2,6,6,2,1,6,4,3,8,8,6,4,3,1,6,2,1,4,4,3,4,9,9,9,6,9,6,5,6,7,9,9,7)
#model1_exploratory <- bfactor(data_EGA, model_egabifactor,technical = list(NCYCLES=2000))
#Iteration: 1309, Log-Lik: -117485.965, Max-Change: 0.00010 (2024-05-21:17.02)

#save(model1_exploratory, file = "mirtmodels/e_modele/model1_exploratory.RData")
load(file = "mirtmodels/e_modele/model1_exploratory.RData")


# --------
# BASELINE MODEL 1a: bifactor resilience, reference: coping
# --------

# Initialize lists to store IRT itemsets and fit statistics
itemsets_IRT <- list()
itemsets_IRT_fit <- list()

# grouping procedure
factor_g0 <- c("Ablehnung (-)", "Ablenkung", "Aktives Coping", "Akzeptanz", "Behavioural disengagement (-)","Religion"  , 
               "Familiärer Zusammenhalt", "Humor", "Instrumentelle Unterstützung", "Kognitive Rekonstruierung", 
               "Mentale Anstrengung", "Moral, Ethik und Altruismus", "Optimismus", 
               "Positives Denken", "Selbstvorwurf (-)", "Selbstwahrnehmung", 
               "Sinn, Zweck/Bestimmung und Entwicklung", "Soziale Unterstützung", "Strukturiertheit", 
               "Umgang mit Angst", "Vorbilder", "Wunsch Denken (-)", "Zukunftsplanung")


factor_r1 <-  c("Familiärer Zusammenhalt", "Mentale Anstrengung", "Moral, Ethik und Altruismus","Optimismus", 
                "Physische Anstrengung", "Selbstwahrnehmung", "Sinn, Zweck/Bestimmung und Entwicklung",
                "Strukturiertheit","Umgang mit Angst", "Vorbilder","Zukunftsplanung")


factor_c2 <-  c("Soziale Unterstützung","Akzeptanz" ,"Instrumentelle Unterstützung"  ,"Ablenkung",
                "Ablehnung (-)"  ,"Humor" ,"Behavioural disengagement (-)", "Kognitive Rekonstruierung" ,
                "Wunsch Denken (-)" ,"Positives Denken","Aktives Coping" , "Selbstvorwurf (-)")



map_items_to_factor <- function(factor_facets, codebook)  {
  # Filtering the codebook based on the factor facets
  items_for_factor <- codebook$item_name_auswertung[codebook$name_facette %in% factor_facets]
  return(items_for_factor)
}

# Mapping items for each factor
items_g0 <- map_items_to_factor(factor_g0, codebook)
items_r1 <- map_items_to_factor(factor_r1, codebook)
items_c2 <- map_items_to_factor(factor_c2, codebook)


# Map your data structure to the example by Chalmers
# Extracting the item names from the dataset
all_items <- colnames(data_EGA)


# Create the 'specific' vector based on the item order in 'data_rcitems_3_noNA'
specific <- numeric(length(all_items))

for (i in seq_along(all_items)) {
  if (all_items[i] %in% items_r1) {
    specific[i] <- 1
  } else if (all_items[i] %in% items_c2) {
    specific[i] <- NA
  } else {
    # Assuming that items not in r1 or c2 are general items and not assigned to a specific factor.
    specific[i] <- 0
  }
}

specific

# Run the bfactor model 
#model1a_s_1bi <- bfactor(data_EGA, specific, technical = list(NCYCLES=2000))
#Iteration: 607, Log-Lik: -177258.122, Max-Change: 0.00010 (2024-05-21)

#save(model1a_s_1bi, file = "mirtmodels/e_modele/model1a_s_1bi.RData")
load(file = "mirtmodels/e_modele/model1a_s_1bi.RData")




# --------
# BASELINE MODEL 1b: bifactor coping, reference: resilience
# --------

# Initialize lists to store IRT itemsets and fit statistics
itemsets_IRT <- list()
itemsets_IRT_fit <- list()

# grouping procedure
factor_g0 <- c("Ablehnung (-)", "Ablenkung", "Aktives Coping", "Akzeptanz", "Behavioural disengagement (-)","Religion"  , 
               "Familiärer Zusammenhalt", "Humor", "Instrumentelle Unterstützung", "Kognitive Rekonstruierung", 
               "Mentale Anstrengung", "Moral, Ethik und Altruismus", "Optimismus", 
               "Positives Denken", "Selbstvorwurf (-)", "Selbstwahrnehmung", 
               "Sinn, Zweck/Bestimmung und Entwicklung", "Soziale Unterstützung", "Strukturiertheit", 
               "Umgang mit Angst", "Vorbilder", "Wunsch Denken (-)", "Zukunftsplanung")


factor_r1 <-  c("Familiärer Zusammenhalt", "Mentale Anstrengung", "Moral, Ethik und Altruismus","Optimismus", 
                "Physische Anstrengung", "Selbstwahrnehmung", "Sinn, Zweck/Bestimmung und Entwicklung",
                "Strukturiertheit","Umgang mit Angst", "Vorbilder","Zukunftsplanung")


factor_c2 <-  c("Soziale Unterstützung","Akzeptanz" ,"Instrumentelle Unterstützung"  ,"Ablenkung",
                "Ablehnung (-)"  ,"Humor" ,"Behavioural disengagement (-)", "Kognitive Rekonstruierung" ,
                "Wunsch Denken (-)" ,"Positives Denken","Aktives Coping" , "Selbstvorwurf (-)")



map_items_to_factor <- function(factor_facets, codebook)  {
  # Filtering the codebook based on the factor facets
  items_for_factor <- codebook$item_name_auswertung[codebook$name_facette %in% factor_facets]
  return(items_for_factor)
}

# Mapping items for each factor
items_g0 <- map_items_to_factor(factor_g0, codebook)
items_r1 <- map_items_to_factor(factor_r1, codebook)
items_c2 <- map_items_to_factor(factor_c2, codebook)


# Map your data structure to the example by Chalmers
# Extracting the item names from the dataset
all_items <- colnames(data_EGA)


# Create the 'specific' vector based on the item order in 'data_rcitems_3_noNA'
specific <- numeric(length(all_items))

for (i in seq_along(all_items)) {
  if (all_items[i] %in% items_r1) {
    specific[i] <- NA
  } else if (all_items[i] %in% items_c2) {
    specific[i] <- 1
  } else {
    # Assuming that items not in r1 or c2 are general items and not assigned to a specific factor.
    specific[i] <- 0
  }
}

specific


# Run the bfactor model 
#model1b_s_1bi <- bfactor(data_EGA, specific, technical = list(NCYCLES=2000))
#Iteration: 402, Log-Lik: -177353.573, Max-Change: 0.00009 (2024-05-21)

#save(model1b_s_1bi, file = "mirtmodels/e_modele/model1b_s_1bi.RData")
load(file = "mirtmodels/e_modele/model1b_s_1bi.RData")

anova(model1a_s_1bi,model1b_s_1bi)


# --------
# Model 2: bifactor resilience and coping
# --------

# Initialize lists to store IRT itemsets and fit statistics
itemsets_IRT <- list()
itemsets_IRT_fit <- list()

# grouping procedure
factor_g0 <- c("Ablehnung (-)", "Ablenkung", "Aktives Coping", "Akzeptanz", "Behavioural disengagement (-)","Religion"  , 
               "Familiärer Zusammenhalt", "Humor", "Instrumentelle Unterstützung", "Kognitive Rekonstruierung", 
               "Mentale Anstrengung", "Moral, Ethik und Altruismus", "Optimismus", 
               "Positives Denken", "Selbstvorwurf (-)", "Selbstwahrnehmung", 
               "Sinn, Zweck/Bestimmung und Entwicklung", "Soziale Unterstützung", "Strukturiertheit", 
               "Umgang mit Angst", "Vorbilder", "Wunsch Denken (-)", "Zukunftsplanung")


factor_r1 <-  c("Familiärer Zusammenhalt", "Mentale Anstrengung", "Moral, Ethik und Altruismus","Optimismus", 
                "Physische Anstrengung", "Selbstwahrnehmung", "Sinn, Zweck/Bestimmung und Entwicklung",
                "Strukturiertheit","Umgang mit Angst", "Vorbilder","Zukunftsplanung")


factor_c2 <-  c("Soziale Unterstützung","Akzeptanz" ,"Instrumentelle Unterstützung"  ,"Ablenkung",
                "Ablehnung (-)"  ,"Humor" ,"Behavioural disengagement (-)", "Kognitive Rekonstruierung" ,
                "Wunsch Denken (-)" ,"Positives Denken","Aktives Coping" , "Selbstvorwurf (-)")



map_items_to_factor <- function(factor_facets, codebook)  {
  # Filtering the codebook based on the factor facets
  items_for_factor <- codebook$item_name_auswertung[codebook$name_facette %in% factor_facets]
  return(items_for_factor)
}

# Mapping items for each factor
items_g0 <- map_items_to_factor(factor_g0, codebook)
items_r1 <- map_items_to_factor(factor_r1, codebook)
items_c2 <- map_items_to_factor(factor_c2, codebook)


# Map your data structure to the example by Chalmers
# Extracting the item names from the dataset
all_items <- colnames(data_EGA)


# Create the 'specific' vector based on the item order in 'data_rcitems_3_noNA'
specific <- numeric(length(all_items))

for (i in seq_along(all_items)) {
  if (all_items[i] %in% items_r1) {
    specific[i] <- 1
  } else if (all_items[i] %in% items_c2) {
    specific[i] <- 2
  } else {
    # Assuming that items not in r1 or c2 are general items and not assigned to a specific factor.
    specific[i] <- 0
  }
}

specific

# Run the bfactor model 
#model2_bi <- bfactor(data_EGA, specific, technical = list(NCYCLES=2000)) 
#Iteration: 768, Log-Lik: -175721.312, Max-Change: 0.00010 (2024-05-21)


#save(model2_bi, file = "mirtmodels/e_modele/model2_bi.RData")
load(file = "mirtmodels/e_modele/model2_bi.RData")

# --------
# Model 3: Ayers Model
# --------


newcodebook <- codebook %>%
  filter(codebook$item_name_auswertung %in% names(data_EGA))

avoidance <- c("Behavioural disengagement (-)", "Ablehnung (-)" ,  "Selbstvorwurf (-)" ,
               "Umgang mit Angst"  ,"Wunsch Denken (-)")      
cognitive_reconstruction <- c("Akzeptanz" ,"Humor", "Positives Denken",    "Sinn, Zweck/Bestimmung und Entwicklung",
                              "Selbstwahrnehmung", "Optimismus"   ,"Kognitive Rekonstruierung")  
problem_solving <- c("Aktives Coping" , "Zukunftsplanung" )
distraction <- c("Ablenkung", "Soziale Unterstützung", "Physische Anstrengung", "Mentale Anstrengung") 
support_seeking <-  c("Instrumentelle Unterstützung", "Familiärer Zusammenhalt")  

# Mapping from factor names to item names
map_factor_to_items <- function(factor_list, newcodebook) {
  mapped_items <- newcodebook$item_name_auswertung[newcodebook$name_facette %in% factor_list]
  return(mapped_items)
}

# Map each factor list to corresponding item names
mapped_avoidance <- map_factor_to_items(avoidance, newcodebook)
mapped_cognitive_reconstruction <- map_factor_to_items(cognitive_reconstruction, newcodebook)
mapped_problem_solving <- map_factor_to_items(problem_solving, newcodebook)
mapped_distraction <- map_factor_to_items(distraction, newcodebook)
mapped_support_seeking <- map_factor_to_items(support_seeking, newcodebook)

# Create the 'specific' vector based on the item order in 'combined_df'
all_items <- colnames(data_EGA)

specific <- numeric(length(all_items))


for (i in seq_along(all_items)) {
  if (all_items[i] %in% mapped_avoidance) {
    specific[i] <- 1
  } else if (all_items[i] %in% mapped_cognitive_reconstruction) {
    specific[i] <- 2
  } else if (all_items[i] %in% mapped_problem_solving) {
    specific[i] <- 3
  } else if (all_items[i] %in% mapped_distraction) {
    specific[i] <- 4
  } else if (all_items[i] %in% mapped_support_seeking) {
    specific[i] <- 5
  } else {
    specific[i] <- 0
  }
}

specific

# Run the bfactor model 
#model3_ayers <- bfactor(data_EGA, specific, technical = list(NCYCLES=2000)) 
#Iteration: 1707, Log-Lik: -174307.895, Max-Change: 0.00010 (2024-05-21)


#save(model3_ayers, file = "mirtmodels/e_modele/model3_ayers.RData")
load(file = "mirtmodels/e_modele/model3_ayers.RData")



# --------
# Model 4: Kaiser Model with coping
# --------

#kaiser raw
selbstwahrnehmung <- c("Selbstwahrnehmung", "Optimismus", "Sinn, Zweck/Bestimmung und Entwicklung",
                       "Positives Denken", "Akzeptanz")   
zukunftsplanung <- c("Zukunftsplanung")
soziale_kompetenz <- c("Humor","Instrumentelle Unterstützung")
familiärer_zusammenhalt <- c("Familiärer Zusammenhalt")
soziale_ressourcen <- c("Soziale Unterstützung")  
#extension
coping <- c("Ablenkung",  "Kognitive Rekonstruierung"  , "Physische Anstrengung", "Mentale Anstrengung", "Aktives Coping", "Umgang mit Angst","Behavioural disengagement (-)" , "Ablehnung (-)"  , "Selbstvorwurf (-)"  ,"Wunsch Denken (-)")       


# Update the mapping function for new factors
mapped_selbstwahrnehmung <- map_factor_to_items(selbstwahrnehmung, newcodebook)
mapped_zukunftsplanung <- map_factor_to_items(zukunftsplanung, newcodebook)
mapped_soziale_kompetenz <- map_factor_to_items(soziale_kompetenz, newcodebook)
mapped_familiärer_zusammenhalt <- map_factor_to_items(familiärer_zusammenhalt, newcodebook)
mapped_soziale_ressourcen <- map_factor_to_items(soziale_ressourcen, newcodebook)
mapped_coping <- map_factor_to_items(coping, newcodebook)

# Create the 'specific' vector based on the item order in 'combined_df'
all_items <- colnames(data_EGA)
specific <- numeric(length(all_items))

for (i in seq_along(all_items)) {
  if (all_items[i] %in% mapped_soziale_ressourcen) {
    specific[i] <- 1
  } else if (all_items[i] %in% mapped_selbstwahrnehmung) {
    specific[i] <- 2
  } else if (all_items[i] %in% mapped_zukunftsplanung) {
    specific[i] <- 3
  } else if (all_items[i] %in% mapped_soziale_kompetenz) {
    specific[i] <- 4
  } else if (all_items[i] %in% mapped_familiärer_zusammenhalt) {
    specific[i] <- 5
  } else if (all_items[i] %in% mapped_coping) {
    specific[i] <- 6
  } else {
    specific[i] <- 0
  }
}
specific



# Run the bfactor model 
#model4_kaisercope <- bfactor(data_EGA, specific, technical = list(NCYCLES=2000))
#Iteration: 1132, Log-Lik: -174214.811, Max-Change: 0.00010 (2024-05-21)


#save(model4_kaisercope, file = "mirtmodels/e_modele/model4_kaisercope.RData")
load(file = "mirtmodels/e_modele/model4_kaisercope.RData")




# --------
# Model 5: Kaiser/Ayers Model 
# --------
#kaiser
selbstwahrnehmung <- c("Selbstwahrnehmung", "Optimismus", "Sinn, Zweck/Bestimmung und Entwicklung",
                       "Positives Denken", "Akzeptanz")   
zukunftsplanung <- c("Zukunftsplanung")
soziale_kompetenz <- c("Humor","Instrumentelle Unterstützung")
familiärer_zusammenhalt <- c("Familiärer Zusammenhalt")
soziale_ressourcen <- c("Soziale Unterstützung" )  
#ayers
distraction <- c("Ablenkung",  "Kognitive Rekonstruierung"  , "Mentale Anstrengung","Physische Anstrengung")
active_coping <- c("Aktives Coping", "Umgang mit Angst")
avoidance <- c("Behavioural disengagement (-)" , "Ablehnung (-)"  , "Selbstvorwurf (-)"  ,
               "Wunsch Denken (-)")       

# Mapping from factor names to item names
map_factor_to_items <- function(factor_list, newcodebook) {
  mapped_items <- newcodebook$item_name_auswertung[newcodebook$name_facette %in% factor_list]
  return(mapped_items)
}


# Update the mapping function for new factors
mapped_selbstwahrnehmung <- map_factor_to_items(selbstwahrnehmung, newcodebook)
mapped_zukunftsplanung <- map_factor_to_items(zukunftsplanung, newcodebook)
mapped_soziale_kompetenz <- map_factor_to_items(soziale_kompetenz, newcodebook)
mapped_familiärer_zusammenhalt <- map_factor_to_items(familiärer_zusammenhalt, newcodebook)
mapped_soziale_ressourcen <- map_factor_to_items(soziale_ressourcen, newcodebook)
mapped_distraction <- map_factor_to_items(distraction, newcodebook)
mapped_active_coping <- map_factor_to_items(active_coping, newcodebook)
mapped_avoidance <- map_factor_to_items(avoidance, newcodebook)

# Create the 'specific' vector based on the item order in 'combined_df'
all_items <- c(mapped_selbstwahrnehmung   
               ,mapped_zukunftsplanung           
               ,mapped_soziale_kompetenz      
               ,mapped_familiärer_zusammenhalt  
               ,mapped_soziale_ressourcen
               ,mapped_distraction
               ,mapped_active_coping
               ,mapped_avoidance)

all_items
cat(all_items)

specific <- numeric(length(all_items))


for (i in seq_along(all_items)) {
  if (all_items[i] %in% mapped_avoidance) {
    specific[i] <- 1
  } else if (all_items[i] %in% mapped_selbstwahrnehmung) {
    specific[i] <- 2
  } else if (all_items[i] %in% mapped_zukunftsplanung) {
    specific[i] <- 3
  } else if (all_items[i] %in% mapped_soziale_kompetenz) {
    specific[i] <- 4
  } else if (all_items[i] %in% mapped_familiärer_zusammenhalt) {
    specific[i] <- 5
  } else if (all_items[i] %in% mapped_soziale_ressourcen) {
    specific[i] <- 6
  } else if (all_items[i] %in% mapped_distraction) {
    specific[i] <- 7
  } else if (all_items[i] %in% mapped_active_coping) {
    specific[i] <- 8
  } else {
    specific[i] <- 0
  }
}
specific



# Run the bfactor model 
#model5_kaiserayers <- bfactor(data_EGA, specific, technical = list(NCYCLES=3000))
# Iteration: 2601, Log-Lik: -173561.729, Max-Change: 0.00010  (2024-05-29)

#save(model5_kaiserayers, file = "mirtmodels/e_modele/model5_kaiserayers.RData")
load(file = "mirtmodels/e_modele/model5_kaiserayers.RData")




# standardized factor loadings
tab <- summary(model5_kaiserayers)
bifactorloading_all_data <- as.data.frame(tab$rotF) 

#Focus on the secoundary dimension. With aim of including many theoretical factors contributing to the dimensional atrribute. 


# take out  based on content 
# statt selbw_1_322 we choose selbw_2_325 for better content valisity

# statt zukun_2_337 we choose zukun_2_344 for better content validity

# add behav_1_132_r for more emphesis on bahavior

# Hauptsache content valide!
rows_to_keep <- c(
  "behav_1_133_r", "behav_1_129_r", "behav_1_132_r" ,"wunsc_1_154_r", "selbv_1_198_r", "ableh_2_081_r",  #6 items
  "sinnz_2_312", "sinnz_1_300", "posit_1_167", "selbw_2_325", "akzep_3_019", "optim_2_211_r",            #6 items
  "zukun_1_339", "zukun_2_347", "zukun_2_344",                                                           #4 items
  "humor_1_082", "humor_1_087", "humor_1_085_r",                                                         #4 items
  "famil_1_373", "famil_1_371", "famil_1_378",                                                           #4 items
  "sozia_1_355", "sozia_1_038", "sozia_1_354",                                                           #4 items
  "physi_1_264", "physi_1_263", "ablen_1_048",                                                           #4 items
  "umgan_1_221", "umgan_1_222", "aktiv_1_180"                                                            #4 items
)







# Filter the DataFrame to keep only the specified rows
bifactorloading_select_data <- bifactorloading_all_data[rownames(bifactorloading_all_data) %in% rows_to_keep, ]

newcodebook_filtered <- newcodebook %>%
  filter(item_name_auswertung %in% rows_to_keep)



data_EGA_final_items <- data_EGA %>%
  select(rows_to_keep)




specific_groups <- list(
  SPEC1 = c("behav_1_133_r", "behav_1_129_r", "behav_1_132_r", "wunsc_1_154_r", "selbv_1_198_r", "ableh_2_081_r"),
  SPEC2 = c("sinnz_2_312", "sinnz_1_300", "posit_1_167", "selbw_2_325", "akzep_3_019", "optim_2_211_r"),
  SPEC3 = c("zukun_1_339", "zukun_2_347", "zukun_2_344"),
  SPEC4 = c("humor_1_082", "humor_1_087", "humor_1_085_r"),
  SPEC5 = c("famil_1_373", "famil_1_371", "famil_1_378"),
  SPEC6 = c("sozia_1_355", "sozia_1_038", "sozia_1_354"),
  SPEC7 = c("physi_1_264", "physi_1_263", "ablen_1_048"),
  SPEC8 = c("umgan_1_221", "umgan_1_222", "aktiv_1_180")
)


# Initialize an empty list to store the assignments
assignments <- list()

# Loop through each column of the dataframe
for (col_name in colnames(data_EGA_final_items)) {
  # Initialize the group value as NA
  group_value <- NA
  
  # Loop through each specific group
  for (i in seq_along(specific_groups)) {
    if (col_name %in% specific_groups[[i]]) {
      group_value <- i
      break
    }
  }
  
  # Store the assignment in the list
  assignments[[col_name]] <- group_value
}

specific <- as.numeric(assignments) 
# Print the assignments list
print(specific)




# Run the bfactor model 
#model5_kaiserayers_final <- bfactor(data_EGA_final_items, specific, technical = list(NCYCLES=3000))
# Iteration: 726, Log-Lik: -52994.808, Max-Change: 0.00010 (2024-05-31)


#save(model5_kaiserayers_final, file = "mirtmodels/e_modele/model5_kaiserayers_final.RData")
load(file = "mirtmodels/e_modele/model5_kaiserayers_final.RData")







# standardized factor loadings
tab <- summary(model5_kaiserayers_final)
bifactorloading_all_data <- as.data.frame(tab$rotF) 

# Unstandardized factor loadings
bifactorloading_all_data_unstan <- as.data.frame(coef(model5_kaiserayers_final, simplify = TRUE)$items) %>% ## Extract raw coefficients from the model, along with their standard errors and confi- dence intervals
          select(1:9)


# Create a new data frame with the desired columns
result <- data.frame(
  ag = bifactorloading_all_data_unstan$a1,
  as = apply(bifactorloading_all_data_unstan[, 2:9], 1, function(row) sum(row[row != 0]))
)


































library(rempsyc)
# Inspect bifactor loadings:
#summary() reports the standardised loadings, which take into account the group variance parameters.
tab <- summary(model5_kaiserayers_final)
tabu <- as.data.frame(tab$rotF)

# Add the tab$h2 column to tabu
tabu <- cbind(tabu, h2 = tab$h2)


#stand for reliability
saveRDS(tabu, file = "paper/Tables/result_stan.rds")


# Define a function to reorder columns from S8 to S1 sequentially by their highest to lowest absolute amount
reorder_columns <- function(data) {
  for (i in 9:2) {
    data <- data[order(-abs(data[, i]), data[, i]), ]
  }
  return(data)
}
# Apply the function to reorder the columns
tabu <- reorder_columns(tabu)

# Add an additional column with item numbers at position 1
tabu <- cbind(Item = 1:nrow(tabu), tabu)

# Round numeric columns except the first one to 2 digits
tabu[, -1] <- round(tabu[, -1], 2)
tabu[tabu == 0] <- ""


# Define column names with lambda symbol
column_names <- c("Item", "λG", "λS1", "λS2", "λS3", "λS4", "λS5", "λS6", "λS7", "λS8", "h2")

# Rename the columns
colnames(tabu) <- column_names

loadingtabu <- tabu


# Convert loadingtabu to a tibble and set row names as a separate column
loadingtabu <- loadingtabu %>%
  rownames_to_column(var = "Row.names")



#coef() reports the raw coefficients from the model, along with their standard errors and confidence intervals
testii <- as.data.frame(coef(model5_kaiserayers_final, simplify = TRUE)$items)[,10:15] 
# Merging data frames based on row names

# Convert testii to a tibble and set row names as a separate column
testii <- testii %>%
  rownames_to_column(var = "Row.names")

# Merge data frames based on row names
combinio <- full_join(loadingtabu, testii, by = "Row.names")





means <- colMeans(data_EGA) # i changerd this from M to means
sds <- apply(data_EGA, 2, sd) # i changed this from SD to sds

# Create a data frame with means and standard deviations for each item
item_stats <- data.frame(mean = means, sd = sds) %>%
  rownames_to_column(var = "Row.names")


# Merge data frames based on row names
combinio <- full_join(combinio, item_stats, by = "Row.names")


combinio <-  combinio %>%
  slice(1:30)


#add unstandaized parameter
combinio$ag <- rep(1:30)  # placeholder for a_g parameter 
combinio$as <- rep(1:30)  # placeholder for a_g parameter 


# overwrite combinio with true values
result <- result %>%
  mutate(Row.names_result = rownames(result))

combinio <- as_tibble(combinio)
result <- as_tibble(result)

str(combinio)
str(result)
combinio <- combinio %>%
  mutate(ag = case_when(
    Row.names %in% result$Row.names_result ~ result$ag[match(Row.names, result$Row.names_result)],
    TRUE ~ ag
  ),
  as = case_when(
    Row.names %in% result$Row.names_result ~ result$as[match(Row.names, result$Row.names_result)],
    TRUE ~ as
  ))



# Move column 11 to position 17
combinio <- combinio %>%
  select(Item, λG, λS1:λS8,ag,as, d1:d6, h2, mean, sd)



# Save the table to your directory

saveRDS(loadingtabu, file = "paper/Tables/Tablesloading.rds")
saveRDS(combinio, file = "paper/Tables/Tablesloading_test.rds")



# --------
# reliability
# --------

tabu <- readRDS("paper/Tables/result_stan.rds") # recovers the prior tabu state


# Calculate squared sum for each column except 'h2'
squared_sums <- sapply(tabu[, !names(tabu) %in% "h2"], function(column) {
  sum(column)^2
})

# Calculate the sum of (1 - value) for 'h2' column
h2_adjusted_sum <- sum(1 - tabu$h2)


w <- ( as.numeric(squared_sums[1]) + as.numeric(sum(squared_sums[2:9])) ) / ( as.numeric(squared_sums[1])  + as.numeric(sum(squared_sums[2:9])) + as.numeric(h2_adjusted_sum) )

wh <- as.numeric(squared_sums[1])/ (as.numeric(squared_sums[1]) + as.numeric(sum(squared_sums[2:9])) + as.numeric(h2_adjusted_sum))



# Define specific groups
specific_groups <- list(
  SPEC1 = c("behav_1_133_r", "behav_1_129_r", "behav_1_132_r", "wunsc_1_154_r", "selbv_1_198_r", "ableh_2_081_r"),
  SPEC2 = c("sinnz_2_312", "sinnz_1_300", "posit_1_167", "selbw_2_325", "akzep_3_019", "optim_2_211_r"),
  SPEC3 = c("zukun_1_339", "zukun_2_347", "zukun_2_344"),
  SPEC4 = c("humor_1_082", "humor_1_087", "humor_1_085_r"),
  SPEC5 = c("famil_1_373", "famil_1_371", "famil_1_378"),
  SPEC6 = c("sozia_1_355", "sozia_1_038", "sozia_1_354"),
  SPEC7 = c("physi_1_264", "physi_1_263", "ablen_1_048"),
  SPEC8 = c("umgan_1_221", "umgan_1_222", "aktiv_1_180")
)


# Create a list to store the subset data frames
grouped_tabu <- lapply(specific_groups, function(items) {
  # Subset the data frame
  subset_df <- tabu[rownames(tabu) %in% items, ]
  
  # Drop columns that contain only zeros
  subset_df <- subset_df[, colSums(subset_df != 0) > 0]
  
  # Ensure the data frame has only three columns
  if (ncol(subset_df) > 3) {
    subset_df <- subset_df[, 1:3]
  }
  
  # Rename the columns to "g", "s", "h"
  colnames(subset_df) <- c("g", "s", "h")
  
  return(subset_df)
})

# Name the elements of the list
names(grouped_tabu) <- names(specific_groups)



# Function to apply the formula
apply_formula <- function(df) {
  sum_g <- sum(as.numeric(df$g))
  sum_s <- sum(as.numeric(df$s))
  sum_h <- sum(as.numeric(1 - df$h))
  
  result <- (sum_s^2) / (sum_g^2 + sum_s^2 + sum_h)
  return(result)
}

# Apply the formula to each group
results <- lapply(grouped_tabu, apply_formula)



wh1 <-  results$SPEC1 #avoidance
wh2 <-  results$SPEC2 #self perception
wh3 <-  results$SPEC3 #zukunftsplanung
wh4 <-  results$SPEC4 #social competence
wh5 <-  results$SPEC5 #family cohesion
wh6 <-  results$SPEC6 #soziale_ressourcen
wh7 <-  results$SPEC7 #distraction
wh8 <-  results$SPEC8 #active coping




# --------
# Validity
# --------

# Determine the columns to keep from the first 105 columns
cols_to_drop <- setdiff(names(valicombi1_numeric)[1:105], rows_to_keep)
# Create the new dataset
valicombi2_numeric <- valicombi1_numeric %>%
  select(-all_of(cols_to_drop))


# Add prefix "rcq_" to the first 30 columns
new_column_names <- names(valicombi2_numeric)
new_column_names[1:30] <- paste0("rcq_", new_column_names[1:30])
names(valicombi2_numeric) <- new_column_names



vali_factors <- c("rcq","brcs", "rs11",
                  #"rsa", "cope",
                  "mccs", "swls", 
                  #"ffa",
                  "bfi_e" ,"bfi_v","bfi_g","bfi_n","bfi_o", "gse", "ppse", "gpsk"
                  #, "bokx"
                  )

# Combine factors for easier processing
all_factors <- c(vali_factors)


# Aggregate data by factor prefixes to calculate mean for each factor
factor_means <- sapply(all_factors, function(prefix) {
  cols <- grep(paste0("^", prefix), names(valicombi2_numeric))
  factor_data <- as.matrix(valicombi2_numeric[, cols])  # Explicitly convert to a matrix
  rowMeans(factor_data, na.rm = TRUE)  # Calculate the mean for each row (factor)
})

factor_means

# Convert the list to a data frame
factor_means_df <- as.data.frame(factor_means) %>%
  select("rcq", "brcs", "rs11" , "mccs", 
        "swls", "gse" , "bfi_e" , "bfi_n",
        "bfi_v", "bfi_g", "bfi_o", "ppse", "gpsk")


pairwiseCount(factor_means_df)
# Calculate the correlation matrix for the factor means
cor_matrix <- cor(factor_means_df, method = "pearson", use = "pairwise.complete.obs")

test <- apa.cor.table(factor_means_df,filename = "paper/Tables/Validation_corr_matrix.doc",table.number = 3,show.conf.interval = FALSE,  show.sig.stars = FALSE,  landscape = TRUE)

# Extract variable names
variable_names <- attr(test$table.body, "dimnames")[[2]]
values <- test$table.body
corr_results_f <- as.data.frame(values, stringsAsFactors = FALSE) # Create a dataframe
colnames(corr_results_f) <- variable_names # Assign column names


save(corr_results_f, file = "paper/Tables/corr_results_f.RData")



# --------
# Model Summary
# --------
#bifactor models
anova_results <- anova(model1a_s_1bi,
      model1b_s_1bi,
      model2_bi,
      model3_ayers,
      model4_kaisercope,
      model5_kaiserayers)



anova(model3_ayers,
      model5_kaiserayers)


#1 model1_exploratory
#2 model5_kaiserayers

coef(model1_exploratory, simplify=TRUE)
summary(model1_exploratory)
#itemfit(model1_exploratory, QMC=TRUE)
residuals(model1_exploratory, QMC=TRUE)



coef(model5_kaiserayers, simplify=TRUE)
summary(model5_kaiserayers) #loadings


# rerun the favored model










# --------
# paper export
# --------

#RCQ-L
RCQL <- codebook[codebook$item_name_auswertung %in% names(data_EGA), ]
RCQL$M <- NA
RCQL$SD <- NA
RCQL <- RCQL %>%
  select(ID = nr,
         `Itemtext (German)` = item_beschriftung_soscisurvey,
         Factor = name_facette,
         Subfactor = name_subskala,
         Inverse = zu_rekodieren,
         item_name_auswertung,
         M,  # Placeholder for mean
         SD)  # Placeholder for standard deviation

# Define German/English equivalents of Factor and Subfactor names
german_factor_names <- c("Ablehnung (-)", "Ablenkung", "Aktives Coping", "Akzeptanz", "Behavioural disengagement (-)", "Familiärer Zusammenhalt", "Humor", "Instrumentelle Unterstützung", "Kognitive Rekonstruierung", "Mentale Anstrengung", "Optimismus", "Physische Anstrengung", "Positives Denken", "Selbstvorwurf (-)", "Selbstwahrnehmung", "Sinn, Zweck/Bestimmung und Entwicklung", "Soziale Unterstützung", "Umgang mit Angst", "Wunsch Denken (-)", "Zukunftsplanung")
english_factor_names <- c("Rejection (-)", "Distraction", "Active Coping", "Acceptance", "Behavioral Disengagement (-)", "Family Cohesion", "Humor", "Instrumental Support", "Cognitive Restructuring", "Mental Effort", "Optimism", "Physical Effort", "Positive Thinking", "Self-Blame (-)", "Self-Perception", "Meaning, Purpose and Growth", "Social Support", "Dealing with Anxiety", "Wishful Thinking", "Future Planning")
german_subfactor_names <- c("selbst leugnen", "anderen die Schuld zuweisen", "an etwas anderes Denken/über etwas weniger Nachdenken", "aktiv werden", "lernen damit zu leben", "mit vergangenem abschließen", "Coping aufgeben", "Familiäre Kohärenz", "witze darüber machen", "instrumentelle Unterstützung", "Kognitive Flexibilität", "rekonstruieren", "Emotionale Flexibilität", "gedächtnis herausfordern", "Zuversicht", "realistischer Optimismus", "Fitness und Kraft", "positiv/optimistisch", "selbst schuld geben", "Selbstwirksamkeitserwartung und Selbstwert", "Hoffnung", "Bedeutung geben", "zweck verfolgen", "soziale Unterstützung", "Sozial Kompetenz", "Ängsten stellen", "wunsch nach veränderung", "wunsch nach wunder/fantasie", "Einstellungen zu Zukunftsaussichten", "Fähigkeit zukunftsorientiert zu planen")
english_subfactor_names <- c("self-blame", "blaming others", "distraction", "taking action", "learning to live with it", "closing the past", "giving up coping", "family coherence", "making jokes about it", "instrumental support", "cognitive flexibility", "reconstructing", "emotional flexibility", "challenging memory", "confidence", "realistic optimism", "fitness and strength", "thinking positively", "blaming oneself", "self-efficacy/self-esteem", "hope", "giving meaning", "pursuing goals", "social support", "social competence", "facing fears", "desire for change", "wish for miracles", "attitudes towards future prospects", "ability to plan for the future")

# Rename Factor and Subfactor columns in RCQL dataframe
RCQL$Factor <- factor(RCQL$Factor, levels = unique(RCQL$Factor), labels = english_factor_names)
RCQL$Subfactor <- factor(RCQL$Subfactor, levels = unique(RCQL$Subfactor), labels = english_subfactor_names)
# Order the dataframe based on Factor and Subfactor columns
RCQL <- RCQL[order(substr(RCQL$Factor, 1, 2), factor(RCQL$Subfactor, levels = unique(RCQL$Subfactor))), ] # must be done based on first letter otherwise it is not working because of the "(-)

RCQL <- RCQL %>%
  slice(5:9, 1:4, 10:n())

RCQL$ID <- 1:98

# Extract means and standard deviations from describe(data_EGA)
stats <- describe(data_EGA) %>% select(mean,sd) %>%
  rownames_to_column("item_name_auswertung")

# Update RCQL with mean and standard deviation columns
RCQL <- RCQL %>%
  left_join(stats, by = "item_name_auswertung") %>%
  mutate(M = round(mean, 2),
         SD = round(sd, 2)) %>%
  select(-mean, -sd)  # Remove redundant columns

# Replace spelling error
RCQL$`Itemtext (German)`[25] <- "Um meine Ängste zu überwinden, mache ich mir dafür notwendige Verhaltensweisen und Fähigkeiten bewusst und setze diese um."

rm(stats)

save(anova_results, file = "paper/Tables/anova_results.RData")

saveRDS(RCQL, file = "paper/Tables/RCQL.rds")

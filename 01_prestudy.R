'-------------------------------------------------------------------------------
                                    setup 
 ------------------------------------------------------------------------------'
### setup ######################################################################
library(openxlsx)     # Reads and writes Excel files without Java dependency
library(writexl)      # Exports data to Excel format (.xlsx)
library(rstudioapi)   # Sets directory and some more
library(parallel)     # Support for parallel computation in R
library(stats4)       # Provides basic statistical functions
library(tidyverse)    # A collection of packages for data manipulation and visualization
library(psych)        # Toolbox for psychometric analysis and data visualization
library(lavaan)       # Latent variable analysis, including SEM (Structural Equation Modeling)
library(GPArotation)  # Factor rotation methods for factor analysis
library(lattice)      # Trellis graphics for R, used for plotting grid graphics
library(mirt)         # Multidimensional Item Response Theory (MIRT) analysis
pacman::p_load(       # Tools for package management in R
  rio,                # Streamlined import and export of data
  naniar              # Tools to assess and visualize missing data
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
data_full <- read.xlsx(xlsxFile = "input/rcall.xlsx",
                       skipEmptyRows = FALSE)
data_rcitems <- read.xlsx(xlsxFile = "input/rcitems.xlsx",
                          skipEmptyRows = FALSE)
codebook <- read.xlsx(xlsxFile = "input/2024-01-05_Rc_items.xlsx")


'-------------------------------------------------------------------------------
                                  demografics
 ------------------------------------------------------------------------------'
### demografics ################################################################

demos <- data_full %>%
  select("age", "gender", "Taetigkeit", "Bildungsabschluss") %>%
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
    )
  )

# Convert age back to numeric for summary statistics, excluding "keine Angabe"
age_numeric <- demos %>%
  filter(age != "keine Angabe") %>%
  mutate(age = as.numeric(age))

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
bildungsabschluss_summary <- demos %>%
  group_by(Bildungsabschluss) %>%
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


# Combine all summaries into one report data frame
report <- list(
  age_summary = age_summary,
  gender_summary = gender_summary,
  taetigkeit_summary = taetigkeit_summary,
  bildungsabschluss_summary = bildungsabschluss_summary
  )

# Display the report
report


'-------------------------------------------------------------------------------
                           reverse troubled items
 ------------------------------------------------------------------------------'
### reverse troubled items #####################################################

# Identify columns to reverse, trimming whitespace and converting to lowercase for comparison
cols_to_reverse <- c("umgan_1_224", "relig_1_110", "relig_1_113", 
                     "optim_2_212_r", "relig_1_112", "ableh_1_069_r",
                     "selbv_1_195_r", "ablen_2_060_r", "optim_2_209_r",
                     "sozia_1_041_r", "relig_1_114", "instr_1_036",
                     "optim_2_211_r", "wunsc_1_150_r", "relig_1_111",
                     "akzep_2_015", "selbv_1_192_r", "relig_1_117")
#alpha(data_rcitems)
#Note, these reverse transformation is suggested from the data 

data_rcitems_1 <- data_rcitems
data_full_1 <- data_full

# Reverse values for identified columns
data_rcitems_1 <- data_rcitems %>%
  mutate(across(all_of(cols_to_reverse), ~ ifelse(. >= 0 & !is.na(.), 8 - ., .)))
data_full_1 <- data_full %>%
  mutate(across(all_of(cols_to_reverse), ~ ifelse(. >= 0 & !is.na(.), 8 - ., .)))

'-------------------------------------------------------------------------------
                         analysis of negative responses 
 ------------------------------------------------------------------------------'
### analysis of negative responses #############################################
# Replace negative values with NA
data_rcitems_2 <- data_rcitems_1 %>% 
  mutate(across(where(is.numeric), ~ifelse(. < 0, NA_real_, .)))
data_full_2 <- data_full_1 %>%
mutate(across(where(is.numeric), ~ifelse(. < 0, NA_real_, .)))
na_counts <- apply(data_rcitems_2, 2, function(x) sum(is.na(x))) # Count NA values per variable

# Calculate Percentage of NAs
total_responses <- nrow(data_rcitems_2)
na_percentage <- (na_counts / total_responses) * 100
threshold <- 5 # threshold
unsuitable_items <- names(na_percentage[na_percentage > threshold])
na_counts
na_percentage
unsuitable_items # 12 items

#order items based on factors
var_order <- codebook$item_name_auswertung
existing_vars <- var_order[var_order %in% names(data_rcitems_2)]
data_rcitems_2 <- data_rcitems_2 %>%
  select(all_of(existing_vars))
data_rcitems_2_filtered <- data_rcitems_2 %>% #consider only variables with na
  select_if(~ any(is.na(.)))

# Use of pacman package:
# based on https://epirhandbook.com/en/missing-data.html
pct_miss(data_rcitems_2) # Percent of ALL data frame values that are missing
pct_miss_case(data_rcitems_2)  # Percent of rows with any value missing
pct_complete_case(data_rcitems_2) # Percent of rows that are complete (no values missing)  

# Visualize the number (or %) of missing values in each column (for highest 50 variables)
top_n <- 50
most_missing <- data_rcitems_2 %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, n_miss) %>%
  top_n(n = top_n, wt = n_miss) %>%
  pull(variable)

data_rcitems_2_most_missing <- data_rcitems_2 %>%
  select(all_of(most_missing))

gg_miss_var <- gg_miss_var(data_rcitems_2_most_missing, show_pct = TRUE) + 
  theme_minimal(base_family = "Times", base_size = 12) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())
ggsave("output/plots/miss_per_item.png", gg_miss_var, height = 10, width = 5, units = "in", dpi = 300)


# Heatmap of missingness across the entire data frame
## Create the heatmap
heatmap_plot_full <- vis_miss(data_rcitems_2_filtered) + 
  coord_flip() +
  labs(title = "Missingness Heatmap (Excluding Complete Variables)") +
  theme_minimal(base_family = "Times", base_size = 12) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 3, color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())
ggsave("output/plots/missing_heatmap_white_background.png", heatmap_plot_full, height = 20, width = 10, units = "in", dpi = 300)


# Heatmap of missingness across factors
## Compute an aggregate missingness per factor excluding the 'name_facette' column
item_missingness <- data_rcitems_2 %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  gather(variable, missingness)
item_missingness_with_factor <- item_missingness %>%
  left_join(codebook, by = c("variable" = "item_name_auswertung"))
missingness_by_factor <- item_missingness_with_factor %>%
  group_by(name_facette) %>%
  summarise(aggregate_missingness = mean(missingness, na.rm = TRUE))
factor_missingness <- missingness_by_factor %>%
  rowwise() %>%
  mutate(aggregate_missingness = mean(c_across(-name_facette), na.rm = TRUE))
# factor_missingness 


## Visualize aggregated missingness in heatmap form
### Adjust the heatmap styling to match the provided plot
heatmap_plot_factor <- ggplot(factor_missingness, aes(x = name_facette, y = 1)) + 
  geom_tile(aes(fill = aggregate_missingness), colour = "white", height = 0.9) +
  scale_fill_gradient(low = "grey", high = "#006400", name = "Missingness Proportion") +
  labs(title = "Missingness Heatmap by Factor", x = "Factors", y = "") +
  theme_bw() +
  theme(
    text = element_text(family = "Times", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("output/plots/missing_heatmap_factorwise.png", heatmap_plot_factor, height = 5, width = 10, units = "in", dpi = 300)

'Religion items more frequently were not asnwered (missing not by random). This indicates, that the religion
factor is not replicated in our sample. We apply a cut-off criteria of 5%.
Items that have more than 5% missings are tracked (except optim_1_199, as it has exactly 5%)'

data_rcitems_3 <- data_rcitems_2


'in the following steps, we compute a mean substitution for the remaining NA values, 
as PCA and IRT analysis cant handle NAs properly. The EFA could be computed with NA 
values, however we do the mean substiution before, so we can keep the drift constant. (mean sub 
takes out variance but keeps the mean) '

# perform mean substitution 
data_rcitems_3_noNA <- data_rcitems_3 %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

rm(data_rcitems_3,data_rcitems_2_most_missing, data_rcitems_2_filtered, data_rcitems_1, data_rcitems)
'-------------------------------------------------------------------------------
                  factor analysis under different conditions
 ------------------------------------------------------------------------------'
### factor analysis ############################################################
#### EFA #######################################################################
# EFA with 1 factor
# Initialize a list to store EFA itemsets
itemsets_EFA <- list()
itemsets_EFA_fit <- list()
EFA01 <- fa(data_rcitems_3_noNA, nfactors = 1, n.obs = 498) # Fit the EFA model
loadings_matrix <- EFA01$loadings # Extract factor loadings
itemsets_EFA$loadings_EFA01 <- as.data.frame(as.matrix(EFA01$loadings)) # Convert loadings matrix to a data frame
itemsets_EFA$loadings_EFA01$item <- rownames(EFA01$loadings)
itemsets_EFA_fit$fitstat_EFA01 <- data.frame( # Create a data frame to store the fit results
  CHI2 =   EFA01$STATISTIC,
  RMSR = EFA01$rms,
  VAR = rowSums(EFA01$Vaccounted["Proportion Var", , drop = FALSE]),
  DF = EFA01[["dof"]])

# EFA with 2 factors up to 26 factors (loop)
for (i in 2:26) {
  # Create dynamic names for list elements
  promax_name <- paste0("loadings_EFA", sprintf("%02d", i), "a_p")
  oblimin_name <- paste0("loadings_EFA", sprintf("%02d", i), "a_o")
  fitstat_promax_name <- paste0("fitstat_EFA", sprintf("%02d", i), "_p")
  fitstat_oblimin_name <- paste0("fitstat_EFA", sprintf("%02d", i), "_o")
  # Promax
  EFA_promax <- fa(data_rcitems_3_noNA, nfactors = i, rotate = "promax", n.obs = 498) # Perform EFA with Promax rotation
  promax_df <- data.frame(matrix(as.numeric(EFA_promax$loadings), nrow(EFA_promax$loadings), ncol(EFA_promax$loadings), dimnames = attr(EFA_promax$loadings, "dimnames"))) # Convert loadings to a data frame
  itemsets_EFA[[promax_name]] <- promax_df # Add the data frame to the list
    itemsets_EFA_fit[[fitstat_promax_name]] <- data.frame( # Store fit statistics for Promax rotation
    CHI2 =   EFA_promax$STATISTIC,
    RMSR = EFA_promax$rms,
    VAR = rowSums(EFA_promax$Vaccounted["Proportion Var", , drop = FALSE]),
    DF = EFA_promax[["dof"]])
  # Oblim
  EFA_oblimin <- fa(data_rcitems_3_noNA, nfactors = i, rotate = "oblimin", n.obs = 498)   # Perform EFA with Oblimin rotation
  oblimin_df <- data.frame(matrix(as.numeric(EFA_oblimin$loadings), nrow(EFA_oblimin$loadings), ncol(EFA_oblimin$loadings), dimnames = attr(EFA_oblimin$loadings, "dimnames")))   # Convert loadings to a data frame
  itemsets_EFA[[oblimin_name]] <- oblimin_df   # Add the data frame to the list
  itemsets_EFA_fit[[fitstat_oblimin_name]] <- data.frame(   # Store fit statistics for Oblimin rotation
    CHI2 =   EFA_oblimin$STATISTIC,
    RMSR = EFA_oblimin$rms,
    VAR = rowSums(EFA_oblimin$Vaccounted["Proportion Var", , drop = FALSE]),
    DF = EFA_oblimin[["dof"]]
  )
}
#warnings()   
#Note, that there are several Convergence issues, which we ignored

#### PCA #######################################################################
# Initialize a list to store EFA itemsets
itemsets_PCA <- list()
itemsets_PCA_fit <- list()
R <- cor(data_rcitems_3_noNA) # cor matrix

#PCA with one factor
PCA01 <- principal(R, n.obs = 498) # Fit the EFA model
loadings_matrix <- PCA01$loadings # Extract factor loadings
itemsets_PCA$loadings_PCA01 <- as.data.frame(as.matrix(PCA01$loadings)) # Convert loadings matrix to a data frame
itemsets_PCA$loadings_PCA01$item <- rownames(PCA01$loadings) # Add row names as a new column to identify items
itemsets_PCA_fit$fitstat_PCA01 <- data.frame( #save modelfits into list
  CHI2 =   PCA01$STATISTIC,
  RMSR = PCA01$rms,
  VAR = rowSums(PCA01$Vaccounted["Proportion Var", , drop = FALSE]),
  DF = PCA01[["dof"]]
)

# PCA with 2 factors up to 26 factors (loop)
for (i in 2:26) {
  # Create dynamic names for list elements
  promax_name <- paste0("loadings_PCA", sprintf("%02d", i), "b_p")
  oblimin_name <- paste0("loadings_PCA", sprintf("%02d", i), "b_o")
  fitstat_promax_name <- paste0("fitstat_PCA", sprintf("%02d", i), "_p")
  fitstat_oblimin_name <- paste0("fitstat_PCA", sprintf("%02d", i), "_o")  
  factor_names <- paste0("MR", 1:i) # Generate column names for factor loadings
  # Promax
  PCA_promax <- principal(R, nfactors = i, n.obs = 498, rotate = "promax") # Perform PCA with Promax rotation
  promax_df <- data.frame(matrix(as.numeric(PCA_promax$loadings), nrow(PCA_promax$loadings), ncol(PCA_promax$loadings), dimnames = attr(PCA_promax$loadings, "dimnames"))) # Convert loadings to a data frame
  colnames(promax_df) <- factor_names
  itemsets_PCA[[promax_name]] <- promax_df # Add the data frame to the list
   itemsets_PCA_fit[[fitstat_promax_name]] <- data.frame( # Store fit statistics for Promax rotation
    CHI2 =   PCA_promax$STATISTIC,
    RMSR = PCA_promax$rms,
    VAR = rowSums(PCA_promax$Vaccounted["Proportion Var", , drop = FALSE]),
    DF = PCA_promax[["dof"]])
   # Oblim
   PCA_oblimin <- principal(R, nfactors = i, n.obs = 498, rotate = "oblimin")
  oblimin_df <- data.frame(matrix(as.numeric(PCA_oblimin$loadings), nrow(PCA_oblimin$loadings), ncol(PCA_oblimin$loadings), dimnames = attr(PCA_oblimin$loadings, "dimnames"))) # Convert loadings to a data frame
  colnames(oblimin_df) <- factor_names
  itemsets_PCA[[oblimin_name]] <- oblimin_df # Add the data frame to the list
itemsets_PCA_fit[[fitstat_oblimin_name]] <- data.frame( # Store fit statistics for Promax rotation
    CHI2 =   PCA_oblimin$STATISTIC,
    RMSR = PCA_oblimin$rms,
    VAR = rowSums(PCA_oblimin$Vaccounted["Proportion Var", , drop = FALSE]),
    DF = PCA_oblimin[["dof"]]
  )
}


#### BiPCA #####################################################################
#see Beaujean, Alexander. (2014). R syntax to accompany Best Practices in Exploratory Factor Analysis} (2014) by Jason Osborne. https://www.researchgate.net/publication/264789997_R_syntax_to_accompany_Best_Practices_in_Exploratory_Factor_Analysis_2014_by_Jason_Osborne

# Initialize a list to store EFA itemsets
itemsets_PCAbifactor <- list()
itemsets_PCAbifactor_fit <- list()
for (i in 3:27) {
  bifactor_name <- paste0("loadings_PCAbifactor", sprintf("%03d", i), "_b")   # Create dynamic names for list elements
  fitstat_bifactor_name <- paste0("fitstat_PCAbifactor", sprintf("%03d", i), "_b")   # Dynamic names for fit statistics
  factor_names <- paste0("MR", 1:i) # Generate column names for factor loadings
  PCAbifactor <- principal(R, nfactors = i, n.obs = 498, rotate = "bifactor") # Perform PCA with Bifactor rotation
  bifactor_df <- data.frame(matrix(as.numeric(PCAbifactor$loadings), nrow(PCAbifactor$loadings), ncol(PCAbifactor$loadings), dimnames = attr(PCAbifactor$loadings, "dimnames"))) # Convert loadings to a data frame
  colnames(bifactor_df) <- factor_names
  itemsets_PCAbifactor[[bifactor_name]] <- bifactor_df # Add the data frame to the list
  itemsets_PCAbifactor_fit[[fitstat_bifactor_name]] <- data.frame( # Store fit statistics for Bifactor rotation
    CHI2 =   PCAbifactor$STATISTIC,
    RMSR = PCAbifactor$rms,
    VAR = rowSums(PCAbifactor$Vaccounted["Proportion Var", , drop = FALSE]),
    DF = PCAbifactor[["dof"]])
}


 
#### MIRT ######################################################################
# Initialize lists to store IRT itemsets and fit statistics
itemsets_IRT <- list()
itemsets_IRT_fit <- list()
all_items <- colnames(data_rcitems_3_noNA)

'Note, that computational demands of mirt models typically increase with the number 
of factors specified. The complexities arise due to the iterative procedures
that optimize the likelihood function. As the number of factors increases, more
parameters neeed to be estimates making the optimization process more complex. and, hence,
more time consuming. 
the code is commented out. Note, that these models took
several computers (macbook air 2020, M1 ship) and days to compute. 
Do not rerun unintensionally ' 

#irt_01non <- mirt(data = data_rcitems_3_noNA, 1,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM') 
#irt_02non <- mirt(data = data_rcitems_3_noNA, 2,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_03non <- mirt(data = data_rcitems_3_noNA, 3,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_04non <- mirt(data = data_rcitems_3_noNA, 4,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_05non <- mirt(data = data_rcitems_3_noNA, 5,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_06non <- mirt(data = data_rcitems_3_noNA, 6,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_07non <- mirt(data = data_rcitems_3_noNA, 7,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM') 
#irt_08non <- mirt(data = data_rcitems_3_noNA, 8,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM') 
#irt_09non <- mirt(data = data_rcitems_3_noNA, 9,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')   
#irt_10non <- mirt(data = data_rcitems_3_noNA, 10,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')    
#irt_11non <- mirt(data = data_rcitems_3_noNA, 11,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_12non <- mirt(data = data_rcitems_3_noNA, 12,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM') 
#irt_13non <- mirt(data = data_rcitems_3_noNA, 13,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM') 
#irt_14non <- mirt(data = data_rcitems_3_noNA, 14,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM') 
#irt_15non <- mirt(data = data_rcitems_3_noNA, 15,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_16non <- mirt(data = data_rcitems_3_noNA, 16,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_17non <- mirt(data = data_rcitems_3_noNA, 17,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_18non <- mirt(data = data_rcitems_3_noNA, 18,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_19non <- mirt(data = data_rcitems_3_noNA, 19,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_20non <- mirt(data = data_rcitems_3_noNA, 20,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_21non <- mirt(data = data_rcitems_3_noNA, 21,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_22non <- mirt(data = data_rcitems_3_noNA, 22,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_23non <- mirt(data = data_rcitems_3_noNA, 23,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_24non <- mirt(data = data_rcitems_3_noNA, 24,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_25non <- mirt(data = data_rcitems_3_noNA, 25,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')
#irt_26non <- mirt(data = data_rcitems_3_noNA, 26,itemtype = "graded", method = 'QMCEM', SE.type = 'MHRM')

#save(irt_01non, file = "mirtmodels/irt_01non.RData")
#save(irt_02non, file = "mirtmodels/irt_02non.RData")
#save(irt_03non, file = "mirtmodels/irt_03non.RData")
#save(irt_04non, file = "mirtmodels/irt_04non.RData")
#save(irt_05non, file = "mirtmodels/irt_05non.RData")
#save(irt_06non, file = "mirtmodels/irt_06non.RData")
#save(irt_07non, file = "mirtmodels/irt_07non.RData")
#save(irt_08non, file = "mirtmodels/irt_08non.RData")
#save(irt_09non, file = "mirtmodels/irt_09non.RData")
#save(irt_10non, file = "mirtmodels/irt_10non.RData")
#save(irt_11non, file = "mirtmodels/irt_11non.RData")
#save(irt_12non, file = "mirtmodels/irt_12non.RData")
#save(irt_13non, file = "mirtmodels/irt_13non.RData")
#save(irt_14non, file = "mirtmodels/irt_14non.RData")
#save(irt_15non, file = "mirtmodels/irt_15non.RData")
#save(irt_16non, file = "mirtmodels/irt_16non.RData")
#save(irt_17non, file = "mirtmodels/irt_17non.RData")
#save(irt_18non, file = "mirtmodels/irt_18non.RData")
#save(irt_19non, file = "mirtmodels/irt_19non.RData")
#save(irt_20non, file = "mirtmodels/irt_20non.RData")
#save(irt_21non, file = "mirtmodels/irt_21non.RData")
#save(irt_22non, file = "mirtmodels/irt_22non.RData")
#save(irt_23non, file = "mirtmodels/irt_23non.RData")
#save(irt_24non, file = "mirtmodels/irt_24non.RData")
#save(irt_25non, file = "mirtmodels/irt_25non.RData")
#save(irt_26non, file = "mirtmodels/irt_26non.RData")

# load the IRT models
load("mirtmodels/irt_01non.RData")
load("mirtmodels/irt_02non.RData")
load("mirtmodels/irt_03non.RData")
load("mirtmodels/irt_04non.RData")
load("mirtmodels/irt_05non.RData")
load("mirtmodels/irt_06non.RData")
load("mirtmodels/irt_07non.RData")
load("mirtmodels/irt_08non.RData")
load("mirtmodels/irt_09non.RData") 
load("mirtmodels/irt_10non.RData")
load("mirtmodels/irt_11non.RData")
load("mirtmodels/irt_12non.RData") 
load("mirtmodels/irt_13non.RData")
load("mirtmodels/irt_14non.RData")
load("mirtmodels/irt_15non.RData") 
load("mirtmodels/irt_16non.RData") 
load("mirtmodels/irt_17non.RData")
load("mirtmodels/irt_18non.RData")
load("mirtmodels/irt_19non.RData")
load("mirtmodels/irt_20non.RData") 
load("mirtmodels/irt_21non.RData") 
load("mirtmodels/irt_22non.RData") 
load("mirtmodels/irt_23non.RData") 
load("mirtmodels/irt_24non.RData") 
load("mirtmodels/irt_25non.RData") 
load("mirtmodels/irt_26non.RData") 


# Get all object names that match the pattern "irt_**non"
object_names <- ls(pattern = "irt_\\d+non") 
for (obj_name in object_names) { # Iterate over these object names
  current_obj <- get(obj_name) # Get the current object based on its name
  loadings_matrix <- current_obj@Fit$F # Extract the loadings matrix
  loadings_df <- as.data.frame(as.matrix(loadings_matrix))
  loadings_df$item <- rownames(loadings_matrix)
  loading_name <- paste0("loadings_", obj_name)  # Create a name for the loading and store it in the list
  itemsets_IRT[[loading_name]] <- loadings_df
  fit_df <- data.frame( # Extracting fit statistics
    CHI2 = current_obj@Fit$G2,
    RMSEA = current_obj@Fit$RMSEA,
    TLI = current_obj@Fit$TLI,
    CFI = current_obj@Fit$CFI,
    AIC = current_obj@Fit$AIC,
    BIC = current_obj@Fit$BIC,
    DF = current_obj@Fit$df)
  fit_name <- paste0("fit_", obj_name)
  itemsets_IRT_fit[[fit_name]] <- fit_df
}


#export results
# Save the lists as RData files
save(itemsets_IRT, file = "output/itemsets_IRT.RData")
save(itemsets_IRT_fit, file = "output/itemsets_IRT_fit.RData")

'-------------------------------------------------------------------------------
                                   itemsets
 ------------------------------------------------------------------------------'
### itemsets ###################################################################

mycutoff=0.5
# Lists containing EFA, PCA and PCA bifactor
analysis_lists <- list(EFA = itemsets_EFA, PCA = itemsets_PCA, PCAbifactor = itemsets_PCAbifactor)
high_loading_items <- c() # Initialize an empty vector to store variable names

# Loop through each analysis list (EFA, PCA, and PCAbifactor)
extract_high_loading_items <- readRDS("functions/extract_high_loading_items.RDS") # load functions that pick high loading items
for (analysis in names(analysis_lists)) {
  current_list <- analysis_lists[[analysis]]
  for (df_name in names(current_list)) {
    df <- current_list[[df_name]]
    high_loading_items <- c(high_loading_items, extract_high_loading_items(df, mycutoff))
  }
}

#Before including IRT sets, one minor correction, as we used different itemnames in prior analysis.
# List of old item names
old_names <- c("selbs_1_190_r", "selbs_1_191_r", "selbs_1_192_r", "selbs_1_193_r", "selbs_1_194_r", 
               "selbs_1_195_r", "selbs_1_196_r", "selbs_1_197_r", "selbs_1_198_r", "selbs_1_316", 
               "selbs_1_317_r", "selbs_1_318", "selbs_1_319", "selbs_1_320_r", "selbs_1_321", 
               "selbs_1_322", "selbs_1_323_r", "selbs_1_324_r", "selbs_1_334_r", "selbs_1_335_r", 
               "selbs_2_325", "selbs_2_326", "selbs_2_327", "selbs_2_328", "selbs_2_329", 
               "selbs_2_330", "selbs_2_331", "selbs_2_332_r", "selbs_2_333")
# List of new item names
new_names <- c("selbv_1_190_r", "selbv_1_191_r", "selbv_1_192_r", "selbv_1_193_r", "selbv_1_194_r", 
               "selbv_1_195_r", "selbv_1_196_r", "selbv_1_197_r", "selbv_1_198_r", "selbw_1_316", 
               "selbw_1_317_r", "selbw_1_318", "selbw_1_319", "selbw_1_320_r", "selbw_1_321", 
               "selbw_1_322", "selbw_1_323_r", "selbw_1_324_r", "selbw_1_334_r", "selbw_1_335_r", 
               "selbw_2_325", "selbw_2_326", "selbw_2_327", "selbw_2_328", "selbw_2_329", 
               "selbw_2_330", "selbw_2_331", "selbw_2_332_r", "selbw_2_333")

# Create a named vector for easy replacement
name_map <- setNames(new_names, old_names)
replace_names <- readRDS("functions/replace_names.RDS") # load functions that change names
# Loop through each data frame in the list and update the 'item' column and row names
for (i in seq_along(itemsets_IRT)) {
  df <- itemsets_IRT[[i]]
  df <- replace_names(df, name_map)
  itemsets_IRT[[i]] <- df
}

# include IRT itemsets in the count. Caution, dont rerun this, as it append to the existing count from above. 
# Process itemsets_IRT
for (df_name in names(itemsets_IRT)) { # Loop through IRT itemsets
  df <- itemsets_IRT[[df_name]]
  high_loading_items <- c(high_loading_items, extract_high_loading_items(df, mycutoff))
}
high_loading_items_IRT_1 <- unique(high_loading_items) # Extract unique variable names


### this following code counts the number of factors, it had a loading above .5. (amount) 
### E.g.,item akzep_3_019 has a frequency of 109. which means, that it had
### a factor loading of .5 or higher in 109. factors of all inspected models. 
# Using the high_loading_items vector (from previous solution), calculate frequencies
item_counts_5 <- table(high_loading_items)
upper_quartile_value <- quantile(as.numeric(item_counts_5), 0.75) # Calculate the 0.75 quantile of the counts

# Extract item names with counts below the 0.75 quantile
items_below_upper_quartile_5 <- names(item_counts_5)[which(as.numeric(item_counts_5) < upper_quartile_value)]
items_below_upper_quartile_5 # this list indicate items loaded above .5, but not as frequent as others ( below .75 quantile)
items_below_upper_quartile_5_good <-setdiff(all_items, items_below_upper_quartile_5)

#Version two with low end (.1 loading)
mycutoff=0.1
# Lists containing EFA, PCA and PCA bifactor
analysis_lists <- list(EFA = itemsets_EFA, PCA = itemsets_PCA, PCAbifactor = itemsets_PCAbifactor)
high_loading_items <- c() # Initialize an empty vector to store variable names

# Loop through each analysis list (EFA, PCA, and PCAbifactor)
for (analysis in names(analysis_lists)) {
  current_list <- analysis_lists[[analysis]]
  for (df_name in names(current_list)) {
    df <- current_list[[df_name]]
    high_loading_items <- c(high_loading_items, extract_high_loading_items(df, mycutoff))
  }
}

# Loop through IRT itemsets
for (df_name in names(itemsets_IRT)) {
  df <- itemsets_IRT[[df_name]]
  high_loading_items <- c(high_loading_items, extract_high_loading_items(df, mycutoff))
}

high_loading_items_IRT_1 <- unique(high_loading_items) # Extract unique variable names


### this following code counts the number of factors, it had a loading above .5. (amount) 
### E.g., item sinnz_2_311 has a frequency of 147. which means, that it had
### a factor loading of .5 or higher in 147 factors. 
# Using the high_loading_items vector (from previous solution), calculate frequencies
item_counts_1 <- table(high_loading_items)
item_counts_1

# Calculate the 0.75 quantile of the counts
upper_quartile_value <- quantile(as.numeric(item_counts_1), 0.75)

# Extract item names with counts below the 0.75 quantile
items_below_upper_quartile_1 <- names(item_counts_1)[which(as.numeric(item_counts_1) < upper_quartile_value)]
items_below_upper_quartile_1 #these indicate good items! (frequently not loaded under .1)
items_below_upper_quartile_1_good <- items_below_upper_quartile_1
items_below_upper_quartile_1 <-setdiff(all_items, items_below_upper_quartile_1_good)

#adjust to apply to version 1 or version 2, plot
# Convert the table to a dataframe
df_item_counts <- as.data.frame(item_counts_1)        
colnames(df_item_counts) <- c("Item", "Frequency")    
df_item_counts <- df_item_counts[order(-df_item_counts$Frequency),] # Arrange the dataframe by descending frequency
comparisonlist <- df_item_counts

# Initialize an empty data frame with the necessary columns
final_df <- data.frame(type = character(),
                       number_of_factors = numeric(),
                       rotation = character(),
                       CHI2 = numeric(),
                       RMSR = numeric(),
                       VAR = numeric(),
                       DF = numeric(),
                       stringsAsFactors = FALSE)

# Append data from PCA and EFA fit lists
append_to_df <- readRDS("functions/append_to_df.RDS") # load functions that appends to df 
append_to_df(itemsets_PCA_fit, "PCA")
append_to_df(itemsets_EFA_fit, "EFA")

# Rename columns
colnames(final_df) <- c("type", "number_of_factors", "rotation", "CHI2", "RMSR", "VAR", "DF")
final_df$number_of_factors <- as.numeric(final_df$number_of_factors)
final_df$CHI2 <- as.numeric(final_df$CHI2)
final_df$RMSR <- as.numeric(final_df$RMSR)
final_df$VAR <- as.numeric(final_df$VAR)
final_df$DF <- as.numeric(final_df$DF)
final_df <- final_df %>%
  filter(rotation %in% c("oblimin", NA)) %>%
  select(-rotation)
itemlist_EFA_PCA <- final_df 
  
#EXPORT TO PAPER FILE
save(itemlist_EFA_PCA, file = "output/itemlist_EFA_PCA.Rdata") # I move these to paper later
save(itemsets_PCAbifactor_fit, file = "output/itemsets_PCAbifactor_fit.Rdata")

rm(list = ls(pattern = "^irt_.*non$"), EFA_promax, EFA_oblimin, EFA01, PCA_oblimin, PCA_promax, PCA01, PCAbifactor)
   
'-------------------------------------------------------------------------------
                                 corr staticstic
 ------------------------------------------------------------------------------'
### corr staticstic ############################################################
relevant_codebook <- codebook[codebook$item_name_auswertung %in% colnames(data_rcitems_2), ]
  
# Group items by their factor from codebook
grouped_data <- sapply(unique(relevant_codebook$name_facette), function(factor_name) {
    items_in_factor <- relevant_codebook$item_name_auswertung[relevant_codebook$name_facette == factor_name]
    rowMeans(data_rcitems_2[, items_in_factor], na.rm = TRUE)})
cor_matrix_grouped <- cor(grouped_data, method = "pearson") # Compute Pearson's correlation matrix for the grouped factors
  
##### between RC factors #######################################################
# Group items by their factor from the codebook
  grouped_data <- sapply(unique(relevant_codebook$name_facette), function(factor_name) {
    items_in_factor <- relevant_codebook$item_name_auswertung[relevant_codebook$name_facette == factor_name]
    rowMeans(data_rcitems_2[, items_in_factor], na.rm = TRUE)})
pearson_matrix <- cor(grouped_data, method = "pearson") # Pearson correlation matrix
  
##### between RC subfactors ####################################################
# Group items by their factor from codebook
grouped_data <- sapply(unique(relevant_codebook$name_subskala), function(factor_name) {
  items_in_factor <- relevant_codebook$item_name_auswertung[relevant_codebook$name_subskala == factor_name]
  rowMeans(data_rcitems_2[, items_in_factor], na.rm = TRUE)})
pearson_matrix <- cor(grouped_data, method = "pearson") # Pearson correlation matrix
  
##### between validity items ###################################################
# Extracting required columns
cols_from_rcitems <- colnames(data_rcitems_2)
cols_BF01 <- grep("^BF01_", colnames(data_full_2), value = TRUE)
cols_RA <- grep("^rsa_", colnames(data_full_2), value = TRUE)
cols_cope <- grep("^cope_", colnames(data_full_2), value = TRUE)
cols_VA01_to_VA10 <- grep("^VA0[1-9]|^VA10", colnames(data_full_2), value = TRUE)
  
# Combine all column names
all_cols <- c(cols_from_rcitems, cols_BF01, cols_RA,cols_cope, "age", "gender", "Bildungsabschluss", "Familienstand", "Taetigkeit", 
                "Semester", "Studienrichtung", "Alleinlebend", "Bewohner_Haushalt", "Ehrenamt", 
                "Religiositaet", "Psychotherapeuten_Unterstuetzung", "Soziooekonomischen_Status", cols_VA01_to_VA10)
selected_data <- data_full_2[, all_cols]  # Extract the data
names_to_replace <- c("VA01" = "SWLS", "VA02" = "P-PSE", "VA03" = "MCCS", "VA04" = "BRSC",  # Renaming columns one by one
                         "VA06" = "RS11_", "VA07" = "FFA", "VA08" = "BOKX",
                        "VA09" = "GPS-K", "VA10" = "GSE")
for (old_name in names(names_to_replace)) {
    new_name <- names_to_replace[old_name]
    colnames(selected_data) <- gsub(paste0("^", old_name, "_"), new_name, colnames(selected_data))
}

selected_data[] <- lapply(selected_data, function(col) { # Convert all columns to numeric if they are not already
    if (is.character(col)) {
      return(ifelse(grepl("^[0-9]+$", col), as.numeric(col), NA))
    } else {
      return(col)
    }
})

# Handle NA values
selected_data[selected_data %in% c(-1, -9)] <- NA
selected_data[is.na(selected_data)] <- NA
selected_data <- as.data.frame(selected_data)

#Ordering
factor_subfactor_pairs <- unique(relevant_codebook[, c("name_facette", "name_subskala")]) # Extract factor and subfactor information from the codebook
sorted_factor_subfactor_pairs <- factor_subfactor_pairs[order(factor_subfactor_pairs$name_facette, factor_subfactor_pairs$name_subskala), ] # Sort factor-subfactor pairs
ordered_item_vars <- unlist(lapply(1:nrow(sorted_factor_subfactor_pairs), function(i) { # Generate ordered item variable names based on sorted factor-subfactor pairs
    factor_name <- as.character(sorted_factor_subfactor_pairs$name_facette[i])
    subfactor_name <- as.character(sorted_factor_subfactor_pairs$name_subskala[i])
    return(colnames(data_rcitems_2)[grep(paste0("^", factor_name, "_", subfactor_name), colnames(data_rcitems_2))])
}))  
  
## Group and Order Item Variables
factor_subfactor_pairs <- unique(relevant_codebook[, c("name_facette", "name_subskala")]) ## Extract factor and subfactor information from the codebook
## Sort factor-subfactor pairs  sorted_factor_subfactor_pairs <- factor_subfactor_pairs[order(factor_subfactor_pairs$name_facette, factor_subfactor_pairs$name_subskala), ]
ordered_item_vars <- unlist(lapply(1:nrow(sorted_factor_subfactor_pairs), function(i) { ## Generate ordered item variable names based on sorted factor-subfactor pairs
    factor_name <- as.character(sorted_factor_subfactor_pairs$name_facette[i])
    subfactor_name <- as.character(sorted_factor_subfactor_pairs$name_subskala[i])
    return(colnames(data_rcitems_2)[grep(paste0("^", factor_name, "_", subfactor_name), colnames(data_rcitems_2))])
}))
## Add the Validity Scales
ordered_validity_scales <- unlist(lapply(names_to_replace, function(scale_prefix) { ## Extract ordered validity scale names
    return(grep(paste0("^", scale_prefix), colnames(selected_data), value = TRUE))
}))
## Add the Remaining Variables
remaining_vars <- c("age", "gender", "Bildungsabschluss", "Familienstand", "Taetigkeit", 
                      "Semester", "Studienrichtung", "Alleinlebend", "Bewohner_Haushalt", "Ehrenamt", 
                      "Religiositaet", "Psychotherapeuten_Unterstuetzung", "Soziooekonomischen_Status")
final_order <- c(ordered_item_vars, ordered_validity_scales,cols_BF01, cols_RA,cols_cope, remaining_vars) ## Combine all variables in the desired order
if (length(final_order) != ncol(selected_data)) { # Ensure all variables are included in the final order
    missing_vars <- setdiff(colnames(selected_data), final_order)
    final_order <- c(final_order, missing_vars)
}

## calculate   
cor_matrix<- cor(selected_data, method = "pearson",use = "pairwise.complete.obs") # calculate   
cor_matrix_ordered <- cor_matrix[final_order, final_order] # Reorder the correlation matrix
cor_matrix_ordered # Return the ordered correlation matrix
cor_matrix_ordered <- cor_matrix_ordered %>% #Export  
  as.data.frame()
  
# Add row names as a new column at the beginning of the data frame
#  cor_matrix_ordered <- cbind(Variable = rownames(cor_matrix_ordered), cor_matrix_ordered)
write_xlsx(cor_matrix_ordered, "output/grand_orrmatrix.xlsx")

##### between validity (sub)scales #############################################
# calculate scale means
selected_data$MCCS <- rowMeans(selected_data[, startsWith(colnames(selected_data), "MCCS")], na.rm = TRUE)
selected_data$COPE <- rowMeans(selected_data[, startsWith(colnames(selected_data), "cope_")], na.rm = TRUE)
selected_data$RS11 <- rowMeans(selected_data[, startsWith(colnames(selected_data), "RS11_")], na.rm = TRUE)
selected_data$BRSC <- rowMeans(selected_data[, startsWith(colnames(selected_data), "BRSC")], na.rm = TRUE)
selected_data$RA <- rowMeans(selected_data[, startsWith(colnames(selected_data), "rsa_")], na.rm = TRUE)
selected_data$FFA <- rowMeans(selected_data[, startsWith(colnames(selected_data), "FFA")], na.rm = TRUE)
selected_data$SWLS <- rowMeans(selected_data[, startsWith(colnames(selected_data), "SWLS0")], na.rm = TRUE)
selected_data$GSE <- rowMeans(selected_data[, startsWith(colnames(selected_data), "GSE")], na.rm = TRUE)

 ## calculate subscale means
  rsa_subscale_patterns <- list( # RSA subscales with their corresponding startsWith pattern
    `Selbstwahrnehmung` = "rsa_1_",
    `Geplante Zukunft` = "rsa_2_",
    `Soziale Kompetenz` = "rsa_3_",
    `Struktureller Stil` = "rsa_4_",
    `Familienzusammenhalt` = "rsa_5_",
    `Soziale Ressourcen` = "rsa_6_"
  )
  cope_subscale_patterns <- list( # COPE subscales with their corresponding startsWith pattern
    `Ablenkung` = "cope_01_",
    `Verleugnung` = "cope_02_",
    `Emotionale Unterstützung` = "cope_03_",
    `Verhaltensrückzug` = "cope_04_",
    `Positive Umdeutung` = "cope_05_",
    `Humor` = "cope_06_",    
    `Aktive Bewältigung` = "cope_07_",
    `Alkohol/Drogen` = "cope_08_",
    `Instrumentelle Unterstützung` = "cope_09_",
    `Ausleben von Emotionen` = "cope_10_",
    `Planung` = "cope_11_",
    `Akzeptanz` = "cope_12_",    
    `Selbstbeschuldigung` = "cope_13_",
    `Religion` = "cope_14_"  
  )
for(subscale in names(rsa_subscale_patterns)) { # Compute the means for each RSA subscale
    items <- colnames(selected_data)[startsWith(colnames(selected_data), rsa_subscale_patterns[[subscale]])]
    selected_data[[subscale]] <- rowMeans(selected_data[, items], na.rm = TRUE)
  }
for(subscale in names(cope_subscale_patterns)) { # Compute the means for each COPE subscale
    items <- colnames(selected_data)[startsWith(colnames(selected_data), cope_subscale_patterns[[subscale]])]
    selected_data[[subscale]] <- rowMeans(selected_data[, items], na.rm = TRUE)
  }
  
all_items <- colnames(data_rcitems_2) #  corr matrix
all_scales <- c("MCCS", "COPE", names(rsa_subscale_patterns), "RS11", "BRSC", "RA", names(cope_subscale_patterns))
full_matrix <- cor(selected_data[, c(all_items, all_scales)], method = "pearson", use = "pairwise.complete.obs")
selective_matrix <- full_matrix[all_items, all_scales] #  selective corrmatrix 

#CONVERGENT  
cor_con_list <- list() # Initialize the list 'cor_con_list'
# Define thresholds
positive_threshold <- 0.5
negative_threshold <- -0.5
low_corr_range <- c(-0.1, 0.1)
# Loop through each column in the selective_matrix
  for (scale in colnames(selective_matrix)) {
    column_data <- selective_matrix[, scale]
    high_positive_items <- names(column_data[column_data > positive_threshold]) # Identify items with high positive correlations
    high_negative_items <- names(column_data[column_data < negative_threshold]) # Identify items with high negative correlations
    no_correlation_items <- names(column_data[column_data > low_corr_range[1] & column_data < low_corr_range[2]]) # Identify items with no/weak correlations
    cor_con_list[[scale]] <- list( # Store the results in the cor_con_list list
      high_positive = high_positive_items,
      high_negative = high_negative_items,
      no_correlation = no_correlation_items
    )
}
  
#DIVERGENT  
#re calculate
# calculate scale means
selected_data$BOKX <- rowMeans(selected_data[, startsWith(colnames(selected_data), "BOKX")], na.rm = TRUE)
selected_data$`GPS-K` <- rowMeans(selected_data[, startsWith(colnames(selected_data), "GPS-K")], na.rm = TRUE) 
selected_data$`P-PSE` <- rowMeans(selected_data[, startsWith(colnames(selected_data), "P-PSE")], na.rm = TRUE)
all_scales <- c("GPS-K", "BOKX" ,"P-PSE")
full_matrix <- cor(selected_data[, c(all_items, all_scales)], method = "pearson", use = "pairwise.complete.obs")
selective_matrix <- full_matrix[all_items, all_scales] #  selective corrmatrix 
  
cor_div_list <- list() # Initialize the list 'cor_div_list'
# Define thresholds
positive_threshold <- 0.5
negative_threshold <- -0.5
low_corr_range <- c(-0.1, 0.1)
# Loop through each column in the selective_matrix
for (scale in colnames(selective_matrix)) {
    column_data <- selective_matrix[, scale]
    high_positive_items <- names(column_data[column_data > positive_threshold]) # Identify items with high positive correlations
    high_negative_items <- names(column_data[column_data < negative_threshold]) # Identify items with high negative correlations
    no_correlation_items <- names(column_data[column_data > low_corr_range[1] & column_data < low_corr_range[2]]) # Identify items with no/weak correlations
    cor_div_list[[scale]] <- list( # Store the results in the cor_div_list list
      high_positive = high_positive_items,
      high_negative = high_negative_items,
      no_correlation = no_correlation_items
    )
}

'-------------------------------------------------------------------------------
                                item reduction
 ------------------------------------------------------------------------------'
### item reduction #############################################################
  '
Objectives:
  items_below_upper_quartile_1_good #these indicate good items! (frequently not loaded under .1)
  items_below_upper_quartile_1      #inverse of items_below_upper_quartile_1
  items_below_upper_quartile_5_good #these indicate good items: inverse of items_below_upper_quartile_5
  items_below_upper_quartile_5      # this list indicate items loaded above .5, but not as frequent as others ( below .75 quantile)
Objects:  
  final_item_removal_worst: This list will contain items that are common across the items_below_upper_quartile, columns_to_remove_d_low_vsubscale, and columns_to_remove_d_low_vscale lists.
  final_item_removal_best: This list will contain items that are in all_items but not present in any of the worst-performing lists.
  final_item_removal_between: This list will contain items that are in all_items but not in final_item_removal_best or final_item_removal_worst.
'
all_items #contains a list of all items
  
# Remove specified items from the initial list
reduced_items_01 <- setdiff(all_items,items_below_upper_quartile_5) #items counts (loading below .5) below .75 quantile (244 items)

# reduced_item_02 was an initial attempt of item exclusion for high missingness. but we later decided to include all items

reduced_items_03 <- setdiff(all_items,cor_con_list$MCCS$no_correlation) #
reduced_items_03 <- setdiff(reduced_items_03,cor_con_list$COPE$no_correlation) #
reduced_items_03 <- setdiff(reduced_items_03,cor_con_list$RS11$no_correlation) #
reduced_items_03 <- setdiff(reduced_items_03,cor_con_list$BRSC$no_correlation) #
reduced_items_03 <- setdiff(reduced_items_03,cor_con_list$RA$no_correlation) #
  
#reduced_items_04
#when I apply all vsubscale, no items remain (see Version 01_itemsets_2023_09_23). Therefore, I penalize it
# Initialize a vector to store items appearing in the lists
reduced_items_04_list <- c()
  
# Combine all the lists into this vector
for (list_name in names(cor_con_list)) {
    reduced_items_04_list <- c(reduced_items_04_list, cor_con_list[[list_name]]$no_correlation)
}
  
## Create a frequency table
freq_table <- table(reduced_items_04_list)
## Identify items that appear more than 5 times
items_to_remove <- names(freq_table[freq_table > 5])

# Now perform the set difference to remove these items from reduced_items_04
reduced_items_04 <- setdiff(all_items, items_to_remove)

reduced_items_05 <- setdiff(all_items,cor_div_list$`GPS-K`$high_positive) #
reduced_items_05 <- setdiff(reduced_items_05,cor_div_list$`GPS-K`$high_negative) #
reduced_items_05 <- setdiff(reduced_items_05,cor_div_list$`BOKX`$high_positive) #
reduced_items_05 <- setdiff(reduced_items_05,cor_div_list$`BOKX`$high_negative) #
reduced_items_05 <- setdiff(reduced_items_05,cor_div_list$`P-PSE`$high_positive) #
reduced_items_05 <- setdiff(reduced_items_05,cor_div_list$`P-PSE`$high_negative) #

reduced_items_06 <- setdiff(all_items,items_below_upper_quartile_1) #items counts (loading below .1) below .75 quantile (246 items)
  

# Combine all lists into a single list
#include: below low loading, nocor to vscale, nocor to vsubscale (hard criteria), cor to divscale, below high laoding
   
#This counts good items. more counts equal better items.
all_reduced_lists <- list(reduced_items_01  ,reduced_items_03, reduced_items_04, reduced_items_05, reduced_items_06)
names(all_reduced_lists) <- c("reduced_items_01", "reduced_items_03", "reduced_items_04", "reduced_items_05", "reduced_items_06")

item_list_names <- list() # Initialize an empty list to store the list names for each item
# Loop through each list and record the names of the lists containing each item
   for (list_name in names(all_reduced_lists)) {
     for (item in all_reduced_lists[[list_name]]) {
       if (is.null(item_list_names[[item]])) {
         item_list_names[[item]] <- list(list_name)
       } else {
         item_list_names[[item]] <- unique(c(item_list_names[[item]], list_name))
       }
     }
   }

# Recount the frequency of each unique item based on the lists it appears in
item_counts_corrected <- sapply(item_list_names, length)

table(df_item_counts$Frequency)


# Convert the table to a data frame for easier inspection
df_item_counts <- data.frame(Item = names(item_counts_corrected), Frequency = item_counts_corrected)
colnames(df_item_counts) <- c("Item", "Frequency")
df_item_counts$ListNames <- sapply(df_item_counts$Item, function(x) paste(item_list_names[[x]], collapse = ", ")) # Add a column for the list names
# Filter the data frame to find items that appear exactly 4 times
items_with_frequency_4 <- df_item_counts[df_item_counts$Frequency >= 4,]
print(items_with_frequency_4) # Inspect the resulting data frame
final_items_list <- as.data.frame(items_with_frequency_4) # Extract the final items list
## here itemlist without the worst items: 
finale_items_not_worst <- as.list(items_with_frequency_4$Item)

#_______________________________________________________________________________    
# we use these 83 Items as a baseline.
#_______________________________________________________________________________    
  
## here we perform forward selection: itemlist with best items are appended if not present
mycutoff=0.9
# Lists containing EFA, PCA and PCA bifactor
analysis_lists <- list(EFA = itemsets_EFA, PCA = itemsets_PCA, PCAbifactor = itemsets_PCAbifactor)

# Initialize an empty vector to store variable names
high_loading_items <- c()
# uses function "extract_high_loading_items"
  # Loop through each analysis list (EFA, PCA, and PCAbifactor)
  for (analysis in names(analysis_lists)) {
    current_list <- analysis_lists[[analysis]]
    for (df_name in names(current_list)) {
      df <- current_list[[df_name]]
      high_loading_items <- c(high_loading_items, extract_high_loading_items(df, mycutoff))
    }
  }
  for (df_name in names(itemsets_IRT)) {   # Loop through IRT itemsets
    df <- itemsets_IRT[[df_name]]
    high_loading_items <- c(high_loading_items, extract_high_loading_items(df, mycutoff))
  }
  high_loading_items_IRT_9 <- unique(high_loading_items) # Extract unique variable names
  
#Append to the items with highest loadings if not present:  
  # -> includes 3 objective selection and is appended to the .9 loading objective.
  finale_items_not_worst_01 <- union(finale_items_not_worst,high_loading_items_IRT_9)
  
  # -> add con vscale highest loadings (above.5)
  finale_items_not_worst_02 <- union(finale_items_not_worst_01,cor_con_list$MCCS$high_positive) #
  finale_items_not_worst_02 <- union(finale_items_not_worst_02,cor_con_list$MCCS$high_negative) #
  finale_items_not_worst_02 <- union(finale_items_not_worst_02,cor_con_list$COPE$high_positive) #
  finale_items_not_worst_02 <- union(finale_items_not_worst_02,cor_con_list$COPE$high_negative) #
  finale_items_not_worst_02 <- union(finale_items_not_worst_02,cor_con_list$BRSC$high_positive) #
  finale_items_not_worst_02 <- union(finale_items_not_worst_02,cor_con_list$BRSC$high_negative) #
  finale_items_not_worst_02 <- union(finale_items_not_worst_02,cor_con_list$RA$high_positive) #
  finale_items_not_worst_02 <- union(finale_items_not_worst_02,cor_con_list$RA$high_negative) #
  
#This criteria, is not easiliy adapted. as it includes to many items. Therefore, we additionally select based on 
#  counts in df_item_counts (number of hits in analysis). So we append only those items, that simultaniously performed well factor wise
# -> add con vsubscale
 ## append negative (above.5)
finale_items_not_worst_03 <- as.list(cor_con_list$Selbstwahrnehmung$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Geplante Zukunft'$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Soziale Kompetenz'$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Struktureller Stil'$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Familienzusammenhalt$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Soziale Ressourcen'$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Ablenkung$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Verleugnung$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Emotionale Unterstützung'$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Verhaltensrückzug$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Positive Umdeutung'$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Humor$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Aktive Bewältigung'$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Alkohol/Drogen'$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Instrumentelle Unterstützung'$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Ausleben von Emotionen'$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Planung$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Akzeptanz$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Selbstbeschuldigung$high_negative) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Religion$high_negative) #
  
## append positive (above.5)
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Selbstwahrnehmung$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Geplante Zukunft'$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Soziale Kompetenz'$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Struktureller Stil'$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Familienzusammenhalt$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Soziale Ressourcen'$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Ablenkung$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Verleugnung$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Emotionale Unterstützung'$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Verhaltensrückzug$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Positive Umdeutung'$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Humor$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Aktive Bewältigung'$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Alkohol/Drogen'$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Instrumentelle Unterstützung'$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$'Ausleben von Emotionen'$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Planung$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Akzeptanz$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Selbstbeschuldigung$high_positive) #
finale_items_not_worst_03 <- union(finale_items_not_worst_03,cor_con_list$Religion$high_positive) #
  
filtered03_df_item_counts <- df_item_counts %>% 
filter(Item %in% finale_items_not_worst_03)

# Filter the data frame to find items that appear exactly 4 times
filtered03_df_item_counts <- filtered03_df_item_counts[filtered03_df_item_counts$Frequency >= 4,]
finale_items_not_worst_03a <- as.list(filtered03_df_item_counts$Item) #correlation with hit in analysis
finale_items_not_worst_03b <- union(finale_items_not_worst_02,finale_items_not_worst_03a) #append 02 to new finale_items_not_worst_03a
  
# -> append highest loading for specific columns
run_one_factor_EFA <- readRDS("functions/run_one_factor_EFA.RDS") # load functions  to run one-factor EFA for subsets and return loadings
# Run one-factor EFA for each subset and store the loadings
loadings_relig <- run_one_factor_EFA(data_rcitems_3_noNA, "relig_")
loadings_relig_1 <- run_one_factor_EFA(data_rcitems_3_noNA, "relig_1")
loadings_relig_2 <- run_one_factor_EFA(data_rcitems_3_noNA, "relig_2")
loadings_subst <- run_one_factor_EFA(data_rcitems_3_noNA, "subst")
loadings_physi_1 <- run_one_factor_EFA(data_rcitems_3_noNA, "physi_1")
  
# Retrieve the highest loading items for each subset
get_highest_loading_item <- readRDS("functions/get_highest_loading_item.RDS") # load functions  to get the highest loading item for a given subset  
high_loading_relig <- get_highest_loading_item(loadings_relig)
high_loading_relig_1 <- get_highest_loading_item(loadings_relig_1)
high_loading_relig_2 <- get_highest_loading_item(loadings_relig_2)
high_loading_subst <- get_highest_loading_item(loadings_subst)
high_loading_physi_1 <- get_highest_loading_item(loadings_physi_1)
  
# Combine these into a single character vector
high_loading_items_specific <- c(high_loading_relig, high_loading_relig_1, high_loading_relig_2, high_loading_subst, high_loading_physi_1)
high_loading_items_specific #contains highest specific factor wise loading
finale_items_not_worst_04 <- union(finale_items_not_worst_03b,high_loading_items_specific)  #append to final dataset

# Append highest discriminant power for specific columns
# Function to run alpha function for subsets and return r.drop values
run_alpha_on_subset <- readRDS("functions/run_alpha_on_subset.RDS") # load functions  to run alpha function for subsets and return r.drop values
get_highest_rdrop_item <- readRDS("functions/get_highest_rdrop_item.RDS") # load functions to get the item with the highest absolute r.drop value for a given subset

# Run alpha for each subset and store the r.drop values
discriminants_relig <- run_alpha_on_subset(data_rcitems_3_noNA, "relig_")  
discriminants_relig_1 <- run_alpha_on_subset(data_rcitems_3_noNA, "relig_1")
discriminants_relig_2 <- run_alpha_on_subset(data_rcitems_3_noNA, "relig_2")
discriminants_subst <- run_alpha_on_subset(data_rcitems_3_noNA, "subst")
discriminants_physi_1 <- run_alpha_on_subset(data_rcitems_3_noNA, "physi_1")
  
# Retrieve the highest discriminant items for each subset
high_discriminant_relig <- get_highest_rdrop_item(discriminants_relig)
high_discriminant_relig_1 <- get_highest_rdrop_item(discriminants_relig_1)
high_discriminant_relig_2 <- get_highest_rdrop_item(discriminants_relig_2)
high_discriminant_subst <- get_highest_rdrop_item(discriminants_subst)
high_discriminant_physi_1 <- get_highest_rdrop_item(discriminants_physi_1)
  
# Combine these into a single character vector
high_discriminant_items_specific <- c(high_discriminant_relig, high_discriminant_relig_1, high_discriminant_relig_2, high_discriminant_subst, high_discriminant_physi_1)
#high_discriminant_items_specific # Display the list of highest discriminant items
finale_items_not_worst_05 <- union(finale_items_not_worst_04, high_discriminant_items_specific) # Append to final dataset


#--> 106 Items in total
common_itemslist <- as.list(finale_items_not_worst_05)
common_itemslist

'-------------------------------------------------------------------------------
                               qualitative adaptions
 ------------------------------------------------------------------------------'
### qualitative adaptions ######################################################
#We conducted a qualitative review of the resulting item list and matched it with the feedback received from our participants. 
#The feedback indicated that many items were duplicates, as we often had multiple variations of the same item to identify the most 
#appropriate one. Consequently, the resulting item list no longer contains such duplications, which is a positive outcome.
#Further examination of the item content confirmed our confidence in the composition of the items, and no additional adaptations 
#were deemed necessary. However, we decided to remove the item "ableh_2_077_r" ("Andere Menschen sind oft für meine Fehler 
#verantwortlich.") due to concerns about its content. The remaining 105 items were then prepared for the present study.

# Remove specific item
common_itemslist <- common_itemslist[common_itemslist != "ableh_2_077_r"]

# adapt common_itemslist
#save(common_itemslist, file = "common_itemslist.RData")
codebookV2 <- codebook %>% filter(item_name_auswertung %in% common_itemslist) #export codebook for Study2
#common_itemslist_df <- as.data.frame(common_itemslist)
#write_xlsx(codebookV2, "output/codebookV2.xlsx")


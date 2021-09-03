# This file contains helper functions for performing the ScreenNC analysis

# Display percentages with one decimal point and a % sign.
pct <- function(x) {scales::percent(x, accuracy = .01, suffix = "%")} 

# Display all dataframe elements as a percentage
pct.df <- function(x){scales::percent(x, accuracy = .1, suffix = "%")} 

# Display p-values; function written by "David_B" on 
# https://stackoverflow.com/questions/37470202/in-line-code-for-reporting-p-values-001-in-r-markdown
myround2 <- function(x, digits = 3){
  if (x < 10^-digits) return(paste('<', 10^-digits))
  paste('=', myround(x, digits))
}

# Format a raw ScreenNC file. Will be used for both snc1.raw and snc2.raw 
format_snc <- function(snc, age_grp_cuts){
  
  # only consider IgG procedures by using the IgG procedure code 
  igg.proccode <- unique(snc[grepl("IGG", snc$PROC_NAME),]$proc_code) #all proc_codes with "IGG" in the PROC_NAME
  if(length(igg.proccode)!=1) warning("WARNING: More than 1 proc_code associated with 
        IGG procedure. Investigate why this is as statistics may not be reliable.")
  
  # may need to check the format of the encounter_date column before converting it
  snc$encounter_date <- gsub(x=snc$encounter_date,pattern=" 0:00",replacement="",fixed=T)
  snc$encounter_date <- mdy(snc$encounter_date)
  
  # subset to only relevant (IgG) tests
  snc_rel <- snc[(snc$proc_code == igg.proccode) & (!is.na(snc$proc_code)),]
  
  # discretize age 
  snc_rel$age_group <- cut(snc_rel$age, breaks = age_grp_cuts, right = FALSE) ### should match age groups 
  
  # recategorize race 
  snc_rel$race_name <- as.character(snc_rel$race_name)
  snc_rel[snc_rel$race_name %in% other_races,]$race_name <- "Other" # group several races into "Other"
  if(length(snc_rel[snc_rel$race_name %in% c("Patient Refused","Unknown"),]$race_name) > 0) {
    snc_rel[snc_rel$race_name %in% c("Patient Refused","Unknown"),]$race_name <- 
      "Patient Refused or Unknown" # group patient refused and unknown together
  }
  snc_rel$race_name <- as.factor(snc_rel$race_name)
  
  # create final versions of datasets, choosing only relevant columns.
  # may need to perform more data cleaning if file format changes.
  snc.final <- snc_rel %>% 
    dplyr::select("sex_name", "race_name", 
                  "age_group",  "encounter_date", "ABNORMAL_YN")
  
  snc.final
}

# format a UNC target population final
format_unc <- function(unc){
  
  # if there is a patient with age, race, and ethnic group unknown, remove (relevant for one patient previously)
  unc <- unc[!(unc$age_group=="NULL" & unc$race_name=="Unknown" & unc$ethnic_group_name=="Unknown"),] 
  
  # clean age group data to match the categories from the sample (ScreenNC) data
  unc$age_group <- as.factor(sub("_", "-", unc$age_group))
  unc <- unc %>% dplyr::filter(!(age_group %in% c("0-9", "10-19"))) # remove lowest age buckets 
  unc$age_group <- droplevels(unc$age_group)
  unc$age_group <- factor(unc$age_group, levels = 
              c("18-19", levels(unc$age_group))) # add a level with no data, to be used later
    
  # recategorize race
  unc$race_name <- as.character(unc$race_name)
  unc[unc$race_name %in% other_races,]$race_name <- "Other"
  if(length(unc[unc$race_name %in% c("Patient Refused","Unknown"),]$race_name) > 0){
    unc[unc$race_name %in% c("Patient Refused","Unknown"),]$race_name <- "Patient Refused or Unknown"}
  unc$race_name <- as.factor(unc$race_name)
  
  # remove the start and end date fields
  unc <- unc %>% dplyr::select(-c(count_start_date,count_end_date))
  
  unc.final <- unc
  return(unc.final)
}


# format a UNC target population final -- a specific piece of code
# that will not work with different age group cuts
# assumes age group cuts are c(18, 20, 40, 60, Inf)
format_unc_specific_cuts <- function(unc){
  
  # if there is a patient with age, race, and ethnic group unknown, remove (relevant for one patient previously)
  unc <- unc[!(unc$age_group=="NULL" & unc$race_name=="Unknown" & unc$ethnic_group_name=="Unknown"),] 
  
  # clean age group data to match the categories from the sample (ScreenNC) data
  unc$age_group <- as.factor(sub("_", "-", unc$age_group))
  unc <- unc %>% dplyr::filter(!(age_group %in% c("0-9", "10-19"))) # remove lowest age buckets 
  unc$age_group_new <- NA
  
  # create some new age levels
  unc[unc$age_group %in% c("20-29", "30-39"),]$age_group_new <- "20-39"
  unc[unc$age_group %in% c("40-49", "50-59"),]$age_group_new <- "40-59"
  unc[unc$age_group %in% c("60-69", "70-79", "80-plus"),]$age_group_new <- "60-plus"
  unc <- unc %>% select(-c(age_group)) %>% 
    rename(age_group = age_group_new) %>% # rename new age column
    relocate(age_group, .after = "race_name")
  
  unc$age_group <- as.factor(unc$age_group)
  unc$age_group <- factor(unc$age_group, levels = 
                            c("18-19", levels(unc$age_group))) # add a level with no data, to be used later
  
  # recategorize race
  unc$race_name <- as.character(unc$race_name)
  unc[unc$race_name %in% other_races,]$race_name <- "Other"
  if(length(unc[unc$race_name %in% c("Patient Refused","Unknown"),]$race_name) > 0){
    unc[unc$race_name %in% c("Patient Refused","Unknown"),]$race_name <- "Patient Refused or Unknown"}
  unc$race_name <- as.factor(unc$race_name)
  
  # remove the start and end date fields
  unc <- unc %>% dplyr::select(-c(count_start_date,count_end_date))
  
  unc.final <- unc
  return(unc.final)
}

# format a census target population final -- a specific piece of code
# that will not work with different age group cuts
# assumes age group cuts are c(18, 20, 40, 60, Inf)
format_census <- function(census){
  
  # clean age group data to match the categories from the sample (ScreenNC) data
  census <- census %>% dplyr::filter(!(age_group %in% c("0-9", "10-19"))) # remove lowest age buckets 
  census$age_group_new <- NA
  
  # create some new age levels
  census[census$age_group %in% c("20-29", "30-39"),]$age_group_new <- "20-39"
  census[census$age_group %in% c("40-49", "50-59"),]$age_group_new <- "40-59"
  census[census$age_group %in% c("60-69", "70-79", "80-plus"),]$age_group_new <- "60-plus"
  census <- census %>% select(-c(age_group)) %>% 
    rename(age_group = age_group_new)  %>% # rename new age column
    relocate(age_group, .after = "race_name")
  
  census$age_group <- as.factor(census$age_group)
  census$age_group <- droplevels(census$age_group)
  # census$age_group <- factor(census$age_group, levels = 
  #                           c("18-19", levels(census$age_group))) # add a level with no data, to be used later
  # 
  # recategorize race
  census$race_name <- as.character(census$race_name)
  census[census$race_name %in% other_races,]$race_name <- "Other"
  if(length(census[census$race_name %in% c("Patient Refused","Unknown"),]$race_name) > 0){
    census[census$race_name %in% c("Patient Refused","Unknown"),]$race_name <- "Patient Refused or Unknown"}
  census$race_name <- as.factor(census$race_name)
  
  census.final <- census
  return(census.final)
}

# is there a difference between elements x and y? used in confidence_checks function.
difference <- function(x, y) {c(setdiff(x, y), setdiff(y, x))}

# ensure that ScreenNC and UNC target population datasets have expected formatting
confidence_checks <- function(snc, unc){
  
  if(length(levels(unc$age_group)) != length(levels(snc$age_group))) {
    warning("Warning: Number of age groups varies between SNC1 and Target Pop.
          Discern why this is happening.")
  } 
  
  snc1.sexes <- unique(snc$sex_name)
  unc.sexes <- unique(unc$sex_name)
  if(length(difference(snc1.sexes,unc.sexes)) != 0) warning("
        Warning: Some sex names differ between SNC1 and UNC population.
        Sex statistics may be in error.")
  
  snc1.races <- unique(snc$race_name)
  unc.races <- unique(unc$race_name)
  if(length(difference(snc1.races,unc.races)) != 0) warning("Warning:
        Some race names differ between SNC1 and UNC population.
        Check if this is expected behavior or not.")
  
  # lab.eths <- unique(snc$ethnic_group_name)
  # unc.eths <- unique(unc$ethnic_group_name)
  # if(length(difference(lab.eths,unc.eths)) != 0) warning("Warning:
  #       Some ethnic group names differ between sample and target population")
  # invisible()
}

# format a dataframe for display in Table 1
format_table1 <- function(df, cohortName, vars_table1){
  
  ### when relevant, go from long to wide format 
  if("pat_count" %in% colnames(df)){
    df <- tidyr::uncount(df, weights = pat_count)
  }
  df <- df %>% select(any_of(vars_table1))
  df$cohort <- cohortName
  return(df)
}

# label Table 1
label_table1 <- function(df){
  table1::label(df$sex_name) <- "Sex"
  table1::label(df$race_name) <- "Race"
  #table1::label(df$ethnic_group_name) <- "Ethnic Group"
  if("age_group" %in% colnames(df)) {table1::label(df$age_group) <- "Age Group"}
  table1::label(df$cohort) <- "Cohort"
  return(df)
}

### function for age statistics (median, etc)
make_age_table <- function(snc, popname){
  
  ### get median, range, and iqr
  labs_age_med <- median(snc$age)
  labs_age_min <- min(snc$age)
  labs_age_max <- max(snc$age)
  labs_age_iqr_lower <- quantile(snc$age, .25, na.rm = T)
  labs_age_iqr_upper <- quantile(snc$age, .75, na.rm = T)
  labs_median_tab <- data.frame(labs_age_min, labs_age_iqr_lower, labs_age_med, 
    labs_age_iqr_upper, labs_age_max)
  
  return(kable(labs_median_tab, format = kable_format, 
               col.names = c("Minimum Age", "1st Quartile", "Median Age", 
                             "3rd Quartile", "Maximum Age"),
    caption = paste("Age Statistics for ", popname, " Sample"), row.names = FALSE))
}

##### make side by side barplot
make_demo_barplot <- function(var, df1, df2, df3){
  vars_grouping <- c(var, "cohort")
  
  sm.df1 <- df1 %>% dplyr::select(all_of(var)) %>% 
    dplyr::mutate(cohort = "SNC1") %>%  # ideally shouldn't hard-code cohort name like this 
    dplyr::group_by_at(vars_grouping) %>% 
    dplyr::summarise(n = n(), .groups = 'drop') %>% 
    dplyr::mutate(prop = n/sum(n))
    
  sm.df2 <- df2 %>% dplyr::select(all_of(var), "pat_count") %>% 
    dplyr::mutate(cohort = "UNC20") %>% #ideally shouldn't have cohort name like this 
    dplyr::group_by_at(vars_grouping) %>% 
    dplyr::summarise(n = sum(pat_count), .groups = 'drop') %>% 
    dplyr::mutate(prop = n/sum(n))
  
  sm.df3 <- df3 %>% dplyr::select(all_of(var), "pat_count") %>% 
    dplyr::mutate(cohort = "UNC19") %>% #ideally shouldn't have cohort name like this 
    dplyr::group_by_at(vars_grouping) %>% 
    dplyr::summarise(n = sum(pat_count), .groups = 'drop') %>% 
    dplyr::mutate(prop = n/sum(n))
  
  comb.df <- dplyr::bind_rows(sm.df1, sm.df2, sm.df3)
  
  #cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  demo.p <- ggplot(comb.df, aes_string(x="cohort", y = "prop", fill=var)) + 
    geom_bar(position="dodge",stat="identity") + 
    xlab("Cohort") + ylab("Proportion") + 
    scale_y_continuous(labels = pct) + 
    coord_flip() + 
    ggtitle(paste("Proportions of", var, "by Cohort")) + 
    scale_fill_jama() + 
    theme_bw()
  
  return(demo.p)
}

# recomputes the pat_count for the chosen standardization variables
make_tgt_pop <- function(dat, vars_std){
  
  dat_tgt <- dat %>% 
    dplyr::select(all_of(vars_std), "pat_count") %>% # select standardization vars
    dplyr::group_by_at(vars_std) %>% 
    dplyr::summarise(pat_count = sum(pat_count), .groups = 'drop') 
  
  n_tgt <- sum(dat_tgt$pat_count)
  dat_tgt <- dat_tgt %>% dplyr::mutate(stratum_prop = pat_count/n_tgt)
  return(dat_tgt)
}


library(readr)


demographics_baseline <- read_csv("outputs/demographics_baseline.csv")
family <- read_csv("outputs/family.csv")
genetic <- read_csv("outputs/genetic.csv")

demographics_long <- read_csv("outputs/demographics_long.csv")
BMI <- read_csv("outputs/ABCD_BMI.csv")
exposome_set <- read.csv("outputs/exposome_set.csv")
exposome_sum_set <- read_csv("outputs/exposome_sum_set.csv")
externalize_dataset <- read_csv("outputs/externalize_dataset.csv")
geo_data <- read_csv("outputs/geo_data.csv")
hormone_saliva <- read_csv("outputs/hormone_saliva.csv")
ksad_y_diagnosis <- read_csv("outputs/ksad_y_diagnosis.csv")
physicalhealth_sum <- read_csv("outputs/physicalhealth_sum.csv")
physicalhealth <- read_csv("outputs/physicalhealth.csv")
psychopathology <- read_csv("outputs/psychopathology.csv")
psychopathology_sum <- read_csv("outputs/psychopathology_sum_scores.csv")
suicide_set <- read_csv("outputs/suicide_set.csv")
site <-read_csv("outputs/site.csv")


dataset = merge(demographics_long, BMI, all = T)
dataset = merge(dataset, exposome_set, all = T)
dataset = merge(dataset, exposome_sum_set, all = T)
dataset = merge(dataset, externalize_dataset, all = T)
dataset = merge(dataset, geo_data, all = T)
dataset = merge(dataset, hormone_saliva, all = T)
dataset = merge(dataset, ksad_y_diagnosis, all = T)
dataset = merge(dataset, physicalhealth_sum, all = T)
dataset = merge(dataset, physicalhealth, all = T)
dataset = merge(dataset, psychopathology, all = T)
dataset = merge(dataset, psychopathology_sum, all = T)
dataset = merge(dataset, suicide_set, all = T)
dataset = merge(dataset, site, all = T)


# keep only relevant timepoints
dataset = dataset[dataset$eventname %in% c("1_year_follow_up_y_arm_1","2_year_follow_up_y_arm_1"),]


# new variable to use in reshape from long to wide format
dataset$timepoint = regmatches(dataset$eventname, regexpr(".*_year", dataset$eventname))
dataset$eventname = NULL
dataset$demo_prim_l = NULL
dataset_wide = reshape(dataset, direction = "wide", idvar = c("src_subject_id", "sex", "sex_br"), timevar = "timepoint", sep = "_")

dataset_wide = dataset_wide[,colSums(is.na(dataset_wide)) != nrow(dataset_wide)]

dataset_wide = merge(demographics_baseline, dataset_wide, all.y = T)
dataset_wide = merge(dataset_wide, family[,c("src_subject_id", "sex", "rel_family_id")] , all.x= T)
dataset_wide = merge(dataset_wide, genetic , all.x= T)



set.seed(131)
library(data.table)
one_family_member = setDT(dataset_wide)[, .SD[sample(x = .N, size = 1)] ,by = rel_family_id]

write.csv(file = "outputs/dataset_bully.csv",x = dataset_wide, row.names = F, na = "")
write.csv(file = "outputs/one_family_member.csv",x = one_family_member, row.names = F, na = "")









source("config.R")
source("utility_fun.R")

########### Sum Scores Culture & Environment Youth ########### 
sscey01 = load_instrument("abcd_sscey01",abcd_files_path)

sscey01 = sscey01[, grepl("^(src|interview|event|sex|pmq|fes|crpbi|srpf|dim)", colnames(sscey01))]

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscey01 = sscey01[,!grepl("_(nm|nt|na|answered)$",colnames(sscey01))] #pr

sscey01$school_protective_factors = sscey01$srpf_y_ss_ses + sscey01$srpf_y_ss_iiss

colnames(sscey01)[colnames(sscey01) == "pmq_y_ss_mean"] = "parent_monitor_mean"
colnames(sscey01)[colnames(sscey01) == "srpf_y_ss_ses"] = "school_environment_sum"
colnames(sscey01)[colnames(sscey01) == "srpf_y_ss_iiss"] = "positive_school_involvement_sum"

summary(droplevels(sscey01))


########### Summary Scores Brief Problem Monitor-Teacher Form for Ages 6-18 ########### 
ssbpmtf = load_instrument("abcd_ssbpmtf01",abcd_files_path)
ssbpmtf = ssbpmtf[, !grepl("_(nm|nt)$", colnames(ssbpmtf))]






########### merge all tables
exposome_set = merge(ssbpmtf,sscey01)

write.csv(file = "outputs/exposome_sum_set.csv",x = exposome_set, row.names = F, na = "")





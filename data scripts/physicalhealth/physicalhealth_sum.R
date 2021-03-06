
source("config.R")
source("utility_fun.R")

########## ABCD Sum Scores Physical Health Parent ###########

ssphp01 = load_instrument("abcd_ssphp01", abcd_files_path)

#select variables 
ssphp01 = ssphp01[,!grepl("_(nm|nt)$",colnames(ssphp01))]

ssphp01$male_p_late_or_post_puberty = ifelse( ssphp01$pds_p_ss_male_category_2 >= 4 ,1,0)
ssphp01$female_p_late_or_post_puberty = ifelse( ssphp01$pds_p_ss_female_category_2 >= 4 ,1, 0)

########## ABCD Sum Scores Physical Health Youth ###########

ssphy01 = load_instrument("abcd_ssphy01", abcd_files_path)

#select variables 
ssphy01 = ssphy01[,!grepl("_(nm|nt)$",colnames(ssphy01))]

ssphy01$male_y_late_or_post_puberty = ifelse(ssphy01$pds_y_ss_male_cat_2 >= 4, 1,0)
ssphy01$female_y_late_or_post_puberty = ifelse(ssphy01$pds_y_ss_female_category_2 >=4, 1,0)
ssphy01$late_or_post_puberty_both_sexes = ifelse(is.na(ssphy01$male_y_late_or_post_puberty), ssphy01$female_y_late_or_post_puberty, ssphy01$male_y_late_or_post_puberty )


ssphy01$puberty_both_sexes = ifelse(is.na(ssphy01$pds_y_ss_male_cat_2), ssphy01$pds_y_ss_female_category_2, ssphy01$pds_y_ss_male_cat_2)



physicalhealth_sum = merge(ssphp01,ssphy01 )

write.csv(file = "outputs/physicalhealth_sum.csv",x = physicalhealth_sum, row.names=F, na = "")





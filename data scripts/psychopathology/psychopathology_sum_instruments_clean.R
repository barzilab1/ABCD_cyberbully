
source("config.R")
source("utility_fun.R")

################### cbcls ################### 
cbcls01 = load_instrument("abcd_cbcls01",abcd_files_path)

#get the t scores
cbcls_t_score = cbcls01[, grepl("^(src|interview|event|sex)|nal_t$", colnames(cbcls01))]

summary(cbcls_t_score[cbcls_t_score$eventname == "2_year_follow_up_y_arm_1",]) 


################### Sum Scores Mental Health Youth ################### 
mhy = load_instrument("abcd_mhy02", abcd_files_path)

# mhy = mhy[,grepl("^(src|interview|event|sex|ple_y|pps_)",colnames(mhy))]

#remove nt (Number Total Questions) and nm (Number Missing Answers)
mhy = mhy[,!grepl("^(sup_|gish_)|_(nm|nt)$",colnames(mhy))]

mhy$pstr_ss_pr = NULL
summary(mhy[mhy$eventname == "2_year_follow_up_y_arm_1" ,])


################### Youth Summary Scores BPM and POA ################### 
yssbpm01 = load_instrument("abcd_yssbpm01", abcd_files_path)
yssbpm01 = yssbpm01[,grepl("^(src|interv|event|sex)|_(r|t|mean|sum)$", colnames(yssbpm01))]
yssbpm01 = yssbpm01[yssbpm01$eventname %in% c("2_year_follow_up_y_arm_1", "1_year_follow_up_y_arm_1"), ]



psychopathology_sum_scores = merge(cbcls_t_score, mhy)
psychopathology_sum_scores = merge(psychopathology_sum_scores, yssbpm01, all = T)


write.csv(file = "outputs/psychopathology_sum_scores.csv",x = psychopathology_sum_scores, row.names = F, na = "")


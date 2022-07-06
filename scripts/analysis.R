library(data.table)
library(tableone)
library(psych)

########################################
# helper functions
########################################
print_results <- function(model, name , DV = "suicidality") {
  print(summary(model)$call$formula)
  
  df = data.frame(
  # Estimate    = round(       summary(model)$coef[ , "Estimate"] , digits = 3),
  OR  = round(exp(   summary(model)$coef[ , "Estimate"]), digits = 1),
  CI_2_5      = round(exp(confint.default(model)[ ,   1     ]  ), digits = 1),
  CI_97_5     = round(exp(confint.default(model)[ ,   2     ]  ), digits = 1),
  P      = round(       summary(model)$coef[ , "Pr(>|z|)"] , digits = 3) 
  )
  
  dv_name = paste0("DV = ", DV)
  
  df = setDT(df, keep.rownames = dv_name)
  # combine the CI to one colunm 
  df[,"95%CI":= paste0(CI_2_5, "-",CI_97_5) ]
  # change p value to <.001
  df[, P:= as.character(P)]
  df[ as.numeric(P) < .001  , P := "<.001"]
  #remove Intercept
  df = df[-1]
 
  
  df = df[,.SD ,.SDcols = c(dv_name,"OR", "95%CI","P" )]
  
  write.table(df, file = paste0("results/", name, ".csv"), sep = ",",quote = FALSE, row.names = F)
  
  df
  
}


########################################
# organize the dataset
########################################

bully_data = read.csv("outputs/dataset_bully.csv")
setDT(bully_data)

# create cyber bully variables
bully_data[,cyberbully_category:= {
  fcase(
    cybb_phenx_harm_2_year == 1 & cybb_phenx_harm2_2_year == 0, 1,
    cybb_phenx_harm_2_year == 0 & cybb_phenx_harm2_2_year == 1, 2,
    cybb_phenx_harm_2_year == 1 & cybb_phenx_harm2_2_year == 1, 3,
    cybb_phenx_harm_2_year == 0 & cybb_phenx_harm2_2_year == 0, 4,
    default = NA)
}]

bully_data[,describe(.SD), .SDcols =c("cybb_phenx_harm_2_year", "cybb_phenx_harm2_2_year", "cyberbully_category", "cybb_phenx_harm_often_2_year", "cybb_phenx_harm2_often_2_year")]



########################################
# table 1
########################################
bully_data[, age_year :=  age_2_year/12]

catVars <- c("sex", "race_white", "race_black", "ethnicity_hisp", "suicidality_y_2_year", "SI_y_2_year", "SA_y_2_year", "nssi_y_2_year")
myVars <- c("age_year", "parents_avg_edu_1_year", catVars )


table1 = CreateTableOne(data = na.omit(bully_data, cols = "age_2_year"), vars = myVars,  factorVars = catVars, strata = "cyberbully_category", addOverall = T)
table1 <- print(table1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)


df = setDT(as.data.frame(table1), keep.rownames = "feature")
df[,test:= NULL]

cols_to_update = colnames(df)[-1]
df[grepl("(%)", feature), (cols_to_update) := lapply(.SD, function(x){sub(")","%)",x)}), .SDcols = cols_to_update]

colnames(df)[3] = "Cyberbully victim only (n=857)"
colnames(df)[4] = "Cyberbully perpetrator only (n=30)"
colnames(df)[5] = "Cyberbully victim and perpetrator (n=66)"
colnames(df)[6] = "No Cyberbullying (n=9 393)"
write.csv(df, file = "results/table1.csv")



table2 = CreateTableOne(data = na.omit(bully_data, cols = "age_2_year"), vars = c("cybb_phenx_harm_2_year", "cybb_phenx_harm2_2_year", "cyberbully_category",myVars),  factorVars = c("cybb_phenx_harm_2_year", "cybb_phenx_harm2_2_year", "cyberbully_category",catVars), strata = "sex")
table2 #<- print(table2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)

table2a = CreateTableOne(data = na.omit(bully_data, cols = "age_2_year"), vars = c("cybb_phenx_harm_2_year", "cybb_phenx_harm2_2_year", "cyberbully_category",myVars),  factorVars = c("cybb_phenx_harm_2_year", "cybb_phenx_harm2_2_year", "cyberbully_category",catVars), strata = "race_black")
table2a #<- print(table2a, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)

table3 = CreateTableOne(data = na.omit(bully_data[cybb_phenx_harm_2_year == 1,], cols = "age_2_year"), vars = myVars,  factorVars = catVars)
table3 #<- print(table3, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)


table4 = CreateTableOne(data = na.omit(bully_data[cybb_phenx_harm2_2_year == 1,], cols = "age_2_year"), vars = c("cybb_phenx_harm_2_year",myVars),  factorVars = c("cybb_phenx_harm_2_year",catVars))
table4 #<- print(table4, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)


data = na.omit(bully_data, cols = "age_2_year")

chisq.test(data$cybb_phenx_harm_2_year, data$sex, correct = F)
chisq.test(data$cybb_phenx_harm_2_year, data$race_white, correct = F)
chisq.test(data$cybb_phenx_harm_2_year, data$race_black, correct = F)
chisq.test(data$cybb_phenx_harm_2_year, data$ethnicity_hisp, correct = F)

chisq.test(data$cybb_phenx_harm2_2_year, data$sex, correct = F)
chisq.test(data$cybb_phenx_harm2_2_year, data$race_white, correct = F)
chisq.test(data$cybb_phenx_harm2_2_year, data$race_black, correct = F)
chisq.test(data$cybb_phenx_harm2_2_year, data$ethnicity_hisp, correct = F)


########################################
# Table 2- Cyberbullying and suicidality
########################################
# common covariates across all models 
covars1 = c( "age_2_year" , "sex_br" ,"race_black","race_white" , "ethnicity_hisp" ,"parents_avg_edu_1_year" )
covars2 = c( covars1, "ple_y_ss_total_bad_2_year" , "parent_monitor_mean_2_year" , "school_protective_factors_2_year","fes_y_ss_fc_2_year", "dim_y_ss_mean_2_year")
covars3 = c( covars2, "bpm_y_scr_external_t_2_year","bpm_y_scr_internal_t_2_year","cbcl_scr_syn_internal_t_2_year", "cbcl_scr_syn_external_t_2_year" )


mod1 = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars1, collapse=" + "))), 
           data = bully_data, family = "binomial")

print_results(mod1, "mod1")


mod2 = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars2, collapse=" + "))),
           data = bully_data, family = "binomial")

print_results(mod2, "mod2")
  

mod3 = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars3, collapse=" + "))),
           data = bully_data, family = "binomial")

print_results(mod3, "mod3")



##################################################
# Table 3- regular bullying and suicidality 
##################################################

# create "regular" bully variables
bully_data[,bully_vic:= peq_ss_relational_victim_2_year+peq_ss_reputation_victim_2_year+peq_ss_overt_victim_2_year]
bully_data[,bully_aggs:= peq_ss_relational_aggs_2_year+peq_ss_reputation_aggs_2_year+peq_ss_overt_aggression_2_year]

bully_data[,bully_vic_90_q:= {
  prs_90_q = quantile(bully_vic, prob = seq(0, 1, length = 11), na.rm = T)["90%"]
  fcase(
    bully_vic > prs_90_q, 1,
    bully_vic <= prs_90_q, 0,
    default = NA)
}]

bully_data[,bully_aggs_90_q:= {
  prs_90_q = quantile(bully_aggs, prob = seq(0, 1, length = 11), na.rm = T)["90%"]
  fcase(
    bully_aggs > prs_90_q, 1,
    bully_aggs <= prs_90_q, 0,
    default = NA)
}]


bully_data[,summary(.SD), .SDcols =c("bully_vic", "bully_aggs", "bully_vic_90_q", "bully_aggs_90_q")]



table3_1 = glm(as.formula(paste0("suicidality_y_2_year ~ bully_vic_90_q +bully_aggs_90_q+", paste(covars1, collapse=" + "))), 
           data = bully_data, family = "binomial")

print_results(table3_1, "table3_1")


table3_2 = glm(as.formula(paste0("suicidality_y_2_year ~ bully_vic_90_q + bully_aggs_90_q+", paste(covars2, collapse=" + "))), 
            data = bully_data, family = "binomial")

print_results(table3_2, "table3_2")


table3_3 = glm(as.formula(paste0("suicidality_y_2_year ~ bully_vic_90_q + bully_aggs_90_q+", paste(covars3, collapse=" + "))), 
               data = bully_data, family = "binomial")

print_results(table3_3,"table3_3")




##################################################
# Table 4- regular bullying corelations
##################################################

table4_1 = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + bully_vic_90_q + bully_aggs_90_q+", paste(covars1, collapse=" + "))), 
               data = bully_data, family = "binomial")

print_results(table4_1,"table4_1")


table4_2 = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + bully_vic_90_q + bully_aggs_90_q+", paste(covars2, collapse=" + "))), 
               data = bully_data, family = "binomial")

print_results(table4_2,"table4_2")


table4_3 = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + bully_vic_90_q + bully_aggs_90_q+", paste(covars3, collapse=" + "))), 
               data = bully_data, family = "binomial")

print_results(table4_3,"table4_3")




##################################################
# Supplemental eTable3 - 
# Overlap between cyber-victimization and offline 
# peer victimization and aggression experiences.
##################################################
bully_data[cybb_phenx_harm_2_year == 1, table(bully_vic_90_q)]
bully_data[cybb_phenx_harm_2_year == 1, table(bully_aggs_90_q)]



######################################################
# Supplemental eTable4 - 
# Association of cyber-victimization/cyber-bullying 
# with indicators of power imbalance with suicidality.
######################################################

bully_data[, Cyber_victim_power_imbalance := {
  fcase(
    cybb_phenx_power_2_year == 1, 1,
    cybb_phenx_power_2_year == 0 | cybb_phenx_harm_2_year == 0, 0,
    default = NA)
}]


bully_data[, Cyber_bully_power_imbalance := {
  fcase(
    cybb_phenx_power2_2_year == 1, 1,
    cybb_phenx_power2_2_year == 0 | cybb_phenx_harm2_2_year == 0, 0,
    default = NA)
}]


eTable4_1 = glm(as.formula(paste0("suicidality_y_2_year ~ Cyber_victim_power_imbalance + Cyber_bully_power_imbalance +", paste(covars1, collapse=" + "))), 
           data = bully_data, family = "binomial")

print_results(eTable4_1, "eTable4_1")


eTable4_2 = glm(as.formula(paste0("suicidality_y_2_year ~ Cyber_victim_power_imbalance + Cyber_bully_power_imbalance +", paste(covars2, collapse=" + "))), 
                data = bully_data, family = "binomial")

print_results(eTable4_2, "eTable4_2")


eTable4_3 = glm(as.formula(paste0("suicidality_y_2_year ~ Cyber_victim_power_imbalance + Cyber_bully_power_imbalance +", paste(covars3, collapse=" + "))), 
                data = bully_data, family = "binomial")

print_results(eTable4_3, "eTable4_3")



######################################################
# Supplemental eTable5 - 
# Association of frequency of cyber-victimization and
# cyber-bullying with suicidality.
######################################################
bully_data[,View(.SD), .SDcols= c("cybb_phenx_harm_2_year", "cybb_phenx_harm_often_2_year")]
bully_data[, Cyber_victimization_past_year_frequency := cybb_phenx_harm_often_2_year]
bully_data[cybb_phenx_harm_2_year == 0, Cyber_victimization_past_year_frequency := 0]
bully_data[,describe(.SD), .SDcols= c("cybb_phenx_harm_often_2_year", "Cyber_victimization_past_year_frequency", "cybb_phenx_harm_2_year")]

bully_data[, Cyber_bullying_past_year_frequency := cybb_phenx_harm2_often_2_year]
bully_data[cybb_phenx_harm2_2_year == 0, Cyber_bullying_past_year_frequency := 0]
bully_data[,table(.SD), .SDcols= c("cybb_phenx_harm2_often_2_year", "Cyber_bullying_past_year_frequency", "cybb_phenx_harm2_2_year")]

bully_data[,table(.SD), .SDcols= c( "Cyber_bullying_past_year_frequency")]
bully_data[,table(.SD), .SDcols= c( "Cyber_victimization_past_year_frequency")]


eTable5_1 = glm(as.formula(paste0("suicidality_y_2_year ~ Cyber_victimization_past_year_frequency + Cyber_bullying_past_year_frequency +", paste(covars1, collapse=" + "))), 
                data = bully_data, family = "binomial")

print_results(eTable5_1, "eTable5_1")


eTable5_2 = glm(as.formula(paste0("suicidality_y_2_year ~ Cyber_victimization_past_year_frequency + Cyber_bullying_past_year_frequency +", paste(covars2, collapse=" + "))), 
                data = bully_data, family = "binomial")

print_results(eTable5_2, "eTable5_2")


eTable5_3 = glm(as.formula(paste0("suicidality_y_2_year ~ Cyber_victimization_past_year_frequency + Cyber_bullying_past_year_frequency +", paste(covars3, collapse=" + "))), 
                data = bully_data, family = "binomial")

print_results(eTable5_3, "eTable5_3")



######################################################
# Supplemental eTable6 - 
# accounting for whether data was collected before or 
# after the declaration of COVID-19 as a pandemic. (march 11)
######################################################
bully_data[,interview_date_2_year:= as.Date(interview_date_2_year, format = "%m/%d/%Y")]
bully_data[, pandemic := {
  fcase(
    interview_date_2_year >= "2020-03-11", 1,
    interview_date_2_year < "2020-03-11", 0,
    default = NA)
}]
bully_data[,View(.SD), .SDcols= c("interview_date_2_year", "pandemic")]
bully_data[,table(.SD), .SDcols= c("pandemic")]



eTable6_1_bp = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars1, collapse=" + "))), 
                data = bully_data[pandemic == 0,], family = "binomial")

print_results(eTable6_1_bp, "eTable6_1_before_p")


eTable6_2_bp = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars2, collapse=" + "))), 
                data = bully_data[pandemic == 0,], family = "binomial")

print_results(eTable6_2_bp, "eTable6_2_before_p")


eTable6_3_bp = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars3, collapse=" + "))), 
                data = bully_data[pandemic == 0,], family = "binomial")

print_results(eTable6_3_bp, "eTable6_3_before_p")



eTable6_1_ap = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars1, collapse=" + "))), 
                   data = bully_data[pandemic == 1,], family = "binomial")

print_results(eTable6_1_ap, "eTable6_1_after_p")


eTable6_2_ap = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars2, collapse=" + "))), 
                   data = bully_data[pandemic == 1,], family = "binomial")

print_results(eTable6_2_ap, "eTable6_2_after_p")


eTable6_3_ap = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars3, collapse=" + "))), 
                   data = bully_data[pandemic == 1,], family = "binomial")

print_results(eTable6_3_ap, "eTable6_3_after_p")


######################################################
# Supplemental eTable7 - 
# Association of cyber-victimization/cyber-bullying with 
# suicidal ideation, suicide attempt and non-suicidal-self 
# injurious behavior.
######################################################


eTable7_1_SI = glm(as.formula(paste0(" SI_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars1, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable7_1_SI, "eTable7_1_SI", "SI")


eTable7_2_SI = glm(as.formula(paste0("SI_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars2, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable7_2_SI, "eTable7_2_SI","SI")


eTable7_3_SI = glm(as.formula(paste0("SI_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars3, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable7_3_SI, "eTable7_3_SI","SI")



eTable7_1_SA = glm(as.formula(paste0(" SA_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars1, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable7_1_SA, "eTable7_1_SA", "SA")


eTable7_2_SA = glm(as.formula(paste0("SA_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars2, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable7_2_SA, "eTable7_2_SA","SA")


eTable7_3_SA = glm(as.formula(paste0("SA_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars3, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable7_3_SA, "eTable7_3_SA","SA")



eTable7_1_NSSI = glm(as.formula(paste0(" nssi_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars1, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable7_1_NSSI, "eTable7_1_NSSI", "NSSI")


eTable7_2_NSSI = glm(as.formula(paste0("nssi_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars2, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable7_2_NSSI, "eTable7_2_NSSI","NSSI")


eTable7_3_NSSI = glm(as.formula(paste0("nssi_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars3, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable7_3_NSSI, "eTable7_3_NSSI","NSSI")



######################################################
# Supplemental eTable8 - 
# Association of cyber-victimization/cyber-bullying with 
# suicidal ideation, suicide attempt and non-suicidal-self 
# injurious behavior, controlling for offline experiences
######################################################

eTable8_1_SI = glm(as.formula(paste0(" SI_y_2_year ~ cybb_phenx_harm_2_year +  bully_vic_90_q + bully_aggs_90_q +", paste(covars1, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable8_1_SI, "eTable8_1_SI", "SI")


eTable8_2_SI = glm(as.formula(paste0("SI_y_2_year ~ cybb_phenx_harm_2_year +  bully_vic_90_q + bully_aggs_90_q +", paste(covars2, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable8_2_SI, "eTable8_2_SI","SI")


eTable8_3_SI = glm(as.formula(paste0("SI_y_2_year ~ cybb_phenx_harm_2_year +  bully_vic_90_q + bully_aggs_90_q +", paste(covars3, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable8_3_SI, "eTable8_3_SI","SI")



eTable8_1_SA = glm(as.formula(paste0(" SA_y_2_year ~ cybb_phenx_harm_2_year + bully_vic_90_q + bully_aggs_90_q +", paste(covars1, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable8_1_SA, "eTable8_1_SA", "SA")


eTable8_2_SA = glm(as.formula(paste0("SA_y_2_year ~ cybb_phenx_harm_2_year + bully_vic_90_q + bully_aggs_90_q +", paste(covars2, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable8_2_SA, "eTable8_2_SA","SA")


eTable8_3_SA = glm(as.formula(paste0("SA_y_2_year ~ cybb_phenx_harm_2_year + bully_vic_90_q + bully_aggs_90_q +", paste(covars3, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable8_3_SA, "eTable8_3_SA","SA")



eTable8_1_NSSI = glm(as.formula(paste0(" nssi_y_2_year ~ cybb_phenx_harm_2_year + bully_vic_90_q + bully_aggs_90_q +", paste(covars1, collapse=" + "))), 
                     data = bully_data, family = "binomial")

print_results(eTable8_1_NSSI, "eTable8_1_NSSI", "NSSI")


eTable8_2_NSSI = glm(as.formula(paste0("nssi_y_2_year ~ cybb_phenx_harm_2_year + bully_vic_90_q + bully_aggs_90_q +", paste(covars2, collapse=" + "))), 
                     data = bully_data, family = "binomial")

print_results(eTable8_2_NSSI, "eTable8_2_NSSI","NSSI")


eTable8_3_NSSI = glm(as.formula(paste0("nssi_y_2_year ~ cybb_phenx_harm_2_year + bully_vic_90_q + bully_aggs_90_q +", paste(covars3, collapse=" + "))), 
                     data = bully_data, family = "binomial")

print_results(eTable8_3_NSSI, "eTable8_3_NSSI","NSSI")


######################################################
# Supplemental eTable9 - 
# Association of different types of offline peer 
# victimization and aggression experiences and suicidality.
######################################################

eTable9_1_overt = glm(as.formula(paste0(" suicidality_y_2_year ~ peq_ss_overt_victim_2_year + peq_ss_overt_aggression_2_year+", paste(covars1, collapse=" + "))), 
                   data = bully_data, family = "binomial")

print_results(eTable9_1_overt, "eTable9_1_overt")

eTable9_2_overt = glm(as.formula(paste0(" suicidality_y_2_year ~ peq_ss_overt_victim_2_year + peq_ss_overt_aggression_2_year+", paste(covars2, collapse=" + "))), 
                      data = bully_data, family = "binomial")

print_results(eTable9_2_overt, "eTable9_2_overt")

eTable9_3_overt = glm(as.formula(paste0(" suicidality_y_2_year ~ peq_ss_overt_victim_2_year + peq_ss_overt_aggression_2_year+", paste(covars3, collapse=" + "))), 
                      data = bully_data, family = "binomial")

print_results(eTable9_3_overt, "eTable9_3_overt")



eTable9_1_reputation = glm(as.formula(paste0(" suicidality_y_2_year ~ peq_ss_reputation_victim_2_year + peq_ss_reputation_aggs_2_year+", paste(covars1, collapse=" + "))), 
                      data = bully_data, family = "binomial")

print_results(eTable9_1_reputation, "eTable9_1_reputation")

eTable9_2_reputation = glm(as.formula(paste0(" suicidality_y_2_year ~ peq_ss_reputation_victim_2_year + peq_ss_reputation_aggs_2_year+", paste(covars2, collapse=" + "))), 
                      data = bully_data, family = "binomial")

print_results(eTable9_2_reputation, "eTable9_2_reputation")

eTable9_3_reputation = glm(as.formula(paste0(" suicidality_y_2_year ~ peq_ss_reputation_victim_2_year + peq_ss_reputation_aggs_2_year+", paste(covars3, collapse=" + "))), 
                      data = bully_data, family = "binomial")

print_results(eTable9_3_reputation, "eTable9_3_reputation")



eTable9_1_relational = glm(as.formula(paste0(" suicidality_y_2_year ~ peq_ss_relational_victim_2_year + peq_ss_relational_aggs_2_year+", paste(covars1, collapse=" + "))), 
                           data = bully_data, family = "binomial")

print_results(eTable9_1_relational, "eTable9_1_relational")

eTable9_2_relational = glm(as.formula(paste0(" suicidality_y_2_year ~ peq_ss_relational_victim_2_year + peq_ss_relational_aggs_2_year+", paste(covars2, collapse=" + "))), 
                           data = bully_data, family = "binomial")

print_results(eTable9_2_relational, "eTable9_2_relational")

eTable9_3_relational = glm(as.formula(paste0(" suicidality_y_2_year ~ peq_ss_relational_victim_2_year + peq_ss_relational_aggs_2_year+", paste(covars3, collapse=" + "))), 
                           data = bully_data, family = "binomial")

print_results(eTable9_3_relational, "eTable9_3_relational")




########################################
# eTable10- Cyberbullying and suicidality
# one family member
########################################
library(lme4)
library(lmerTest)
library(MuMIn)

one_family_member = read_csv("outputs/one_family_member.csv")

eTable10_1 = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars1, collapse=" + "))), 
                   data = one_family_member, family = "binomial")

print_results(eTable10_1, "eTable10_1")


eTable10_2 = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars2, collapse=" + "))),
           data = one_family_member, family = "binomial")

print_results(eTable10_2, "eTable10_2")


eTable10_3 = glm(as.formula(paste0("suicidality_y_2_year ~ cybb_phenx_harm_2_year + cybb_phenx_harm2_2_year +", paste(covars3, collapse=" + "))),
           data = one_family_member, family = "binomial")

print_results(eTable10_3, "eTable10_3")




source("config.R")
source("utility_fun.R")


########### ABCD Parent Pubertal Development Scale and Menstrual Cycle Survey History (PDMS) ########### 

ppdms = load_instrument("abcd_ppdms01",abcd_files_path)

#"Don't know" will be treated as NA
ppdms[ppdms == "999"] = NA

#remove empty col
ppdms = ppdms[, !colSums(is.na(ppdms)) == nrow(ppdms)]
ppdms$pds_select_language___1 = NULL

#fix scale 
ppdms$pds_f5b_p = ppdms$pds_f5b_p - 1
ppdms$pds_f5b_p[ppdms$pds_f5b_p == 3] = 1

ppdms$pds_f6_p[ppdms$pds_f6_p >= 99] = NA
ppdms$menstrualcycle2_p[ppdms$menstrualcycle2_p >= 400] = NA
ppdms$menstrualcycle2_p[ppdms$menstrualcycle2_p <= 3] = NA

describe(ppdms)


########### ABCD Youth Pubertal Development Scale and Menstrual Cycle Survey History (PDMS) ########### 

ypdms = load_instrument("abcd_ypdms01",abcd_files_path)

#"Don't know" and "Decline to answer" will be treated as NA
ypdms[ypdms == "777" | ypdms == "999"] = NA
ypdms$pds_device = NULL

#remove empty col
ypdms = ypdms[, !colSums(is.na(ypdms)) == nrow(ypdms)]

#fix scale 
ypdms$pds_f5_y = ypdms$pds_f5_y - 1
ypdms$pds_f5_y[ypdms$pds_f5_y == 3] = 1

ypdms$menstrualcycle4_y[ypdms$menstrualcycle4_y >= 2] = NA

describe(ypdms)





physicalhealth = merge(ppdms,ypdms)

write.csv(file = "outputs/physicalhealth.csv",x = physicalhealth, row.names = F, na = "")




source("config.R")
source("utility_fun.R")

########### Discrimination ########### 
ydmes01 = load_instrument("abcd_ydmes01",abcd_files_path)

ydmes01[ydmes01 == 777 | ydmes01 == 999] = NA
ydmes01 = droplevels(ydmes01)

summary(ydmes01[ydmes01$eventname == "2_year_follow_up_y_arm_1",])


################### TRAUMA ################### 
#abcd_ptsd01
ptsd01 = load_instrument("abcd_ptsd01",abcd_files_path)


########### Youth Neighborhood Safety/Crime ########### 
nsc01 = load_instrument("abcd_nsc01",abcd_files_path)


########### Parent Neighborhood Safety/Crime ########### 
pnsc01 = load_instrument("abcd_pnsc01",abcd_files_path)
pnsc01 = pnsc01[, !(colnames(pnsc01) %in% c("nei_p_select_language___1"))]


########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)

summary(yle01[yle01$eventname == "2_year_follow_up_y_arm_1",])


########### family relationship section ########### 
acspsw03 = load_instrument("acspsw03",abcd_files_path)
acspsw03 = acspsw03[acspsw03$eventname == "baseline_year_1_arm_1",grepl("src|inter|sex|event|fam", colnames(acspsw03))]

summary(acspsw03)


########### Cyber Bully ########### 
cb = load_instrument("abcd_cb01",abcd_files_path)
cb[cb == 777 | cb == 999] = NA
summary(cb)

########### Longitudinal Parent Diagnostic Interview for DSM-5 Background Items Full ########### 
lpksad01 = load_instrument("abcd_lpksad01",abcd_files_path)
lpksad01 = lpksad01[,c("src_subject_id", "sex", "eventname", "kbi_p_c_bully_l")]
lpksad01[lpksad01 == 777] = NA
lpksad01$kbi_p_c_bully_l[lpksad01$kbi_p_c_bully_l == 2] = 0



########### merge all tables
exposome_set = merge(ydmes01,ptsd01, all = T)
exposome_set = merge(exposome_set,nsc01, all = T)
exposome_set = merge(exposome_set,yle01, all = T)
exposome_set = merge(exposome_set,pnsc01, all = T)
exposome_set = merge(exposome_set,cb, all = T)
exposome_set = merge(exposome_set,lpksad01, all = T)


write.csv(file = "outputs/exposome_set.csv",x = exposome_set, row.names=F, na = "")
write.csv(file = "outputs/family.csv",x = acspsw03, row.names=F, na = "")




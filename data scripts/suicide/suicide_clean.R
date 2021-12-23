
source("config.R")
source("utility_fun.R")

ksad_y = load_instrument("abcd_ksad501",abcd_files_path)

#555 and 888 will be NA
ksad_y[ksad_y == "888" | ksad_y == "555"] = NA



##calculate suicidality
#if one of the items is 1, the result will be 1
#if one of the items is NA and the rest is 0, the result will be NA

#ideation
ksad_y$SI_current_y = apply(ksad_y[,which(grepl("ksads_23_(946|947|948|949|950|951)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SI_past_y = apply(ksad_y[,which(grepl("ksads_23_(957|958|959|960|961|962)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SI_y = (ksad_y$SI_current_y == 1 | ksad_y$SI_past_y == 1)*1


#attempt
ksad_y$SA_current_y = apply(ksad_y[,which(grepl("ksads_23_(952|953|954)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SA_past_y = apply(ksad_y[,which(grepl("ksads_23_(963|964|965)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SA_y = (ksad_y$SA_current_y == 1 | ksad_y$SA_past_y == 1)*1


#combine ideation and attempt
ksad_y$suicidality_current_y = (ksad_y$SI_current_y == 1 | ksad_y$SA_current_y == 1)*1
ksad_y$suicidality_past_y = (ksad_y$SI_past_y == 1 | ksad_y$SA_past_y == 1)*1
ksad_y$suicidality_y = (ksad_y$SI_y == 1 | ksad_y$SA_y == 1)*1

#nssi
ksad_y$nssi_current_y = apply(ksad_y[,which(grepl("ksads_23_(945|955)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$nssi_past_y = apply(ksad_y[,which(grepl("ksads_23_(956|966)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$nssi_y = (ksad_y$nssi_current_y == 1 | ksad_y$nssi_past_y == 1)*1



suicide_set = ksad_y[,grepl("src|inter|event|sex|SI|SA|sui|nssi", colnames(ksad_y))]


write.csv(file = "outputs/suicide_set.csv",x = suicide_set, row.names = F, na = "")





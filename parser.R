#load libraries ----
library(dplyr)
library(readxl)
library(magrittr)
library(stringr)
library(lubridate)
library(XML)
library(data.table)
library(readxl)
library(openxlsx)
library(glue)
library(purrr)
library(tidyr)

#set working directory with codelist output files ----

#commented out for testing ----
#read in example file to get metadata from first tab
# setwd("/lrlhps/data/study_build_team/vdv_codelist_items_compare_tool_output/")
# file_cl_name <- "VDV_Codelist_Items_Compare_Tool_AMAZ_2021-05-19.xlsx"
odm_ded_path <- "/lrlhps/data/study_build_team/odm_ded_files/ODM_DED_2024-12-03.xml"
# coversheet_df <- readxl::read_xlsx(file_cl_name, 
                                   # sheet="Cover_Page")
#get xml data loaded ----
data <- xmlParse(odm_ded_path)

#Setup namespace  ----
nsDefs <- xmlNamespaceDefinitions(data)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
names(ns)[1] <- "x"

#Create ODM_FORM_IG table ----
##get FormDef node 
formdef_nodes <- getNodeSet(data, "//x:FormDef", namespaces=ns)
z1 <- lapply(formdef_nodes, xmlGetAttr, "OID")
z1 <- rbindlist(map(z1, as.data.table)) %>% data.frame() %>% mutate(row_num = row_number())
# formdef_nodes[690:692]

z2_itemgroupoid <- lapply(formdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@ItemGroupOID]", xmlGetAttr, "ItemGroupOID")
  return(r)
})
ig_oid_list <- map(z2_itemgroupoid, as.data.table)
ig_oid <- rbindlist(ig_oid_list, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
ig_oid2 <- gather(ig_oid, "key", "value", -row_num) %>% filter(!is.na(value)) %>% arrange(key)

ODM_FORM_IG <- ig_oid2 %>% 
  left_join(z1, by=c("row_num"="row_num")) %>% 
  rename(Form =V1) %>% 
  select(-row_num,-key) %>% 
  select(`Form OID` = Form, `Item Group OID` = value)

#Create OGM_IG_Item table ----
##get ItemGroupDef node 
itemgrpdef_nodes <- getNodeSet(data, "//x:ItemGroupDef", namespaces=ns)

z1_item <- lapply(itemgrpdef_nodes, xmlGetAttr, "OID")
z1_item <- rbindlist(map(z1_item, as.data.table)) %>% data.frame() %>% mutate(row_num = row_number())


###################CH 14-JUL-2023 code removed as SDTM Domain is not in ODM for ItemGroupDef##################
# z1_domain <- lapply(itemgrpdef_nodes, xmlGetAttr, "Domain")
# z1_domain <- rbindlist(map(z1_domain, as.data.table)) %>% data.frame() %>% mutate(row_num = row_number())





# formdef_nodes[690:692]
z2_itemoid <- lapply(itemgrpdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@ItemOID]", xmlGetAttr, "ItemOID")
  return(r)
})
ioid_list <- map(z2_itemoid, as.data.table)
ioid <- rbindlist(ioid_list, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
ioid2 <- gather(ioid, "key", "value", -row_num) %>% filter(!is.na(value))

z2_itemgroupmand <- lapply(itemgrpdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Mandatory]", xmlGetAttr, "Mandatory")
  return(r)
})
ioid_list_mand <- map(z2_itemgroupmand, as.data.table)
ioid_mand <- rbindlist(ioid_list_mand, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
ioid2_mand <- gather(ioid_mand, "key", "value", -row_num) %>% filter(!is.na(value))

z2_itemgroupitempop <- lapply(itemgrpdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Name='ItemPopulationMethod']", xmlValue)
  return(r)
})
ioid_list_popmeth <- map(z2_itemgroupitempop, as.data.table)
ioid_popmeth <- rbindlist(ioid_list_popmeth, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
ioid2_popmeth <- gather(ioid_popmeth, "key", "value", -row_num) %>% filter(!is.na(value))

z2_itemgroupdefval <- lapply(itemgrpdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  itemoid_nodes <- getNodeSet(subDoc,"//*[@ItemOID]")
  # r <- xpathApply(subDoc, "//*[@Name='DefaultValue']", xmlValue)
  r <- sapply(itemoid_nodes,function(y){
    # xpathSApply(y, "//*[@Name='DefaultValue']", xmlValue, 'DefaultValue')
    lapply(getNodeSet(xmlDoc(y), "//*[@Name='DefaultValue']"),xmlValue)
  })
  return(r)
})
ioid_list_defval <- map(z2_itemgroupdefval, as.data.table)
ioid_defval <- rbindlist(ioid_list_defval, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
ioid2_defval <- gather(ioid_defval, "key", "value", -row_num) %>% filter(!is.na(value)) %>% arrange(key)
ioid2_defval <- ioid2_defval %>% filter(value!="NULL") %>% mutate(value=as.character(value))

ODM_IG_Item <- ioid2 %>%
  cbind(ioid2_mand %>% select(`Item Mandatory` = value)) %>%
  left_join(z1_item, by=c("row_num"="row_num")) %>%
  rename(`Item Group OID`=V1,`Item OID`=value) 

#CH 14-JUL-2023 code removed as SDTM Domain is not in ODM for ItemGroupDef
#  %>% left_join(z1_domain, by=c("row_num"="row_num")) 
#%>%  rename(`SDTM Domain` = V1) 

ODM_IG_Item <- ODM_IG_Item %>% left_join(ioid2_popmeth, by=c("row_num"="row_num", "key"="key")) %>% 
  left_join(ioid2_defval, by=c("row_num"="row_num", "key"="key"))
ODM_IG_Item <- ODM_IG_Item %>% rename(c("Item population Method"="value.x","Default Value"="value.y")) %>% 
  select(-row_num, -key) %>% 
  select(`Item Group OID`, everything())


#Create ODM_Item_DT Sheet ----
itemdef_nodes <- getNodeSet(data, "//x:ItemDef", namespaces=ns) %>% replace_na("NULL")
z1_itemdef_oid <- lapply(itemdef_nodes, xmlGetAttr, "OID")
z1_itemdef_oid <- rbindlist(map(z1_itemdef_oid, as.data.table)) %>% data.frame() %>% mutate(row_num = row_number())

z1_itemdef_text <- lapply(itemdef_nodes, xmlGetAttr, "DataType") %>% replace_na("NULL")
z1_itemdef_text <- rbindlist(map(z1_itemdef_text, as.data.table)) %>% data.frame() %>% mutate(row_num = row_number())

z1_itemdef_len0 <- lapply(itemdef_nodes, xmlGetAttr, "Length") %>% replace_na("NULL")
z1_itemdef_len <- rbindlist(map(z1_itemdef_len0, as.data.table),use.names=TRUE,fill=TRUE) %>% data.frame() %>% mutate(row_num = row_number())

z1_itemdef_sfn0 <- lapply(itemdef_nodes, xmlGetAttr, "SASFieldName") %>% replace_na("NULL")
z1_itemdef_sfn <- rbindlist(map(z1_itemdef_sfn0, as.data.table),use.names=TRUE,fill=TRUE) %>% data.frame() %>% mutate(row_num = row_number())

z1_itemdef_codelist0 <- lapply(itemdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@CodeListOID]", xmlGetAttr, 'CodeListOID')
  return(r)
})
itemdef_list_codelist <- map(z1_itemdef_codelist0, as.data.table)
itemdef_codelist <- rbindlist(itemdef_list_codelist, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
itemdef2_codelist <- gather(itemdef_codelist, "key", "value", -row_num) %>% filter(!is.na(value))

ODM_Item_DT <- cbind(`Item OID` = z1_itemdef_oid,
                     `Item Data Type` = z1_itemdef_text, 
                     `Item Length` = z1_itemdef_len,
                     `SAS Field Name` = z1_itemdef_sfn
) %>% 
  select(ends_with("V1")) %>%
  rename_at(vars(everything()),~str_remove(.,".V1")) %>%
  mutate(row_num = row_number()) %>%
  left_join(itemdef2_codelist, by=c("row_num")) %>%
  select(-row_num, -key) %>% rename("Codelist OID"="value") %>% unique()



#Create ODM_Item_Details Sheet ----
z1_itemdef_origin <- lapply(itemdef_nodes, xmlGetAttr, "Origin") %>% replace_na("NULL")
z1_itemdef_origin <- rbindlist(map(z1_itemdef_origin, as.data.table)) %>% data.frame() %>% mutate(row_num = row_number())



################## code removed as SDTM not in ODM##################
#z1_itemdef_sdtm <- lapply(itemdef_nodes, function(x){
#  subDoc <- xmlDoc(x)
#  r <- xpathApply(subDoc, "//*[@Name='SDTMVariableName']", xmlValue)
#  return(r)
#})
#itemdef_list_sdtm <- map(z1_itemdef_sdtm, as.data.table)
#itemdef_sdtm <- rbindlist(itemdef_list_sdtm, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
#itemdef2_sdtm <- gather(itemdef_sdtm, "key", "value", -row_num) %>% filter(!is.na(value))

#z1_itemdef_sdtm_sqd <- lapply(itemdef_nodes, function(x){
#  subDoc <- xmlDoc(x)
#  r <- xpathApply(subDoc, "//*[@Name='SDTMSuppQualDesc']", xmlValue)
#  return(r)
#})
#itemdef_list_sdtm_sqd <- map(z1_itemdef_sdtm_sqd, as.data.table)
#itemdef_sdtm_sqd <- rbindlist(itemdef_list_sdtm_sqd, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
#itemdef2_sdtm_sqd <- gather(itemdef_sdtm_sqd, "key", "value", -row_num) %>% filter(!is.na(value))
################## code removed as SDTM not in ODM##################



z1_itemdef_dv <- lapply(itemdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Name='DefaultValue']", xmlValue)
  return(r)
})
itemdef_list_dv <- map(z1_itemdef_dv, as.data.table)
itemdef_dv <- rbindlist(itemdef_list_dv, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
#distinct is used to remove repeated values from New ODM
itemdef2_dv <- gather(itemdef_dv, "key", "value", -row_num) %>% filter(!is.na(value)) %>% distinct(row_num, value, .keep_all = TRUE)





z1_itemdef_pm <- lapply(itemdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Name='ItemPopulationMethod']", xmlValue)
  return(r)
})
itemdef_list_pm <- map(z1_itemdef_pm, as.data.table)
itemdef_pm <- rbindlist(itemdef_list_pm, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
#distinct is used to remove repeated values from New ODM
itemdef2_pm <- gather(itemdef_pm, "key", "value", -row_num) %>% filter(!is.na(value)) %>% distinct(row_num, value, .keep_all = TRUE)





z1_itemdef_question <- lapply(itemdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  itemoid_nodes <- getNodeSet(subDoc,"//x:Question", namespaces=ns)
  r <- sapply(itemoid_nodes,function(y){
    lapply(getNodeSet(xmlDoc(y), '//*[@xml:lang="en-US"]'), xmlValue)
  })
  return(r)
})
itemdef_list_question <- map(z1_itemdef_question, as.data.table)
itemdef_question <- rbindlist(itemdef_list_question, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
#distinct is used to remove repeated values from New ODM
itemdef2_question <- gather(itemdef_question, "key", "value", -row_num) %>% filter(!is.na(value)) %>% distinct(row_num, value, .keep_all = TRUE)






#In New ODM (year 2023 Onwards) Column name for "Active Status" for Items is changed to "Item Active Status"
#batch1
z1_itemdef_astat <- lapply(itemdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  #r <- xpathApply(subDoc, "//*[@Name='ActiveStatus']", xmlValue)
  r <- xpathApply(subDoc, "//*[@Name='ItemActiveStatus']", xmlValue)
  return(r)
})

itemdef_list_astat <- map(z1_itemdef_astat, as.data.table)
itemdef_astat <- rbindlist(itemdef_list_astat, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
#distinct is used to remove repeated values from New ODM
itemdef2_astat <- gather(itemdef_astat, "key", "value", -row_num) %>% filter(!is.na(value)) %>% distinct(row_num, value, .keep_all = TRUE)




z1_itemdef_checkallq <- lapply(itemdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Name='CheckAllQuestion']", xmlValue)
  return(r)
})
itemdef_list_checkallq <- map(z1_itemdef_checkallq, as.data.table)
itemdef_checkallq <- rbindlist(itemdef_list_checkallq, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
#distinct is used to remove repeated values from New ODM
itemdef2_checkallq <- gather(itemdef_checkallq, "key", "value", -row_num) %>% filter(!is.na(value)) %>% distinct(row_num, value, .keep_all = TRUE)




z1_itemdef_checkallsn <- lapply(itemdef_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Name='CheckAllSASName']", xmlValue)
  return(r)
})
itemdef_list_checkallsn <- map(z1_itemdef_checkallsn, as.data.table)
itemdef_checkallsn <- rbindlist(itemdef_list_checkallsn, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()

itemdef2_checkallsn <- gather(itemdef_checkallsn, "key", "value", -row_num) %>% filter(!is.na(value)) %>% distinct(row_num, value, .keep_all = TRUE)




#distinct is used to remove repeated values from New ODM
itemdef2_oid <- cbind(z1_itemdef_oid, z1_itemdef_origin %>% select(Origin=V1, -row_num)) %>% distinct(V1, .keep_all = TRUE)


#Post removing repeated values extracted from new ODM the below join will produce limited record not causing memory error
ODM_Item_Details <- itemdef2_oid %>% 
  ##CH 04-JUL-2023: SDTM varibale is not part of XML so need to remove
  #left_join(itemdef2_sdtm, by="row_num") %>% 
  left_join(itemdef2_dv, by="row_num") %>%
  left_join(itemdef2_pm, by="row_num") %>%
  #CH 04-JUL-2023: SDTM varibale is not part of XML so need to remove
  #left_join(itemdef2_sdtm_sqd %>% rename(`SDTM SuppQual Desc` = value), by="row_num") %>%
  left_join(itemdef2_question %>% rename(`Question` = value), by="row_num") %>%
  left_join(itemdef2_astat %>% rename(`Active Status` = value), by="row_num") %>%
  left_join(itemdef2_checkallq %>% rename(`Check All Question` = value), by="row_num") %>%
  left_join(itemdef2_checkallsn %>% rename(`Check All SAS Name` = value), by="row_num") 




################## code removed as SDTM not in ODM##################
#CH 04-JUL-2023: SDTM varibale is not part of XML so need to remove
#ODM_Item_Details <- ODM_Item_Details %>% select(-starts_with("key"), -row_num) %>% 
#  rename(`Item OID`=V1, `SDTM Variable Name`=value.x, `Default Value`=value.y, `Item population Method`=value) %>%
#  ungroup %>%
#  select(`Item OID`, `SDTM Variable Name`,`Default Value`, `Item population Method`, Origin, everything())
################## code removed as SDTM not in ODM##################



#CH 04-JUL-2023: Above code modified as below.
ODM_Item_Details <- ODM_Item_Details %>% 
  select(-starts_with("key"), -row_num) %>% 
  rename(`Item OID`=V1, 
         `Default Value`=value.x, 
         `Item population Method`=value.y) %>%
  ungroup %>%
  select(`Item OID`, 
         `Default Value`, 
         `Item population Method`, 
         Origin, everything())

#Create Codelist ODM ----
codelistref_nodes <- getNodeSet(data, "//x:CodeList", namespaces=ns) %>% replace_na("NULL")

z1_codelist_oid <- lapply(codelistref_nodes, xmlGetAttr, "OID")
z1_codelist_oid <- rbindlist(map(z1_codelist_oid, as.data.table)) %>% data.frame() %>% mutate(row_num = row_number())



z1_codelist_cv <- lapply(codelistref_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@CodedValue]", xmlGetAttr, 'CodedValue')
  return(r)
})
codelist_cv_list <- map(z1_codelist_cv, as.data.table)
codelist_cv <- rbindlist(codelist_cv_list, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
codelist_cv2 <- gather(codelist_cv, "key", "value", -row_num) %>% filter(!is.na(value)) %>% arrange(row_num, key)







##################CH 04-Jul-2023 Decode not in xml ####################
# z1_codelist_label <- lapply(codelistref_nodes, function(x){
#   subDoc <- xmlDoc(x)
#   itemoid_nodes <- getNodeSet(subDoc,"//x:Decode", namespaces=ns)
#   r <- sapply(itemoid_nodes,function(y){
#     # xpathSApply(y, "//*[@TranslatedText]", xmlValue, 'TranslatedText')
#     # lapply(y, "//*[@TranslatedText]"), xmlValue)
#     lapply(getNodeSet(xmlDoc(y), "//*[@xml:lang='en-US']"), xmlValue)
#   })
#   return(r)
# })
# codelist_label_list <- map(z1_codelist_label, as.data.table)
# codelist_label <- rbindlist(codelist_label_list, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
# codelist_label2 <- gather(codelist_label, "key", "value", -row_num) %>% filter(!is.na(value)) %>% arrange(row_num, key)
################## Decode not in xml ####################


########CH 17-Jul-2023 Long Decode willl be used as Label intead of DECODE##############
z1_codelist_codevaluelong <- lapply(codelistref_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Name='LongDecode']", xmlValue)
  return(r)
})
codelist_Longcodevalue <- map(z1_codelist_codevaluelong, as.data.table)
codelist_Longcodevalue <- rbindlist(codelist_Longcodevalue, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
codelist_Longcodevalue2 <- gather(codelist_Longcodevalue, "key", "value", -row_num) %>% filter(!is.na(value)) %>% arrange(row_num, key)



z1_codelist_rank <- lapply(codelistref_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Name='CodeListValueID']", xmlValue)
  return(r)
})
codelist_rank_list <- map(z1_codelist_rank, as.data.table)
codelist_rank <- rbindlist(codelist_rank_list, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
codelist_rank2 <- gather(codelist_rank, "key", "value", -row_num) %>% filter(!is.na(value)) %>% arrange(row_num, key)


z1_codelist_codevalue0 <- lapply(codelistref_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Name='ShortDecode']", xmlValue)
  return(r)
})
codelist_shortcodevalue <- map(z1_codelist_codevalue0, as.data.table)
codelist_shortcodevalue <- rbindlist(codelist_shortcodevalue, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
codelist_shortcodevalue2 <- gather(codelist_shortcodevalue, "key", "value", -row_num) %>% filter(!is.na(value)) %>% arrange(row_num, key)

z1_codelist_astat0 <- lapply(codelistref_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Name='CodeActiveStatus']", xmlValue)
  return(r)
})

#Below code seems redundant so commented.
# z1_codelist_astat0 <- lapply(codelistref_nodes, function(x){
#   subDoc <- xmlDoc(x)
#   itemoid_nodes <- getNodeSet(subDoc,"//x:CodeListItem", namespaces=ns)
#   r <- sapply(itemoid_nodes,function(y){
#     # xpathSApply(y, "//*[@TranslatedText]", xmlValue)
#     # lapply(y, "//*[@ActiveStatus]"), xmlValue)
#     lapply(getNodeSet(xmlDoc(y), "//*[@Name='CodeActiveStatus']"), xmlValue)
#   })
#   return(r)
# })

codelist_astat <- map(z1_codelist_astat0, as.data.table)
codelist_astatvalue <- rbindlist(codelist_astat, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
codelist_astatvalue2 <- gather(codelist_astatvalue, "key", "value", -row_num) %>% filter(!is.na(value)) %>% arrange(row_num, key)

# Codelist_ODM <- z1_codelist_oid %>% select(`Codelist OID`=V1, everything()) %>% 
#   left_join(codelist_cv2, by="row_num") %>% select(-key,-row_num) %>% rename(`Code Value` = value) %>% 
#   cbind(codelist_Longcodevalue2) %>% select(-key, -row_num) %>% rename(`Label` = value) %>%
#   cbind(codelist_rank2) %>% select(-key, -row_num) %>% rename(`Rank` = value) %>%
#   cbind(codelist_shortcodevalue2) %>% select(-key, -row_num) %>% rename(`Short Code Value` = value) %>%
#   cbind(codelist_astatvalue2) %>% select(-key, -row_num) %>% rename(`Active Status` = value)

#CH17-Jul-2023 New code to create for Codelist_ODM added as above code is giving error "arguments imply differing number of rows: ..."
Codelist_ODM <- z1_codelist_oid %>% select(`Codelist OID`=V1, everything()) %>% 
  left_join(codelist_cv2, by="row_num") %>% rename(`Code Value` = value) %>% 
  inner_join(codelist_Longcodevalue2 ,by=c("row_num","key")) %>% rename(`Label` = value) %>% #ODM Label
  inner_join(codelist_rank2,by=c("row_num","key")) %>% rename(`Rank` = value) %>%
  inner_join(codelist_shortcodevalue2 ,by=c("row_num","key")) %>% rename(`Short Code Value` = value) %>%
  inner_join(codelist_astatvalue2 ,by=c("row_num","key")) %>% rename(`Active Status` = value) %>%
  select(-key, -row_num) %>%  unique()


#Create Codelist SVA ----
z1_codelist_oid <- lapply(codelistref_nodes, xmlGetAttr, "OID")
z1_codelist_oid <- rbindlist(map(z1_codelist_oid, as.data.table)) %>% data.frame() %>% mutate(row_num = row_number())

z1_subvalueattr <- lapply(codelistref_nodes, function(x){
  subDoc <- xmlDoc(x)
  r <- xpathApply(subDoc, "//*[@Name='SubmissionValueAttribute']", xmlValue)
  return(r)
})
sub_value_attr_list <- map(z1_subvalueattr, as.data.table)
sub_value_attr <- rbindlist(sub_value_attr_list, fill = TRUE, idcol = T) %>% rename(row_num = .id) %>% data.frame()
sub_value_attr2 <- gather(sub_value_attr, "key", "value", -row_num) %>% filter(!is.na(value)) %>% arrange(row_num, key)

# Codelist_SVA <- cbind(z1_codelist_oid %>% select(`Codelist OID` = V1), 
#                       sub_value_attr %>% select(`Submission Value Attribute` = V1) )


#CH17-Jul-2023 New code to create for Codelist_SVA added as above code is giving error "arguments imply differing number of rows: ..."
Codelist_SVA <- z1_codelist_oid %>% select(`Codelist OID`=V1, everything()) %>% 
  left_join(sub_value_attr %>% select(row_num, `Submission Value Attribute` = V1) , by="row_num") %>%
  select(-row_num) %>%  unique()

#declare list of dataframes here for export----
list_of_datasets <- list(
  "Cover_Page" = coversheet_df()$data(),
  "ODM_FORM_IG" = ODM_FORM_IG,
  "ODM_IG_Item" = ODM_IG_Item, 
  "Codelist_ODM" = Codelist_ODM,
  "Codelist_SVA" = Codelist_SVA,
  "ODM_Item_DT" = ODM_Item_DT,
  "ODM_Item_Details" = ODM_Item_Details
)

#commented out for testing ----
# browser()

#export if needed and has write access ----
StudyName = coversheet_df()$data() %>% 
  filter(Value == "Study_Name") %>% 
  select(Measure) %>%
  pull()

# tryCatch to ensure app still works if user doesnt have write access ----
tryCatch(
  openxlsx::write.xlsx(list_of_datasets,
                       paste0("/lrlhps/data/study_build_team/custom-odm-parsing/",
                              "ODM-DED-PROC-", StudyName, "_", today(), ".xlsx")), 
  error = function(err){
    print("user probably doesnt have write access to the folder path")
  }
)



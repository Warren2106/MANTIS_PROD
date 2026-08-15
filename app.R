##################################################################################################################################
#Title: Metrics Analytics Network Technological Integrated Systems (MANTIS)
#       -- combined with the VDV ODM Parser app as an in-app tab (previously an iframe to a separate Posit Connect deployment)
#
#Author(s): Elana Spell (c271661), Jonathan Schacht (c303639),  Aviad Adlersberg (c242664)
#VDV ODM Parser CODE NAME          : ODM Parser From VDV (https://github.com/EliLillyCo/LRL_ODMParserFromVDV_App/blob/main/app.R)
#VDV ODM Parser Author             : Sam Parmar (Author), Ben Nealy (Validator), Mudit (Publish to R Connect)
#
#Application Code Location: /lrlindy01/lrl01.grp/ODR/03_Project/LRL_MANTIS/LRL_MANTIS/app.R
#Dev Link: https://shiny-dev.am.lilly.com/MANTIS/ 

#linux commands to control permissions
# chgrp Spiny_Flower_MANTIS /lrlhps/data/MANTIS
# chmod 770 /lrlhps/data/MANTIS
# ls -ld /lrlhps/data/MANTIS
#required user groups
# Spiny_Flower_MANTIS - - - all users
# Ghost_MANTIS - - - MANTIS developers

##################################################################################################################################

#setwd("/lrlhps/data/MANTIS/LRL_MANTIS/Mantis")
#working directory for MANTIS setwd("/lrlhps/data/MANTIS")
#allow for larger files to be uploaded
options(shiny.maxRequestSize = 1000 * 1024^2)
Sys.umask('0007') 

##################################################################################################################################
## ALL LIBRARY CALLS (combined from MANTIS app and VDV ODM Parser app) ----
##################################################################################################################################
library(shiny)
library(plyr)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)
library(data.table)
library(shinybusy)
library(zoo)
library(stringi)
library(conflicted)
library(DT)
library(lubridate)
library(stringr)
library(shinyjs)
library(glue)
library(datamods)
library(shinyBS)
library(shinyalert)
library(praise)

conflicts_prefer(plyr::mutate)
conflicted::conflicts_prefer(shinydashboard::box)
conflicts_prefer(DT::dataTableOutput)
conflicts_prefer(dplyr::filter)

##define functions used in the application
SDS_DED_FUN<-function(odm_forms,odm_items,odm_code,odm_sdtm,odm_code_item,Lib_items,asses,Lib_codes,mapped_items){
  #ODM DED
  odm_forms <- data.frame(odm_forms())
  odm_items <- data.frame(odm_items())
  odm_code <- data.frame(odm_code())
  odm_sdtm <- data.frame(odm_sdtm())
  odm_code_item <- data.frame(odm_code_item())
  
  
  names(odm_forms) <- make.names(names(odm_forms))
  names(odm_items) <- make.names(names(odm_items))
  names(odm_code) <- make.names(names(odm_code))
  names(odm_sdtm) <- make.names(names(odm_sdtm))
  names(odm_code_item) <- make.names(names(odm_code_item))
  
  #Join items to forms
  
  odm_forms %>%
    left_join(odm_items, by = c("Item.Group.OID" = "Item.Group.OID"))->odm_combine
  
  #remove duplicate columns
  odm_combine %>% select(-Default.Value)->odm_combine
  #odm_sdtm %>% select(-Item.population.Method)->odm_sdtm
  
  #add SDTM info
  odm_combine %>%
    left_join(odm_sdtm, by = c("Item.OID" = "Item.OID"))->odm_combine
  
  #add Codelist and SAS  info
  odm_combine %>%
    left_join(odm_code_item, by = c("Item.OID" = "Item.OID"))->odm_combine
  
  
  
  ded_code<-odm_code
  ded<-odm_combine
  
  
  #rename columns
  ded %>% 
    dplyr::rename(DED.Form.OID = Form.OID)%>%
    dplyr::rename(Mandatory = Item.Mandatory)%>%
    #dplyr::rename(DED.Item.Group.SDTM.Domain = SDTM.Domain) %>%
    dplyr::rename(Item.Active.Status = Active.Status) %>%
    dplyr::rename(Item.SAS.Field.Name = SAS.Field.Name)->ded
  
  
  ded_code %>% 
    dplyr::rename(Code = Code.Value)%>%
    dplyr::rename(Long.Decode = Label)%>%
    dplyr::rename(Codelist.Active.Status = Active.Status)  ->ded_code
  
  
  #Placehoders for testing
  #ded$DED.Item.Group.SDTM.Domain <-"Place_Hold"
  #ded$Item.Active.Status <-"Place_Hold"
  #ded_code$Codelist.Active.Status <-"Place_Hold"
  
  #subset active codes
  ded %>% select(Item.OID,Item.Active.Status)->ded_active
  ded_code %>% select(Codelist.OID, Codelist.Active.Status) -> ded_code_active
  
  
  
  
  Lib_items <- data.frame(Lib_items())
  names(Lib_items) <- make.names(names(Lib_items)) 
  
  links <-Lib_items
  
  
  
  asses <- data.frame(asses())
  names(asses) <- make.names(names(asses)) 
  
  # MODIFIED: Updated column names from Selection.Name/Selection.Label
  #           to Choice.Name/Choice.Label per SDS specification change
  asses %>% select(Name,Item.External.ID,Item.Label,Data.Type,Label, Choice.Name,Choice.Label)->asses_lup
  #fill in form IDs
  zoo::na.locf(asses$Name)->asses$Name
  
  asses %>% filter(!is.na(asses$Item.External.ID))->asses
  
  #rename columns
  asses %>% 
    dplyr::rename(Question = Item.Label)%>% 
    dplyr::rename(Item.Name = Item.External.ID)->asses
  
  
  #end update
  
  Lib_codes <- data.frame(Lib_codes())
  names(Lib_codes) <- make.names(names(Lib_codes)) 
  
  ### Mapping file####
  
  mapped_items <- data.frame(mapped_items())
  
  names(mapped_items) <- make.names(names(mapped_items)) 
  
  
  
  
  #20R2 update
  if("Label" %in% colnames(Lib_items))
  {
    Lib_items %>% 
      dplyr::rename(Label...15 = Label)->Lib_items
    
  }
  
  if("Form.Name" %in% colnames(Lib_items))
  {
    Lib_items %>% 
      dplyr::rename(Name = Form.Name)->Lib_items
    
  }
  
  
  if("Form.Name" %in% colnames(links))
  {
    links %>% 
      dplyr::rename(Name = Form.Name)->links
    
  }
  
  
  if("Label" %in% colnames(links))
  {
    links %>% 
      dplyr::rename(Label...2 = Label)->links
    
  }
  
  
  ##### Items #####
  
  
  
  #Fill in missing SDTM Variables
  #ded$SDTM.Variable.Name <- ifelse(is.na(ded$SDTM.Variable.Name), ded$SDTM.SuppQual.Desc, ded$SDTM.Variable.Name)
  
  #get rid of labels
  Lib_items %>%
    filter(Data.Type != "Label") ->Lib_items
  
  
  #Trim setup
  Lib_items$item_trim<-gsub("_.*", "", Lib_items$Item.Name)
  
  #Trim everything after last underscore
  Lib_items$item_trim_second<-sub("_[^_]+$", "", Lib_items$Item.Name)
  
  #Trim primary domain
  Lib_items$domain_trim_prime<-gsub("_.*", "", Lib_items$Name)
  
  #Trim secondary domain
  #Trim everything after last underscore
  Lib_items$domain_trim_second<-sub("_[^_]+$", "", Lib_items$Name)
  Lib_items$domain_trim_second<-gsub(".*?\\_","",Lib_items$domain_trim_second)
  
  #Concat primary
  ifelse(is.na(Lib_items$item_trim ),  Lib_items$prime_concat<-"",paste(Lib_items$domain_trim_prime,Lib_items$item_trim)) -> Lib_items$prime_concat
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  Lib_items$prime_concat = sub(searchString,replacementString,Lib_items$prime_concat)
  
  
  #Concat secondary
  ifelse(is.na(Lib_items$item_trim ),  Lib_items$second_concat<-"",paste(Lib_items$domain_trim_second,Lib_items$item_trim))->Lib_items$second_concat
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  Lib_items$second_concat = sub(searchString,replacementString,Lib_items$second_concat)
  
  
  
  #Concat primary
  ifelse(is.na(Lib_items$item_trim_second ),  Lib_items$prime_concat_second<-"",paste(Lib_items$domain_trim_prime,Lib_items$item_trim_second))->Lib_items$prime_concat_second
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  Lib_items$prime_concat_second = sub(searchString,replacementString,Lib_items$prime_concat_second)
  
  
  #Concat secondary
  ifelse(is.na(Lib_items$item_trim_second ),  Lib_items$second_concat_second<-"",paste(Lib_items$domain_trim_second,Lib_items$item_trim_second))->Lib_items$second_concat_second
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  Lib_items$second_concat_second = sub(searchString,replacementString,Lib_items$prime_concat_second)
  
  
  
  
  
  
  
  
  #Concat primary Raw
  ifelse(is.na(Lib_items$Item.Name),  Lib_items$prime_concat_raw<-"",paste(Lib_items$domain_trim_prime,Lib_items$Item.Name))->Lib_items$prime_concat_raw
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  Lib_items$prime_concat_raw = sub(searchString,replacementString,Lib_items$prime_concat_raw)
  
  
  #Concat secondary
  ifelse(is.na(Lib_items$Item.Name ),  Lib_items$second_concat_raw<-"",paste(Lib_items$domain_trim_second,Lib_items$Item.Name))->Lib_items$second_concat_raw
  
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  Lib_items$second_concat_raw = sub(searchString,replacementString,Lib_items$second_concat_raw)
  
  #Hybrid
  ifelse(Lib_items$domain_trim_prime == Lib_items$domain_trim_second,Lib_items$hybrid <-"No","Yes")->Lib_items$hybrid
  
  
  #prepare DED
  #concat items
  paste(ded$DED.Form.OID,ded$Item.OID)->ded$concat_ded
  #replace any spaces
  ded$concat_ded = sub(searchString,replacementString,ded$concat_ded)
  
  #subste for merge
  #ded%>% select(DED.Form.OID,Item.OID,Item.SAS.Field.Name,SDTM.Variable.Name,SDTM.SuppQual.Desc,concat_ded,Mandatory, Question)->ded_sub
  ded%>% select(DED.Form.OID,Item.OID,Item.SAS.Field.Name,concat_ded,Mandatory, Question)->ded_sub
  
  ded_sub%>%
    dplyr::rename(SAS.Field.Name = Item.SAS.Field.Name)->ded_sub
  
  
  #Join
  Lib_items %>%
    left_join(ded_sub, by = c("prime_concat_raw" = "concat_ded"))->Lib_items
  
  Lib_items %>%
    left_join(ded_sub, by = c("second_concat_raw" = "concat_ded"))->Lib_items
  
  Lib_items %>%
    left_join(ded_sub, by = c("prime_concat" = "concat_ded"))->Lib_items
  
  
  Lib_items %>%
    left_join(ded_sub, by = c("second_concat" = "concat_ded"))->Lib_items
  
  Lib_items %>%
    left_join(ded_sub, by = c("prime_concat_second" = "concat_ded"))->Lib_items
  
  Lib_items %>%
    left_join(ded_sub, by = c("second_concat_second" = "concat_ded"))->Lib_items
  
  Lib_items %>%
    left_join(ded_sub, by = c("second_concat_second" = "concat_ded"))->Lib_items
  
  
  #remove duplicate rows
  Lib_items[!duplicated(Lib_items), ] -> Lib_items
  
  #Questions
  ifelse(!is.na(Lib_items$Question.x),Lib_items$Question <- Lib_items$Question.x,
         ifelse(!is.na(Lib_items$Question.y),Lib_items$Question <- Lib_items$Question.y,
                ifelse(!is.na(Lib_items$Question.x.x),Lib_items$Question <- Lib_items$Question.x.x, 
                       ifelse(!is.na(Lib_items$Question.y.y),Lib_items$Question <- Lib_items$Question.y.y,
                              ifelse(!is.na(Lib_items$Question.y.y.y),Lib_items$Question <- Lib_items$Question.y.y.y,
                                     ifelse(!is.na(Lib_items$Question.x.x.x),Lib_items$Question <- Lib_items$Question.x.x.x,"Missing"))))))-> Lib_items$Question
  
  #Form OIDS
  ifelse(!is.na(Lib_items$DED.Form.OID.x ),Lib_items$DED.Form.OID <- Lib_items$DED.Form.OID.x,
         ifelse(!is.na(Lib_items$DED.Form.OID.y),Lib_items$DED.Form.OID <- Lib_items$DED.Form.OID.y,
                ifelse(!is.na(Lib_items$DED.Form.OID.x.x),Lib_items$DED.Form.OID <- Lib_items$DED.Form.OID.x.x, 
                       ifelse(!is.na(Lib_items$DED.Form.OID.y.y),Lib_items$DED.Form.OID <- Lib_items$DED.Form.OID.y.y,
                              ifelse(!is.na(Lib_items$DED.Form.OID.y.y.y),Lib_items$DED.Form.OID <- Lib_items$DED.Form.OID.y.y.y,
                                     ifelse(!is.na(Lib_items$DED.Form.OID.x.x.x),Lib_items$DED.Form.OID <- Lib_items$DED.Form.OID.x.x.x,"Missing"))))))-> Lib_items$DED.Form.OID
  
  
  #Mandaotry
  ifelse(!is.na(Lib_items$Mandatory.x),Lib_items$Mandatory <- Lib_items$Mandatory.x,
         ifelse(!is.na(Lib_items$Mandatory.y),Lib_items$Mandatory <- Lib_items$Mandatory.y,
                ifelse(!is.na(Lib_items$Mandatory.x.x),Lib_items$Mandatory <- Lib_items$Mandatory.x.x, 
                       ifelse(!is.na(Lib_items$Mandatory.y.y),Lib_items$Mandatory <- Lib_items$Mandatory.y.y,
                              ifelse(!is.na(Lib_items$Mandatory.y.y.y),Lib_items$Mandatory <- Lib_items$Mandatory.y.y.y,
                                     ifelse(!is.na(Lib_items$Mandatory.x.x.x),Lib_items$Mandatory <- Lib_items$Mandatory.x.x.x,"Missing"))))))-> Lib_items$Mandatory
  
  
  #Subset
  Lib_items %>% select(Name,Item.Name,Label...15,Codelist,Item.OID.x,Item.OID.y,Item.OID.x.x,Item.OID.y.y, Item.OID.x.x.x,Item.OID.y.y.y,Mandatory,Codelist,Question,DED.Form.OID,SAS.Field.Name)->Library_items
  
  
  
  #Collapse ItemOID
  ifelse(!is.na(Library_items$Item.OID.x),Library_items$Item_OID <- Library_items$Item.OID.x,
         ifelse(!is.na(Library_items$Item.OID.y),Library_items$Item_OID  <- Library_items$Item.OID.y,
                ifelse(!is.na(Library_items$Item.OID.x.x),Library_items$Item_OID  <- Library_items$Item.OID.x.x, 
                       ifelse(!is.na(Library_items$Item.OID.y.y),Library_items$Item_OID  <- Library_items$Item.OID.y.y,
                              ifelse(!is.na(Library_items$Item.OID.y.y.y),Library_items$Item_OID  <- Library_items$Item.OID.y.y.y
                                     ,ifelse(!is.na(Library_items$Item.OID.x.x.x),Library_items$Item_OID  <- Library_items$Item.OID.x.x.x,"Missing"))))))-> Library_items$Item_OID 
  
  
  
  
  
  
  Library_items %>% 
    dplyr::rename(Label = Label...15)->Library_items
  #delete blank items
  Library_items[!is.na(Library_items$Item.Name), ]->Library_items
  
  Library_items %>% select(Name,Item.Name,Label,Item_OID, SAS.Field.Name,Mandatory,Codelist,Question,DED.Form.OID)->Library_items
  
  #Remove Status and Trig itmes, we don't need them
  dplyr::filter(Library_items, !grepl('TRIG_.*', Item.Name))->Library_items
  
  dplyr::filter(Library_items, !grepl('TRIG*', Name))->Library_items
  dplyr::filter(Library_items, !grepl('*DUMMY*', Item.Name))->Library_items
  
  
  ##update
  
  
  #Trim setup
  asses$item_trim<-gsub("_.*", "", asses$Item.Name)
  
  #Trim everything after last underscore
  asses$item_trim_second<-sub("_[^_]+$", "", asses$Item.Name)
  
  #Trim primary domain
  asses$domain_trim_prime<-gsub("_.*", "", asses$Name)
  
  #Trim secondary domain
  #Trim everything after last underscore
  asses$domain_trim_second<-sub("_[^_]+$", "", asses$Name)
  asses$domain_trim_second<-gsub(".*?\\_","",asses$domain_trim_second)
  
  #Concat primary
  ifelse(is.na(asses$item_trim ),  asses$prime_concat<-"",paste(asses$domain_trim_prime,asses$item_trim)) -> asses$prime_concat
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  asses$prime_concat = sub(searchString,replacementString,asses$prime_concat)
  
  
  #Concat secondary
  ifelse(is.na(asses$item_trim ),  asses$second_concat<-"",paste(asses$domain_trim_second,asses$item_trim))->asses$second_concat
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  asses$second_concat = sub(searchString,replacementString,asses$second_concat)
  
  
  
  #Concat primary
  ifelse(is.na(asses$item_trim_second ),  asses$prime_concat_second<-"",paste(asses$domain_trim_prime,asses$item_trim_second))->asses$prime_concat_second
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  asses$prime_concat_second = sub(searchString,replacementString,asses$prime_concat_second)
  
  
  #Concat secondary
  ifelse(is.na(asses$item_trim_second ),  asses$second_concat_second<-"",paste(asses$domain_trim_second,asses$item_trim_second))->asses$second_concat_second
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  asses$second_concat_second = sub(searchString,replacementString,asses$prime_concat_second)
  
  
  
  
  
  
  
  
  #Concat primary Raw
  ifelse(is.na(asses$Item.Name),  asses$prime_concat_raw<-"",paste(asses$domain_trim_prime,asses$Item.Name))->asses$prime_concat_raw
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  asses$prime_concat_raw = sub(searchString,replacementString,asses$prime_concat_raw)
  
  
  #Concat secondary
  ifelse(is.na(asses$Item.Name ),  asses$second_concat_raw<-"",paste(asses$domain_trim_second,asses$Item.Name))->asses$second_concat_raw
  
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  asses$second_concat_raw = sub(searchString,replacementString,asses$second_concat_raw)
  
  #Hybrid
  ifelse(asses$domain_trim_prime == asses$domain_trim_second,asses$hybrid <-"No","Yes")->asses$hybrid
  
  
  
  
  #Join
  asses %>%
    left_join(ded_sub, by = c("prime_concat_raw" = "concat_ded"))->asses
  
  asses %>%
    left_join(ded_sub, by = c("second_concat_raw" = "concat_ded"))->asses
  
  asses %>%
    left_join(ded_sub, by = c("prime_concat" = "concat_ded"))->asses
  
  
  asses %>%
    left_join(ded_sub, by = c("second_concat" = "concat_ded"))->asses
  
  asses %>%
    left_join(ded_sub, by = c("prime_concat_second" = "concat_ded"))->asses
  
  asses %>%
    left_join(ded_sub, by = c("second_concat_second" = "concat_ded"))->asses
  
  asses %>%
    left_join(ded_sub, by = c("second_concat_second" = "concat_ded"))->asses
  
  
  #remove duplicate rows
  asses[!duplicated(asses), ] -> asses
  
  
  
  #Form OIDS
  ifelse(!is.na(asses$DED.Form.OID.x ),asses$DED.Form.OID <- asses$DED.Form.OID.x,
         ifelse(!is.na(asses$DED.Form.OID.y),asses$DED.Form.OID <- asses$DED.Form.OID.y,
                ifelse(!is.na(asses$DED.Form.OID.x.x),asses$DED.Form.OID <- asses$DED.Form.OID.x.x, 
                       ifelse(!is.na(asses$DED.Form.OID.y.y),asses$DED.Form.OID <- asses$DED.Form.OID.y.y,
                              ifelse(!is.na(asses$DED.Form.OID.y.y.y),asses$DED.Form.OID <- asses$DED.Form.OID.y.y.y,
                                     ifelse(!is.na(asses$DED.Form.OID.x.x.x),asses$DED.Form.OID <- asses$DED.Form.OID.x.x.x,"Missing"))))))-> asses$DED.Form.OID
  
  
  #Mandaotry
  ifelse(!is.na(asses$Mandatory.x),asses$Mandatory <- asses$Mandatory.x,
         ifelse(!is.na(asses$Mandatory.y),asses$Mandatory <- asses$Mandatory.y,
                ifelse(!is.na(asses$Mandatory.x.x),asses$Mandatory <- asses$Mandatory.x.x, 
                       ifelse(!is.na(asses$Mandatory.y.y),asses$Mandatory <- asses$Mandatory.y.y,
                              ifelse(!is.na(asses$Mandatory.y.y.y),asses$Mandatory <- asses$Mandatory.y.y.y,
                                     ifelse(!is.na(asses$Mandatory.x.x.x),asses$Mandatory <- asses$Mandatory.x.x.x,"Missing"))))))-> asses$Mandatory
  
  
  #Collapse ItemOID
  ifelse(!is.na(asses$Item.OID.x),asses$Item_OID <- asses$Item.OID.x,
         ifelse(!is.na(asses$Item.OID.y),asses$Item_OID  <- asses$Item.OID.y,
                ifelse(!is.na(asses$Item.OID.x.x),asses$Item_OID  <- asses$Item.OID.x.x, 
                       ifelse(!is.na(asses$Item.OID.y.y),asses$Item_OID  <- asses$Item.OID.y.y,
                              ifelse(!is.na(asses$Item.OID.y.y.y),asses$Item_OID  <- asses$Item.OID.y.y.y
                                     ,ifelse(!is.na(asses$Item.OID.x.x.x),asses$Item_OID  <- asses$Item.OID.x.x.x,"Missing"))))))-> asses$Item_OID 
  
  
  #rename columns
  asses %>% 
    dplyr::rename(Question = Question.x)->asses
  #Subset
  #update_04_06_2022
  asses %>% select(Item.Name,Item_OID,Question,DED.Form.OID,Name,Mandatory)->asses
  #end update
  
  #####CDB######
  
  
  #form oid list from library items
  Library_items %>% select(Item.Name,Name,DED.Form.OID) -> form_oids
  
  ##SDTM domnains for FORM OIDS
  
  ded %>% select(Item.OID,DED.Form.OID) -> ded_sdtm_domain
  
  #delete duplicates
  ded_sdtm_domain[!duplicated(ded_sdtm_domain), ] -> ded_sdtm_domain
  
  #concat items
  paste(form_oids$Item.Name,form_oids$DED.Form.OID)->form_oids$concat
  
  paste(ded_sdtm_domain$Item.OID,ded_sdtm_domain$DED.Form.OID)->ded_sdtm_domain$concat
  
  
  
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  
  form_oids$concat= sub(searchString,replacementString,form_oids$concat)
  ded_sdtm_domain$concat= sub(searchString,replacementString,ded_sdtm_domain$concat)
  
  
  #subset
  ded_sdtm_domain %>% select(concat) ->ded_sdtm_domain
  
  
  #Join
  form_oids %>%
    left_join(ded_sdtm_domain, by = c("concat" = "concat"))->form_oids_sdtm
  
  form_oids_sdtm %>% select(-Name, -Item.Name, -DED.Form.OID)->form_oids_sdtm
  
  #delete duplicates
  form_oids_sdtm[!duplicated(form_oids_sdtm), ] -> form_oids_sdtm
  
  #remove blank domains
  
  na.omit(form_oids_sdtm) ->form_oids_sdtm 
  
  
  #CDB
  
  Library_items %>%
    select(Item_OID,Question,DED.Form.OID,Name,Mandatory,Item.Name) ->CDB_items
  
  
  
  #filter for Extracted = Yes
  #get rid of items wihtout a codelists
  mapped_items %>% filter(Include.in.Extracts.Yes.No. == "Yes")->mapped_items
  
  #select
  mapped_items %>% select(Item.Name.OID, Lilly.DED.Form.OID, Study.Form.Name,Codelist.Name,Default.Value,Logic.to.Hard.Code.Value.in.Dataset, Value.to.be.Hard.Coded)->mapped_items_sub
  
  #concat items
  paste(mapped_items_sub$Lilly.DED.Form.OID,mapped_items_sub$Item.Name.OID)->mapped_items_sub$concat
  #replace any spaces
  mapped_items_sub$concat= sub(searchString,replacementString,mapped_items_sub$concat)
  
  #Join
  mapped_items_sub %>%
    left_join(ded_sub, by = c("concat" = "concat_ded"))->mapped_items_comb
  
  #set value
  ifelse(!is.na(mapped_items_comb$Logic.to.Hard.Code.Value.in.Dataset),mapped_items_comb$mapping<- mapped_items_comb$Logic.to.Hard.Code.Value.in.Dataset,
         ifelse(!is.na(mapped_items_comb$Value.to.be.Hard.Coded),mapped_items_comb$mapping <- mapped_items_comb$Value.to.be.Hard.Coded,
                ifelse(!is.na(mapped_items_comb$Default.Value),mapped_items_comb$mapping<- mapped_items_comb$Default.Value, "Missing")))-> mapped_items_comb$mapping
  
  
  #select
  mapped_items_comb %>% select(Item.Name.OID, Question, Lilly.DED.Form.OID, Study.Form.Name, Mandatory, mapping)->mapped_items_comb_2
  
  #remove duplicate rows
  mapped_items_comb_2[!duplicated(mapped_items_comb_2), ] -> mapped_items_comb_2
  
  
  #rename
  mapped_items_comb_2 %>% 
    dplyr::rename(Item_OID = Item.Name.OID) %>% 
    dplyr::rename(DED.Form.OID = Lilly.DED.Form.OID) %>% 
    dplyr::rename(Name = Study.Form.Name)%>% 
    dplyr::rename(Item.Name = mapping) ->mapped_items_comb_3
  
  #Bind study items becuase we need to know where all items are being mapped not just the derived ones from the mapping spec
  CDB_items_map <- rbind(CDB_items, mapped_items_comb_3)
  
  
  #Join SDTM domain
  
  #concat items
  paste(CDB_items_map$Item_OID,CDB_items_map$DED.Form.OID)->CDB_items_map$concat 
  #replace any spaces
  CDB_items_map$concat= sub(searchString,replacementString,CDB_items_map$concat) 
  
  CDB_items_map %>%
    left_join(ded_sdtm_domain, by = c("concat" = "concat"))->CDB_items_map 
  
  CDB_items_map %>% select(-concat) -> CDB_items_map 
  
  
  
  #Active status CDB items
  CDB_items_map %>%
    left_join(ded_active, by = c("Item_OID" = "Item.OID"))->CDB_items_map
  
  #remove duplicate rows
  CDB_items_map[!duplicated(CDB_items_map), ] -> CDB_items_map
  
  
  ded %>% select(DED.Form.OID) -> ded_sdtm_domain_as
  
  if(nrow(asses)>0){
    asses %>%
      left_join(ded_sdtm_domain_as, by = c("DED.Form.OID" = "DED.Form.OID"))->asses
    asses$Item.Active.Status <-"A"
    asses[!duplicated(asses), ] -> asses
    
    asses %>% select(Item_OID,Question,DED.Form.OID,Name,Mandatory,Item.Name,Item.Active.Status)->asses
    
    rbind(CDB_items_map,asses)->CDB_items_map
    
  }
  
  
  #Transpose
  
  CDB_items_map %>%
    tibble::rowid_to_column()%>% 
    spread(key = Name, value = Item.Name) ->CDB
  
  CDB %>%  select(-rowid)->CDB
  
  #Colapse rows into one
  
  CDB_items_map %>%  select (Item.Name)->item_name
  
  
  
  CDB = cbind(CDB, item_name)
  
  CDB[!duplicated(CDB), ] -> CDB
  
  
  
  #concat form and item oids- we need to do this so if the same item is used on different forms we want to keep them as seperate rows
  paste(CDB$DED.Form.OID,CDB$Item.Name,CDB$Item_OID)->CDB$concat
  #Collapse into single rows
  setDT(CDB)[, lapply(.SD, function(x) x[!is.na(x)][1L]), by = concat]->CDB2
  CDB2 %>%  select(-concat)->CDB2
  
  
  
  
  #UPDATE
  
  #Update collapse per Form and ITEM OID so we can be on one row
  
  ##test- added item.name
  paste(CDB2$DED.Form.OID,CDB2$Item.Name,CDB2$Item_OID)->CDB2$concat
  paste(CDB2$DED.Form.OID,CDB2$Item_OID)->CDB2$concat2
  #end test
  #Collapse into single rows
  setDT(CDB2)[, lapply(.SD, function(x) x[!is.na(x)][1L]), by = concat]->CDB2
  
  
  #collapse with seperator by item OID and FOrm OID concat
  #pick out the columns to collapse
  CDB2 %>% select(concat,concat2,c(6:ncol(CDB2)))->CDB3
  
  #collapse and paste
  
  CDB3[, lapply(.SD,   function(x) paste0(x[!is.na(x)], collapse=",")), by=concat2]->CDB3
  
  
  #select the columns with the non collapse data
  
  CDB2 %>% select (Item_OID:Item.Active.Status)->CDB4
  paste(CDB4$DED.Form.OID,CDB4$Item_OID)->CDB4$concat
  
  
  CDB3 %>% select(concat2,c(4:ncol(CDB3)))->CDB3
  
  #join 
  CDB4 %>%
    left_join(CDB3, by = c("concat" = "concat2"))->CDB5
  
  
  CDB5[!duplicated(CDB5), ] -> CDB5
  
  
  CDB5 -> CDB2
  
  #drop item name
  CDB2 %>% select(-concat) ->CDB2
  
  #End Update
  
  
  #rename extract listing
  CDB2 %>% 
    dplyr::rename(Extract_Listing = DED.Form.OID)->CDB2
  
  
  #drop item name
  CDB2 %>% select(-Item.Name) ->CDB2
  
  
  #####code Lists########
  
  #trim the cl form codelists
  sub("cl","",Library_items$Codelist)->Library_items$code_trim
  #Trim everything after last underscore
  Library_items$code_trim_num<-sub("_[^_]+$", "", Library_items$code_trim)
  
  #subset ded codelist
  ded_code %>% select(Codelist.OID,Code, Long.Decode) ->ded_code_Sub
  
  #subset library codes
  Lib_codes %>% select(Name,Choice.Code,Choice.Label)->lib_codes_sub
  
  lib_codes_sub %>% filter(!is.na(Choice.Code))->lib_codes_sub
  
  
  #ODM Update
  #Create codelist ref for Boleen lookup values
  ded %>% filter (!is.na(Check.All.Question), !is.na(Codelist.OID)) ->bool_code
  
  bool_code%>%select(Item.OID, Codelist.OID)->bool_code
  #End ODM Update
  
  #subset boleen items
  Lib_items %>% select(Item.Name, Data.Type) %>% filter(Data.Type == "Boolean")->bool
  
  #join boolean with codelist oids
  bool %>%
    left_join(bool_code, by = c("Item.Name" = "Item.OID"))->bool2
  
  bool2[!duplicated(bool2), ] -> bool2
  
  #prepare for merge by dropping data type
  
  bool2 %>% select (-Data.Type)->bool2
  
  #merge with library items
  Library_items ->Library_items_bool
  
  #Drop columns used for joining codelists
  Library_items %>% select(-code_trim,-code_trim_num)->Library_items
  
  #join
  
  Library_items_bool %>%
    left_join(bool2, by = c("Item.Name" = "Item.Name"))->Library_items_bool
  
  #ODM Update
  Library_items_bool$code_trim<- ifelse(!is.na(Library_items_bool$Codelist.OID), Library_items_bool$Codelist.OID, Library_items_bool$code_trim)
  
  Library_items_bool$Codelist<- ifelse(!is.na(Library_items_bool$Codelist.OID), Library_items_bool$Codelist.OID, Library_items_bool$Codelist)
  #End ODM Update
  
  #Join
  Library_items_bool %>%
    left_join(lib_codes_sub, by = c("Codelist" = "Name"))->Library_items_code
  
  #ODM Update
  Library_items_code$Choice.Code<- ifelse(!is.na(Library_items_code$Codelist.OID), Library_items_code$Label, Library_items_code$Choice.Code)
  Library_items_code$Choice.Label<- ifelse(!is.na(Library_items_code$Codelist.OID), Library_items_code$Label, Library_items_code$Choice.Label)
  #End ODM Update
  
  #create a SAS lookup table to add 
  ded %>% select(Item.OID, Item.SAS.Field.Name, Check.All.SAS.Name)->sas
  
  #replace SAS field with Check all if available
  
  sas$Item.SAS.Field.Name<- ifelse(!is.na(sas$Check.All.SAS.Name), sas$Check.All.SAS.Name, sas$Item.SAS.Field.Name)
  
  #remove checks all column
  
  sas %>% select (-Check.All.SAS.Name) -> sas
  
  #Join
  Library_items_code %>%
    left_join(sas, by = c("Item_OID" = "Item.OID"))->Library_items_code
  
  #get rid of items wihtout a codelists
  Library_items_code %>% filter(!is.na(Choice.Code))->Library_items_code
  
  #ODM update2
  odm_code_item %>%
    select (Item.OID, Codelist.OID)->ded_code_Ref
  
  ded_code_Ref$trim_ref <- ded_code_Ref$Codelist.OID
  
  
  ded_code_Ref %>% select ( -Codelist.OID)->ded_code_Ref
  
  #####
  Library_items_code %>%
    left_join(ded_code_Ref, by = c("Item_OID" = "Item.OID"))->Library_items_code
  
  #end update2
  
  #concat items
  paste(Library_items_code$code_trim,Library_items_code$Choice.Code)->Library_items_code$concat_libcode_trim
  
  paste(Library_items_code$code_trim_num,Library_items_code$Choice.Code)->Library_items_code$concat_libcode_trim_num
  
  
  paste(Library_items_code$trim_ref,Library_items_code$Choice.Code)->Library_items_code$trim_ref
  
  
  paste(ded_code_Sub$Codelist.OID,ded_code_Sub$Code)->ded_code_Sub$concat_ded_code
  
  
  
  #set to lower case so we can make sure we get a match
  tolower(ded_code_Sub$concat_ded_code)->ded_code_Sub$concat_ded_code
  tolower(Library_items_code$concat_libcode_trim)->Library_items_code$concat_libcode_trim
  tolower(Library_items_code$concat_libcode_trim_num)->Library_items_code$concat_libcode_trim_num
  tolower(Library_items_code$trim_ref)->Library_items_code$trim_ref
  
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  ded_code_Sub$concat_ded_code = sub(searchString,replacementString,ded_code_Sub$concat_ded_code)
  Library_items_code$concat_libcode_trim = sub(searchString,replacementString,Library_items_code$concat_libcode_trim)
  Library_items_code$concat_libcode_trim_num= sub(searchString,replacementString,Library_items_code$concat_libcode_trim_num)
  Library_items_code$trim_ref= sub(searchString,replacementString,Library_items_code$trim_ref)
  
  
  #Join
  Library_items_code %>%
    left_join(ded_code_Sub, by = c("concat_libcode_trim" = "concat_ded_code"))->Library_items_code_ded1
  
  
  
  
  Library_items_code_ded1 %>%
    left_join(ded_code_Sub, by = c("concat_libcode_trim_num" = "concat_ded_code"))->Library_items_code_ded2
  
  
  Library_items_code_ded2 %>%
    left_join(ded_code_Sub, by = c("trim_ref" = "concat_ded_code"))->Library_items_code_ded2
  
  
  ifelse((!is.na(Library_items_code_ded2$Codelist.OID.y)), Library_items_code_ded2$Codelist.OID.y,Library_items_code_ded2$Codelist.OID.x ) -> Library_items_code_ded2$Codelist.OID
  
  
  #remove temp items
  Library_items_code_ded2 %>% select(-code_trim,-code_trim_num,-trim_ref)->Library_items_code_ded2
  
  #Collapse
  #DED_Code_OID
  ifelse(!is.na(Library_items_code_ded2$Codelist.OID),Library_items_code_ded2$Codelist.OID <- Library_items_code_ded2$Codelist.OID,
         ifelse(!is.na(Library_items_code_ded2$Codelist.OID.x),Library_items_code_ded2$Codelist.OID <- Library_items_code_ded2$Codelist.OID.x,
                ifelse(!is.na(Library_items_code_ded2$Codelist.OID.y),Library_items_code_ded2$Codelist.OID <- Library_items_code_ded2$Codelist.OID.y,      ifelse(!is.na(Library_items_code_ded2$Codelist.OID.y.y),Library_items_code_ded2$Codelist.OID <- Library_items_code_ded2$Codelist.OID.y.y,
                                                                                                                                                                  ifelse(!is.na(Library_items_code_ded2$Codelist.OID),Library_items_code_ded2$Codelist.OID <- "Missing","Missing")))))-> Library_items_code_ded2$Codelist.OID
  
  #DED_Code
  ifelse(!is.na(Library_items_code_ded2$Code),Library_items_code_ded2$DED_Code <- Library_items_code_ded2$Code,
         ifelse(!is.na(Library_items_code_ded2$Code.x),Library_items_code_ded2$DED_Code <- Library_items_code_ded2$Code.x,
                ifelse(!is.na(Library_items_code_ded2$Code.y),Library_items_code_ded2$DED_Code <- Library_items_code_ded2$Code.y,
                       ifelse(!is.na(Library_items_code_ded2$Code),Library_items_code_ded2$DED_Code <- "Missing","Missing"))))-> Library_items_code_ded2$DED_Code
  
  
  #DED_Label
  ifelse(!is.na(Library_items_code_ded2$Long.Decode),Library_items_code_ded2$DED_Long_Code <- Library_items_code_ded2$Long.Decode,
         ifelse(!is.na(Library_items_code_ded2$Long.Decode.x),Library_items_code_ded2$DED_Long_Code <- Library_items_code_ded2$Long.Decode.x,
                ifelse(!is.na(Library_items_code_ded2$Long.Decode.y),Library_items_code_ded2$DED_Long_Code <- Library_items_code_ded2$Long.Decode.y,
                       ifelse(!is.na(Library_items_code_ded2$Codelist),Library_items_code_ded2$DED_Long_Code <- "Missing","Missing"))))-> Library_items_code_ded2$DED_Long_Code
  
  #####
  
  #End ODM Update2
  
  #subset
  Library_items_code_ded2 %>% select(Name,Item.Name,Label,Item_OID, Mandatory,Codelist,Choice.Code,Choice.Label,Codelist.OID,DED_Code,DED_Long_Code,Item.SAS.Field.Name )->Library_items_code_ded2 
  
  
  #get rid of duplicates
  
  Library_items_code_ded2[!duplicated(Library_items_code_ded2), ] -> Library_items_code_ded2
  
  #add form oids
  
  form_oids %>% select(-concat,-Item.Name) -> form_oids_cl 
  
  plyr::join(Library_items_code_ded2, form_oids_cl, by="Name", type="left", match="first")-> Library_items_code_ded2 
  
  
  #add mapped codelist items
  
  #get rid of items wihtout a codelists
  mapped_items %>% filter(Include.in.Extracts.Yes.No. == "Yes")->mapped_items
  
  #Filter- we only want those that have a codelist or a default value or a hard code value
  mapped_items %>% filter(!is.na(Codelist.Name)| !is.na(Default.Value) | !is.na(Logic.to.Hard.Code.Value.in.Dataset))->mapped_items_cl
  
  mapped_items_cl %>% select(Study.Form.Name,Lilly.DED.Form.OID,Item.Name.OID,Codelist.Name,Default.Value,Logic.to.Hard.Code.Value.in.Dataset, Value.to.be.Hard.Coded)->mapped_items_cl 
  
  
  #make each hard coded value its own row
  
  mapped_items_cl %>% 
    mutate(Value.to.be.Hard.Coded = strsplit(as.character(Value.to.be.Hard.Coded), '\n')) %>% 
    unnest(Value.to.be.Hard.Coded)->mapped_items_cl
  
  
  #set value
  
  ifelse(!is.na(mapped_items_cl$Value.to.be.Hard.Coded),mapped_items_cl$mapping <- mapped_items_cl$Value.to.be.Hard.Coded,
         ifelse(!is.na(mapped_items_cl$Default.Value),mapped_items_cl$mapping<- mapped_items_cl$Default.Value,ifelse(!is.na(mapped_items_cl$Logic.to.Hard.Code.Value.in.Dataset),mapped_items_cl$mapping<- mapped_items_cl$Logic.to.Hard.Code.Value.in.Dataset, "Missing")))-> mapped_items_cl$mapping
  
  #DED info
  ded_sub %>% select(DED.Form.OID,Item.OID,Question,Mandatory)->ded_cl
  ded_code_Sub %>% select(Codelist.OID,Long.Decode)->ded_code_Sub_cl
  
  
  
  #concat form oid and codelist oid 
  paste(ded_cl$DED.Form.OID ,ded_cl$Item.OID)->ded_cl$concat
  paste(mapped_items_cl$Lilly.DED.Form.OID ,mapped_items_cl$Item.Name.OID)->mapped_items_cl$concat
  
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  ded_cl$concat = sub(searchString,replacementString,ded_cl$concat)
  mapped_items_cl$concat = sub(searchString,replacementString,mapped_items_cl$concat)
  
  #join
  mapped_items_cl %>%
    left_join(ded_cl, by = c("concat" = "concat"))->mapped_items_cl
  
  #replace any spaces
  searchString <- ' '
  replacementString <- ''
  
  mapped_items_cl %>%
    left_join(ded_code_Sub_cl, by = c("Codelist.Name" = "Codelist.OID"))->mapped_items_cl
  
  #rename
  mapped_items_cl %>% select(-DED.Form.OID)->mapped_items_cl
  mapped_items_cl %>% 
    dplyr::rename(Name = Study.Form.Name)%>% 
    dplyr::rename(Item_OID = Item.Name.OID)%>% 
    dplyr::rename(Codelist.OID = Codelist.Name)%>% 
    dplyr::rename(DED_Code = mapping)%>% 
    dplyr::rename(DED.Form.OID = Lilly.DED.Form.OID)%>% 
    dplyr::rename(Label = Question)%>%
    dplyr::rename(DED_Long_Code = Long.Decode) ->mapped_items_cl
  
  
  #subset
  mapped_items_cl %>% select(Name,Label,Item_OID, Mandatory,Codelist.OID,DED_Code,DED_Long_Code,DED.Form.OID )->mapped_items_cl 
  
  #add missing coluns
  mapped_items_cl$Codelist <-mapped_items_cl$Codelist.OID 
  mapped_items_cl$Choice.Code <-mapped_items_cl$DED_Code 
  mapped_items_cl$Choice.Label <-mapped_items_cl$DED_Long_Code 
  mapped_items_cl$Item.Name <-mapped_items_cl$Item_OID 
  #combine to codelist
  
  #Remove duplicates
  mapped_items_cl  <- dplyr::distinct(mapped_items_cl,  .keep_all = TRUE)
  sas  <- dplyr::distinct(sas,  .keep_all = TRUE)
  
  
  #add SAS labels to mapped variables
  #Join
  mapped_items_cl %>%
    left_join(sas, by = c("Item_OID" = "Item.OID"))->mapped_items_cl
  
  combi_cl <- rbind(Library_items_code_ded2,mapped_items_cl)
  
  #update
  # remove duplicate rows
  combi_cl[!duplicated(combi_cl), ] -> combi_cl
  
  combi_cl%>% select(Name,Item.Name,Label,Item_OID, Item.SAS.Field.Name, Mandatory,Codelist,	Choice.Code,	Choice.Label,Codelist.OID,DED_Code,DED_Long_Code,DED.Form.OID)->combi_cl
  
  #concat items
  paste(combi_cl$Item_OID,combi_cl$DED.Form.OID)->combi_cl$concat 
  #replace any spaces
  combi_cl$concat= sub(searchString,replacementString,combi_cl$concat) 
  
  combi_cl %>%
    left_join(ded_sdtm_domain, by = c("concat" = "concat"))->combi_cl
  
  combi_cl %>% select(-concat) ->combi_cl
  
  #remove duplicate rows
  combi_cl[!duplicated(combi_cl), ] -> combi_cl
  
  #rename SAS field
  combi_cl%>%
    dplyr::rename(SAS.Field.Name = Item.SAS.Field.Name)->combi_cl
  #end update
  
  #linking table
  
  links%>% select(Name,Label...2,External.ID)->links2
  links2 %>% filter(!is.na(Label...2))->links2
  
  Library_items %>% select(Name, DED.Form.OID)->lt
  
  lt$Extract_Listing <-lt$DED.Form.OID
  
  lt %>% 
    dplyr::rename(Form_Name = Name)->lt
  
  
  #Join with form labels
  lt %>%
    left_join(links2, by = c("Form_Name" = "Name"))->lt
  
  
  lt %>% 
    dplyr::rename(FORM = Label...2)%>%
    dplyr::rename(FORMEID = External.ID) %>%
    dplyr::rename(DED.Dataset.name = Extract_Listing)->lt
  
  
  lt %>% select(FORM,FORMEID,DED.Dataset.name)->lt
  
  #remove duplicate rows
  lt[!duplicated(lt), ] -> lt
  
  #add active status
  
  #Active status CDB items
  
  #Join
  
  #items
  Library_items %>%
    left_join(ded_active, by = c("Item_OID" = "Item.OID"))->Library_items
  
  #items for codes
  combi_cl %>%
    left_join(ded_active, by = c("Item_OID" = "Item.OID"))->combi_cl
  
  #join without duplicates
  #remove duplicate rows
  ded_code_active[!duplicated(ded_code_active), ] -> ded_code_active
  combi_cl[!duplicated(combi_cl), ] -> combi_cl
  
  #Code lists	
  combi_cl %>%
    left_join(ded_code_active, by = c("Codelist.OID" = "Codelist.OID"))->combi_cl
  
  combi_cl[!duplicated(combi_cl), ] -> combi_cl
  
  
  #remove duplicate rows
  Library_items[!duplicated(Library_items), ] -> Library_items
  
  #remove duplicate rows
  combi_cl[!duplicated(combi_cl), ] -> combi_cl
  
  #ODM Update
  if(nrow(asses)>0){
    #create dummy columns for library items
    asses$Label <- asses$Question
    
    #ded codeslists by item oid
    ded %>% select(Item.SAS.Field.Name,Item.OID,Codelist.OID)->codeslist
    
    ded %>% select(Item.SAS.Field.Name,Item.OID)->sdtm_sq
    
    sdtm_sq%>%
      dplyr::rename(SAS.Field.Name = Item.SAS.Field.Name)->sdtm_sq
    
    
    codeslist%>%
      dplyr::rename(SAS.Field.Name = Item.SAS.Field.Name)->codeslist
    
    #Join with form labels
    asses %>%
      left_join(codeslist, by = c("Item_OID" = "Item.OID"))->asses_cl
    
    asses_cl %>%
      left_join(sdtm_sq, by = c("Item_OID" = "Item.OID"))->asses_cl
    
    #remove duplicate rows
    asses_cl[!duplicated(asses_cl), ] -> asses_cl
    
    #set codelist
    
    asses_cl$Codelist <- asses_cl$Codelist.OID
    
    ifelse(!is.na(asses_cl$SAS.Field.Name.x),asses_cl$SAS.Field.Name.x, 
           ifelse(!is.na(asses_cl$SAS.Field.Name.y),asses_cl$SAS.Field.Name.y ,"Missing"))-> asses_cl$SAS.Field.Name
    
    #select library items 
    asses_cl %>% select(Name,Item.Name,Label,Item_OID,SAS.Field.Name,Mandatory,Codelist,Question,DED.Form.OID,Item.Active.Status)->asses_cl
    
    #combine with library items
    rbind(Library_items,asses_cl)->Library_items
    
    
    #Assesment Codelists
    # MODIFIED: Updated column names from Selection.Name/Selection.Label
    #           to Choice.Name/Choice.Label per SDS specification change
    asses_lup %>% select(Item.External.ID,Choice.Name,Choice.Label)->asses_cd
    
    #fill externa id names if blank so we have name with each codelist item
    
    
    zoo::na.locf(asses_cd$Item.External.ID, na.rm = FALSE)->asses_cd$Item.External.ID
    
    #remove suffix _ADJ from assessments items names
    asses_cd$Item.External.ID<-gsub("_ADJ", "", asses_cd$Item.External.ID)
    
    asses_cl %>%
      left_join(asses_cd, by = c("Item.Name" = "Item.External.ID"))->asses_codes
    
    
    #filter blank codelists
    # MODIFIED: Updated filter column from Selection.Name to Choice.Name
    #           per SDS specification change
    asses_codes %>% filter(!is.na(Choice.Name))->asses_codes
    
    
    #rename columns to match combi_cl
    # MODIFIED: Renamed from Selection.Name -> Choice.Code (via Choice.Name)
    #           Selection.Label is now Choice.Label (column already named correctly,
    #           so rename line dropped to avoid self-rename error)
    asses_codes %>% 
      dplyr::rename(Choice.Code = Choice.Name)->asses_codes
    
    #prepare ded codelists for join
    ded_code_Sub$joins<-ded_code_Sub$Codelist.OID
    
    ded_code_Sub %>% select(joins,Codelist.OID,Code,Long.Decode)->ded_code_Sub_asses
    
    #replace underscores from choice codes and replace with spaces, assessments don't allow spaces for codes we want to capture both
    
    asses_codes$Choice.Code <- gsub("_", " ", asses_codes$Choice.Code)
    
    #concat
    ded_code_Sub_asses$joins<-paste(ded_code_Sub_asses$Codelist.OID,ded_code_Sub_asses$Code)
    asses_codes$concat<-paste(asses_codes$Codelist,asses_codes$Choice.Code)
    
    asses_codes$concat_cc<-paste(asses_codes$Codelist,asses_codes$Choice.Label)
    
    #replace any spaces
    searchString <- ' '
    replacementString <- ''
    ded_code_Sub_asses$joins = sub(searchString,replacementString,ded_code_Sub_asses$joins)
    asses_codes$concat = sub(searchString,replacementString,asses_codes$concat)
    
    asses_codes$concat_cc = sub(searchString,replacementString,asses_codes$concat_cc)
    
    trimws(asses_codes$concat)->asses_codes$concat
    
    trimws(asses_codes$concat_cc)->asses_codes$concat_cc
    
    #lower case
    tolower(asses_codes$concat)->asses_codes$concat
    tolower(asses_codes$concat_cc)->asses_codes$concat_cc
    tolower(ded_code_Sub_asses$joins)->ded_code_Sub_asses$joins
    
    
    #join
    asses_codes %>%
      left_join(ded_code_Sub_asses, by = c("concat" = "joins"))->asses_codes
    
    asses_codes %>%
      left_join(ded_code_Sub_asses, by = c("concat_cc" = "joins"))->asses_codes
    
    
    #label missing
    ifelse(!is.na(asses_codes$Codelist.OID.x),asses_codes$Codelist.OID.x,ifelse(!is.na(asses_codes$Codelist.OID.y),asses_codes$Codelist.OID.y,"Missing"))-> asses_codes$Codelist.OID
    
    
    ifelse(!is.na(asses_codes$Code.x),asses_codes$Code.x,ifelse(!is.na(asses_codes$Code.y),asses_codes$Code.y,"Missing"))-> asses_codes$Code 
    
    
    ifelse(!is.na(asses_codes$Long.Decode.x),asses_codes$Long.Decode.x,ifelse(!is.na(asses_codes$Long.Decode.y),asses_codes$Long.Decode.y,"Missing"))-> asses_codes$Long.Decode 
    
    asses_codes %>% select(-concat)->asses_codes
    
    #rename
    asses_codes %>% 
      dplyr::rename(DED_Code = Code) %>%
      dplyr::rename(DED_Long_Code = Long.Decode)->asses_codes
    
    #join codelist active status
    asses_codes %>%
      left_join(ded_code_active, by = c("Codelist.OID" = "Codelist.OID"))->asses_codes
    
    #order columns
    asses_codes %>% select(Name,	Item.Name,	Label,	Item_OID,	SAS.Field.Name,	Mandatory,	Codelist,	Choice.Code,	Choice.Label,	Codelist.OID,	DED_Code,	DED_Long_Code,	DED.Form.OID,	Item.Active.Status,	Codelist.Active.Status)->asses_codes
    
    asses_codes[!duplicated(asses_codes), ] -> asses_codes
    
    
    #add SDTM domain
    
    #concat items
    paste(asses_codes$Item_OID,asses_codes$DED.Form.OID)->asses_codes$concat 
    #replace any spaces
    asses_codes$concat= sub(searchString,replacementString,asses_codes$concat) 
    
    asses_codes %>%
      left_join(ded_sdtm_domain, by = c("concat" = "concat"))->asses_codes
    
    asses_codes %>% select(-concat) ->asses_codes
    
    #remove duplicate rows
    asses_codes[!duplicated(asses_codes), ] -> asses_codes
    
    
    combi_cl%>% select(Name,	Item.Name,	Label,	Item_OID,	SAS.Field.Name,	Mandatory,	Codelist,	Choice.Code,	Choice.Label,	Codelist.OID,	DED_Code,	DED_Long_Code,	DED.Form.OID,Item.Active.Status,	Codelist.Active.Status)->combi_cl
    
    combi_cl<-rbind(combi_cl,asses_codes)
  }
  
  #End ODM update
  
  Library_items <- Library_items %>% 
    mutate_if(is.character,
              stri_trans_general,
              id = "latin-ascii")
  
  combi_cl <- combi_cl %>% 
    mutate_if(is.character,
              stri_trans_general,
              id = "latin-ascii")
  
  lt <- lt %>% 
    mutate_if(is.character,
              stri_trans_general,
              id = "latin-ascii")
  
  
  CDB2 <- CDB2 %>% 
    mutate_if(is.character,
              stri_trans_general,
              id = "latin-ascii")
  
  
  
  #remove status forms from items list (we do this at the end so it is included in CDB mapping)
  dplyr::filter(Library_items, !grepl('STAT_.*', Item.Name))->Library_items
  
  #add Item.SAS.Field.Name to Items
  #Join
  Library_items %>%
    left_join(sas, by = c("Item_OID" = "Item.OID"))->Library_items
  
  Library_items %>% select(Name,Item.Name,Label,Item_OID,Item.SAS.Field.Name,Mandatory,Codelist,Question,DED.Form.OID,Item.Active.Status)->Library_items
  
  
  #remove duplicate rows
  Library_items[!duplicated(Library_items), ] -> Library_items
  
  
  
  return(list(Library_items,combi_cl,CDB2, lt))
}

#Adding a Usage Log
usagelogr::registerAppInUsageTool("MANTIS")



#### Define UI for application 

ui <- navbarPage(
  
  #Title of the tab when MANTIS is launched
  windowTitle = ("MANTIS"),
  
  #Title of the Homepage
  title = strong(span("Metrics Analytics Network Technological Integrated Systems (M.A.N.T.I.S)", style = "color: MediumSeaGreen; font-size: 30px; font-family: Papyrus")),
  
  header = tagList(
  tags$head(
    tags$style(HTML("
      .navbar-header {
        float: none;
        width: 100%;
        text-align: center;
      }
      .navbar-brand {
        float: none;
        display: inline-block;
      }
      .navbar-left, .navbar-nav {
        float: none !important;
      }
      .navbar-nav {
        display: block;
        width: 100%;
        padding: 10px 15px;
      }
      .navbar-nav > li {
        float: none;
        display: inline-block;
        margin-right: 10px;
      }
      .navbar-nav > li > a {
        background-color: #e6e6e6 !important;
        border-radius: 4px;
        padding: 8px 20px !important;
        color: #333 !important;
      }
      .navbar-nav > li.active > a {
        background-color: #d0d0d0 !important;
        font-weight: bold;
      }
      .navbar-nav > li > a:hover {
        background-color: #d5d5d5 !important;
      }
      .navbar-collapse {
        border-top: 1px solid #e7e7e7;
      }
    "))
  ),
  useShinydashboard(),
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert()
),
  
  ##Veeva tab
  tabPanel(title = "Veeva Tools",
           fluidPage(
             
             navbarPage("SDTM & CDB Report"),
             br(),
             br(),   
             sidebarLayout(
               sidebarPanel(
                 
                 textInput("studyid", "Enter Study Name", placeholder = "Type study name here (XXX-YYY-ZZZZ)"),
                 fileInput("uploadFile_ded", "DED ODM Report", accept = c(".xlsx")),
                 fileInput("uploadFile_sds", "Select Study SDS", accept = c(".xlsx")),
                 fileInput("uploadFile_map", "Select Items to Map"),
                 actionButton("Show_Report_Btn_Click","Run Report"),
                 downloadButton("download_all", "Download All" ),
                 
                 mainPanel(
                   tableOutput('contents')
                 )
               ),
               
               
               mainPanel(
                 
                 # Output: Tabset w/ plot, summary, and table ----
                 box(title = "Preview SDS DED Outputs",width = 20,collapsible = FALSE,solidHeader = TRUE,background = NULL,
                     tabsetPanel(type = "tabs",
                                 tabPanel("SDS to DED Items",dataTableOutput("Library_items")),
                                 tabPanel("SDS to DED Codelists",dataTableOutput("combi_cl")),
                                 tabPanel("CDB",dataTableOutput("CDB2"))))
               )
               
               
             )
             
           )), # END veeva tab panel
  
  #ODM parse Tab -- now runs in-app instead of embedding the external Posit Connect iframe
  tabPanel(title = "VDV ODM Parser",
           fluidPage(
             h3("Study Build Team"),
             sidebarLayout(
               sidebarPanel(
                 tags$b("Step 1. Import VDV Codelists Coversheet"),
                 tags$br(), 
                 shinyBS::bsButton("launch_modal", 
                                   "Upload here",
                                   icon("upload"), 
                                   size = "default"),
                 shinyjs::hidden(p(id = "upload_text1", "*Coversheet uploaded.")),
                 tags$p("",style = "margin-bottom: 15px;"),
                 tags$b("Step 2. Run ODM Parser"),
                 tags$br(), 
                 shinyBS::bsButton("runParser", 
                                   "Click here", 
                                   icon("running"),
                                   size = "default"),
                 shinyjs::hidden(p(id = "text1", "*Processing... Please wait.")),
                 tags$p("", style = "margin-bottom: 15px;"),  
                 tags$b("Step 3. Download parsed data from DED ODM xml reference file (all tabs)"),
                 tags$br(), 
                 downloadButton("downloadButton1", 
                                "Download here"),
                 tags$p("", style = "margin-bottom: 15px;"), 
                 tags$b("Step 4 (Optional). Select which data sheet you would like to view."),
                 selectInput("dropdown_sheet1", "", selected = "Cover_Page", choices = c("Cover_Page", "ODM_FORM_IG", "ODM_IG_Item", "Codelist_ODM", 
                                                                 "Codelist_SVA", "Codelist_Item_DT", "ODM_Item_Details")),
                 tags$b("Note: Wait a few minutes for processing during Step 2. 
                        This app is based on output from VDV Codelist Items Compare Tool Macro. 
                        App designed for use with Microsoft Edge browser.
                        Results automatically archived here: 
                        /lrlhps/data/study_build_team/custom-odm-parsing/
                        "),
                 tags$div(h5("Developed by Statistics, Data & Analytics (SDnA)"), 
                          style="text-align: center")
               ),
               mainPanel(
                 DT::dataTableOutput(outputId = "data")
               )
             )
           )
  )
  
) # end UI specifications

# Define server logic 
server <- function(input, output, session) {
  
  ##################################################################################################################################
  ## Veeva Tools (MANTIS) server logic ----
  ##################################################################################################################################
#welcome alert ---- 
  shinyalert("Welcome to M.A.N.T.I.S App", type = "info", timer = 3000, showConfirmButton = FALSE)
  
  observeEvent(input$Show_Report_Btn_Click, {
    #showModal(modalDialog("Loading Report"))
    show_modal_spinner(
      spin = "breeding-rhombus",
      color = "#74c365",
      text = "Loading..."
    )
    
    
    
    ########DED filess######
    
    SDS_DED_MATRIX<-SDS_DED_FUN(
      Lib_items = reactive({read_excel(input$uploadFile_sds$datapath, sheet = "Form Definitions", col_types = "text")}),
      asses = reactive({read_excel(input$uploadFile_sds$datapath, sheet = "Assessments", col_types = "text")}),
      Lib_codes = reactive({read_excel(input$uploadFile_sds$datapath, sheet = "Codelists", col_types = "text")}),
      mapped_items = reactive({read_excel(input$uploadFile_map$datapath, sheet = 2, col_types = "text")}),
      odm_forms = reactive({read_excel(input$uploadFile_ded$datapath, sheet = "ODM_FORM_IG", col_types = "text")}),
      odm_items = reactive({read_excel(input$uploadFile_ded$datapath, sheet = "ODM_IG_Item", col_types = "text")}),
      odm_code = reactive({read_excel(input$uploadFile_ded$datapath, sheet = "Codelist_ODM", col_types = "text")}),
      odm_sdtm = reactive({read_excel(input$uploadFile_ded$datapath, sheet = "ODM_Item_Details", col_types = "text")}),
      odm_code_item = reactive({read_excel(input$uploadFile_ded$datapath, sheet = "ODM_Item_DT", col_types = "text")})
    ) 
    
    ### Outputs####
    
    output$Library_items<- DT::renderDataTable({
      DT::datatable(SDS_DED_MATRIX[[1]], 
                    options = list(scrollX = TRUE,pageLength = 5, info = FALSE,
                                   lengthMenu = list(c(5, -1), c("5", "All")) ))
    })
    
    output$combi_cl<- DT::renderDataTable({
      DT::datatable(SDS_DED_MATRIX[[2]], 
                    options = list(scrollX = TRUE,pageLength = 5, info = FALSE,
                                   lengthMenu = list(c(5, -1), c("5", "All")) ))
    })
    
    
    output$CDB2<- DT::renderDataTable({
      DT::datatable(SDS_DED_MATRIX[[3]], 
                    options = list(scrollX = TRUE,pageLength = 5, info = FALSE,
                                   lengthMenu = list(c(5, -1), c("5", "All")) ))
    })
    
    #removeModal()
    remove_modal_spinner()
    
    #### Downloads and Exports####
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = 'DED_SDS_items')
    writeData(wb, sheet = "DED_SDS_items", x = SDS_DED_MATRIX[[1]])
    
    setHeaderFooter(wb,
                    sheet = 1,
                    header = c(NA, dQuote(input$studyid) , NA),
                    footer = c("&[Page] of &[Pages]","&[Date]", "&[Time]"))
    
    addWorksheet(wb, sheetName = 'DED_SDS_codelist')
    writeData(wb, sheet = 2, x = SDS_DED_MATRIX[[2]])
    
    setHeaderFooter(wb,
                    sheet = "DED_SDS_codelist",
                    header = c(NA, dQuote(input$studyid) , NA),
                    footer = c("&[Page] of &[Pages]","&[Date]", "&[Time]"))
    
    addWorksheet(wb, sheetName = 'CDB_Mappings')
    writeData(wb, sheet = 3,x = SDS_DED_MATRIX[[3]])
    
    setHeaderFooter(wb,
                    sheet = "CDB_Mappings",
                    header = c(NA, dQuote(input$studyid) , NA),
                    footer = c("&[Page] of &[Pages]","&[Date]", "&[Time]"))
    
    addWorksheet(wb, sheetName = "Linking_Table")
    writeData(wb, sheet = 4, x = SDS_DED_MATRIX[[4]])
    
    setHeaderFooter(wb,
                    sheet = "Linking_Table",
                    header = c(NA, dQuote(input$studyid) , NA),
                    footer = c("&[Page] of &[Pages]","&[Date]", "&[Time]"))
    
    output$download_all <- downloadHandler(
      
      filename = function() {
        paste(paste("SDS_DED_ALL",input$studyid, sep="_"),"xlsx", sep=".")
        
      },
      content = function(filename){
        #user download location
        saveWorkbook(wb, file = filename, overwrite = TRUE)
        
        #download location to database 
        DED_SDS_items<-SDS_DED_MATRIX[[1]]
        DED_SDS_codelist<-SDS_DED_MATRIX[[2]]
        CDB_Mappings<-SDS_DED_MATRIX[[3]]
        CDB_Mappings<-CDB_Mappings[,0:6]
        
        Lib_items = reactive({read_excel(input$uploadFile_sds$datapath, sheet = "Form Definitions")})
        asses = reactive({read_excel(input$uploadFile_sds$datapath, sheet = "Assessments")})
        Lib_codes = reactive({read_excel(input$uploadFile_sds$datapath, sheet = "Codelists")})
        mapped_items = reactive({read_excel(input$uploadFile_map$datapath, sheet = 2)})
        
        SDS_items <- data.frame(Lib_items())
        names(SDS_items) <- make.names(names(SDS_items)) 
        
        SDS_asses <- data.frame(asses())
        names(SDS_asses) <- make.names(names(SDS_asses)) 
        
        SDS_codes <- data.frame(Lib_codes())
        names(SDS_codes) <- make.names(names(SDS_codes)) 
        
        SDS_mapped_items <- data.frame(mapped_items())
        names(SDS_mapped_items) <- make.names(names(SDS_mapped_items)) 
        
        
        #download if Rules are available for study
        
        sheets_name <- reactive({
          if (!is.null(input$uploadFile_sds)) {
            return(excel_sheets(path = input$uploadFile_sds$datapath))
          } else {
            return(NULL)
          }
        })
        
        Rules <- reactive({if (!is.null(input$uploadFile_sds) &&
                               ("Rules" %in% sheets_name())) {
          return(read_excel(input$uploadFile_sds$datapath,
                            sheet = "Rules"))
        } else {
          return(NULL)
        }
        })
        
        Rules <- suppressWarnings(data.frame(Rules()))
        names(Rules) <- suppressWarnings(make.names(names(Rules)) )
        
        
        #download if Shchedule tree are available for study
        
        sheets_name_sched <- reactive({
          if (!is.null(input$uploadFile_sds)) {
            return(excel_sheets(path = input$uploadFile_sds$datapath))
          } else {
            return(NULL)
          }
        })
        
        schedule_tree <- reactive({if (!is.null(input$uploadFile_sds) &&
                                       ("Schedule - Tree" %in% sheets_name_sched())) {
          return(read_excel(input$uploadFile_sds$datapath,
                            sheet = "Schedule - Tree"))
        } else {
          return(NULL)
        }
        })
        
        schedule_tree <- suppressWarnings(data.frame(schedule_tree()))
        names(schedule_tree) <- suppressWarnings(make.names(names(schedule_tree)) )
        #end shedule tree
        
        
        # #add columns
        DED_SDS_items$Study<-input$studyid
        DED_SDS_items$Date_run<-Sys.Date()
        
        DED_SDS_codelist$Study<-input$studyid
        DED_SDS_codelist$Date_run<-Sys.Date()
        
        CDB_Mappings$Study<-input$studyid
        CDB_Mappings$Date_run<-Sys.Date()
        
        SDS_items$Study<-input$studyid
        SDS_items$Date_run<-Sys.Date()
        
        if(nrow(SDS_asses)>0){
          SDS_asses$Study<-input$studyid
          SDS_asses$Date_run<-Sys.Date()
        }
        
        SDS_codes$Study<-input$studyid
        SDS_codes$Date_run<-Sys.Date()
        
        SDS_mapped_items$Study<-input$studyid
        SDS_mapped_items$Date_run<-Sys.Date()
        
        if(nrow(Rules)>0){
          Rules$Study<-input$studyid
          Rules$Date_run<-Sys.Date()
        }
        
        if(nrow(schedule_tree)>0){
          schedule_tree$Study<-input$studyid
          schedule_tree$Date_run<-Sys.Date()
        }
        tryCatch({
          
          #Raw SDS
          write.xlsx(x=SDS_items, file =paste(paste("//lrlhps/data/MANTIS/Data/Veeva/utility/SDS/SDS_items",input$studyid,Sys.Date(), sep="_"),"xlsx", sep="."), col.names=TRUE, row.names=F, append=FALSE)
          
          if(nrow(SDS_asses)>0){
            write.xlsx(x=SDS_asses, file =paste(paste("//lrlhps/data/MANTIS/Data/Veeva/utility/SDS/SDS_asses",input$studyid,Sys.Date(), sep="_"),"xlsx", sep="."), col.names=TRUE, row.names=F, append=FALSE)
          }
          
          write.xlsx(x=SDS_codes, file =paste(paste("//lrlhps/data/MANTIS/Data/Veeva/utility/SDS/SDS_codes",input$studyid,Sys.Date(), sep="_"),"xlsx", sep="."), col.names=TRUE, row.names=F, append=FALSE)
          write.xlsx(x=SDS_mapped_items, file =paste(paste("//lrlhps/data/MANTIS/Data/Veeva/utility/SDS/SDS_mapped_items",input$studyid, Sys.Date(),sep="_"),"xlsx", sep="."), col.names=TRUE, row.names=F, append=FALSE)
          
          if(nrow(Rules)>0){
            write.xlsx(x=Rules, file =paste(paste("//lrlhps/data/MANTIS/Data/Veeva/utility/SDS/SDS_Rules",input$studyid,Sys.Date(), sep="_"),"xlsx", sep="."), col.names=TRUE, row.names=F, append=FALSE)
          }#end rules if
          
          if(nrow(schedule_tree)>0){
            write.xlsx(x=schedule_tree, file =paste(paste("//lrlhps/data/MANTIS/Data/Veeva/utility/SDS/SDS_schedule_tree",input$studyid,Sys.Date(), sep="_"),"xlsx", sep="."), col.names=TRUE, row.names=F, append=FALSE)
          }#end rules if
          
          
          #SDS_DED Output
          write.xlsx(x=DED_SDS_items, file = paste(paste("//lrlhps/data/MANTIS/Data/Veeva/utility/SDS/SDS_DED_Output/DED_SDS_items",input$studyid, Sys.Date(),sep="_"),"xlsx", sep="."),append=F,sep=",",row.names = F)
          write.xlsx(x=DED_SDS_codelist, file = paste(paste("//lrlhps/data/MANTIS/Data/Veeva/utility/SDS/SDS_DED_Output/DED_SDS_codelist",input$studyid, Sys.Date(),sep="_"),"xlsx", sep="."),append=F,sep=",",row.names = F)
          write.xlsx(x=CDB_Mappings, file = paste(paste("//lrlhps/data/MANTIS/Data/Veeva/utility/SDS/SDS_DED_Output/CDB_Mappings",input$studyid, Sys.Date(),sep="_"),"xlsx", sep="."),append=F,sep=",",row.names = F)
          
          #End Database Download
          
          write.table(DED_SDS_items, file = "//lrlhps/data/MANTIS/LRL_MANTIS/Veeva/Output/DED_SDS_items.csv",append=T,sep=",",row.names = F)
          write.table(DED_SDS_codelist, file = "//lrlhps/data/MANTIS/LRL_MANTIS/Veeva/Output/DED_SDS_codelist.csv",append=T,sep=",",row.names = F)
          write.table(CDB_Mappings, file = "//lrlhps/data/MANTIS/LRL_MANTIS/Veeva/Output/CDB_Mappings.csv",append=T,sep=",",row.names = F)
          
        }, warning = function(w) {
          message("Network write warning (non-fatal): ", conditionMessage(w))
        }, error = function(e) {
          message("Network write failed (user download unaffected): ", conditionMessage(e))
        })   
        
      }
    )
    
    
  })
  
  ##################################################################################################################################
  ## VDV ODM Parser server logic (merged in from the standalone app) ----
  ##################################################################################################################################
  
  shinyjs::disable("downloadButton1")
  
  # create df with imported coversheet
  coversheet_df <- reactiveVal(NULL)
  
  observeEvent(input$launch_modal, {
    datamods::import_modal(
      id = "myid",
      from = c("file"),
      title = "Import data for VDV ODM Parser"
    )
    coversheet_df(datamods::import_server("myid", return_class = "tbl_df"))
  })
  
  # show uploaded text ----
  observe({
    req( !is.null(coversheet_df()) )
    req( coversheet_df()$data(), coversheet_df()$name())
    shinyjs::show("upload_text1")
    })
  
  # wait for user to indicate run process ----
  list_of_datasets_react <- reactive({
    req(input$runParser)
    #validate/need message to ensure uploaded proper format reference file ----
    validate(
      need(input$launch_modal, 
           "Please ensure that VDV Codelists Coversheet is uploaded in Step 1.")
    )
    req(coversheet_df())
    validate(
      need( !is.null(coversheet_df()) & nrow(coversheet_df()$data()) > 1, 
           "Please ensure that VDV Codelists Coversheet is uploaded successfully in Step 1.")
    )
    validate(
      need( coversheet_df()$data() %>% colnames() %>% 
              str_detect("Value") %>% any(), 
            "Please ensure that the VDV Codelists Coversheet is coversheet is uploaded that has Measure and Value columns.")
    )
    
    # define odm ded import path ----
    odm_ded_name <- coversheet_df()$data() %>% 
      filter(Value == "DED_File_Name") %>% 
      select(Measure) %>%
      pull() %>% 
      str_c(".xlsx")                 # Append .xlsx
    
    odm_ded_path <- str_c("/lrlhps/data/study_build_team/odm_ded_files/", 
                          odm_ded_name)
    validate(
      need(file.exists(odm_ded_path) , 
           glue::glue("Ensure that the ODM DED xlsx file is here: {odm_ded_path}")
             )
    )
    
    # disable run button after first click
    shinyjs::disable("runParser")
    shinyjs::show("text1")
    
    # run parsing script on data  ---- 
    source("parser_Xlsx.R", local = TRUE)
    
    # reenable run button after run completion 
    shinyjs::enable("runParser")
    shinyjs::hide("text1")
    
    shinyjs::enable("downloadButton1")
    
    #return list of datasets generated via parser.R
    list_of_datasets

  })
  
  # Reactive for Study Name from uploaded input coversheet ----
  StudyName_react <- reactive({
      coversheet_df()$data() %>% 
      filter(Value == "Study_Name") %>% 
      select(Measure) %>%
      pull() 
    })
  
  # Output DT table ----
  output$data <- DT::renderDT({
    req(list_of_datasets_react())
    DT::datatable( list_of_datasets_react()[[input$dropdown_sheet1]] ) }, 
    server = FALSE )

    # specify download handler for output ----  
  output$downloadButton1 <- downloadHandler(
    filename = function(){
      paste0("ODM-DED-PROC-", StudyName_react(), "_", today(), ".xlsx")
    },
    content = function(con){
      openxlsx::write.xlsx(list_of_datasets_react(), file = con)
    }
  )
  
  #end server logic    
}

# Run the application 
shinyApp(ui = ui, server = server)

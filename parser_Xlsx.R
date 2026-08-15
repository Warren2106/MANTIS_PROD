#load libraries ----
library(readxl)
library(readxl)
library(openxlsx)

# Specify the path to the Excel file
#odm_ded_path <- "/lrlhps/data/study_build_team/odm_ded_files/DED_Attribute_Report_24Nov2024.xlsx"

# Read the tblForms sheet
tblForms <- read_excel(odm_ded_path, sheet = "tblForms")

# Select only the desired columns for ODM_FORM_IG
ODM_FORM_IG <- tblForms %>% select("Form OID" = `DED OID`, "Item Group OID" = `IG OID`)

# Read the tblItemGroups sheet
tblItemGroups <- read_excel(odm_ded_path, sheet = "tblItemGroups")

# Select only the desired columns for ODM_IG_Item
ODM_IG_Item <- tblItemGroups %>% select("Item Group OID" = `IG OID`, "Item OID" = `DED Item OID`, "Item Mandatory" = `Mandatory`,"Item population Method" = `Item Population Method`, `Default Value`)

# Read the tblCodelists sheet
tblCodelists <- read_excel(odm_ded_path, sheet = "tblCodelists")

# Select only the desired columns for Codelist_ODM
Codelist_ODM <- tblCodelists %>% select(`Codelist OID`, "Code Value" = `Code`, "Label" = `Long Decode`, `Rank`, "Short Code Value" = `Short Decode`, "Active Status" = `Codelist Active Status`)

# Select only the desired columns for Codelist_SVA
Codelist_SVA <- tblCodelists %>% select(`Codelist OID`, `Submission Value Attribute`)

# Read the tblItems sheet
tblItems <- read_excel(odm_ded_path, sheet = "tblItems")

# Select only the desired columns for ODM_Item_DT
ODM_Item_DT <- tblItems %>% select("Item OID" = `DED Item OID`, `Item DataType`, "Item Length" = `Length`, "SAS Field Name" = `Item SAS Field Name`, `Codelist OID`)

# Select only the desired columns for ODM_Item_DT
ODM_Item_Details <- tblItems %>% select("Item OID" = `DED Item OID`, `Default Value`, "Item population Method" = `Item Population Method`, "Origin" = `Item Origin`, `Question`, `Active Status`, `Check All Question`, `Check All SAS Name`)


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
















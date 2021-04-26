
#install.packages("tidyverse") 
#install.packages("httr") 
#install.packages("jsonlite")
#install.packages("RCurl2)
#install.packages("readxl")
#install.packages("openxlsx")
#install.packages("data.table")
#install.packages("tibble")
#install.packages("flextable")
#install.packages("formattable")
#Load packages

library(tidyverse) 
library(httr) 
library(jsonlite)
library(RCurl)
library(readxl)
library(openxlsx)
library(data.table)
library(tibble)
library(flextable)
library(rlang)
library(formattable)
library(flextable)
set_flextable_defaults(big.mark = ",", 
                       font.size = 7, 
                       theme_fun = theme_vanilla,
                       padding.bottom = 6, 
                       padding.top = 6,
                       padding.left = 6,
                       padding.right = 6,
                       background.color = "white")




##Function to get RTS data from API endpoint-----


is.not.null <- function(x) {!is.null(x)}

get_api_data<- function(api_endpoint,pagination_link_variable_name){
  RTS_data_all <- data.frame() 
  api_response<- GET(api_endpoint)  
  
  tryCatch(
    while (is.not.null(api_response[1])) {
      api_response_json <- content(api_response, as = "text", encoding= "UTF-8")
      api_RTS_data <- fromJSON(api_response_json, flatten = TRUE) %>% 
        data.frame()
      RTS_data_all<- bind_rows(RTS_data_all, api_RTS_data)
      
      next_url= api_RTS_data[ncol(api_RTS_data),pagination_link_variable_name]
      
      
      api_response <-GET(next_url)
      
    },error = function(e) if(is.null(api_response[1])){break})
  
  return(RTS_data_all)}




get_data<- function(api_endpoint){
  tryCatch(data_request <-GET(api_endpoint))
  data_response <- content(data_request, as = "text", encoding   = "UTF-8")
  final_data <- fromJSON(data_response, flatten = TRUE) %>% 
    data.frame()
  return(final_data)
}




##Function to Read in excel file 


read_excel_file<-function(file){
  if(file.exists(file)==FALSE){print('Path not found, please check again.')}
  else {
    excel_dataset <- read_excel(file)
    return(excel_dataset)}}



#Function to Rename dataset columns

rename__columns <- function(dataset,old_names,new_names){
  stopifnot(length(old_names) == length(new_names))
  setnames(dataset, old_names, new_names) }


#Function to getclean columns,create new variables merge datasets from different endpoints

clean_and_merge_datasets<-function(country_codesD,flow_type_codesD,region_codesD,SITC_codesD,RTS_dataD){
  #Renaming columns in country codes
  rename__columns(country_codesD,c("value.CountryId","value.CountryName","value.Area1a"),c("CountryId","CountryName","EU_or_NONEU"))
  
  #Renaming columns in flowtype codes
  rename__columns(flow_type_codesD,c("value.FlowTypeId","value.FlowTypeDescription"),c("FlowTypeId","FlowTypeDescription"))
  
  #Renaming columns in region code
  rename__columns(region_codesD,c("value.RegionId","value.RegionName","value.RegionGroupName","value.RegionCodeNumeric"),c("RegionId","RegionName","RegionGroupName","RegionCodeNumeric"))
  
  SITC_codesD<- read.xlsx("data/SITC codes 1 and 2.xlsx", sheet=1L)
  rename__columns(SITC_codesD,c("SITC1.description","SITC2.description"), c("SITC1 description","SITC2 description"))
  #Create year variable
  rename__columns(RTS_data,c("value.CountryId","value.GovRegionId","value.FlowTypeId","value.CommoditySitc2Id","value.MonthId","value.Value"),c("CountryId","RegionId","FlowTypeId","CommoditySitc2Id","MonthId","Value"))
  RTS_dataD[,"year"] <- substr(RTS_dataD[,"MonthId"], 1, 4)
  RTS_dataD[,"quarter"] <- substr(RTS_dataD[,"MonthId"], 5, 6)
  #Merge datasets
  RTS_data_all<-
    left_join(RTS_dataD, region_codesD,by ="RegionId")%>%
    left_join(., country_codesD, by="CountryId")%>%     
    left_join(., flow_type_codesD, by="FlowTypeId")%>%
    left_join(., SITC_codesD,by ="CommoditySitc2Id" )
  
  #Not sure about whether 'Stores & Provis.' should be EU or NON EU, 75%+ of export values for 2018 & 2019 were to the EU. Have assigned it to EU.
  RTS_data_all$EU_or_NONEU[RTS_data_all$CountryName=='Stores & Provis.'] <- "European Union"
  
  return(RTS_data_all)}


##RTS, country,region,flowtype,commodity SITC2 datasets API endpoints
endpoint= "https://api.uktradeinfo.com/RTS"
region_endpoint="https://api.uktradeinfo.com/Region"
flow_type_endpoint="https://api.uktradeinfo.com/FlowType"
commodity_SITC2_endpoint="https://api.uktradeinfo.com/Commodity"
country_endpoint= "https://api.uktradeinfo.com/Country" 

##GetRTS country,region,flowtype,commodity SITC2 datasets from API endpoints

RTS_data<-get_api_data(endpoint,"X.odata.nextLink")
region_codes<-get_data (region_endpoint)
flow_type_codes<-get_data (flow_type_endpoint) 
SITC2_codes<-get_data (commodity_SITC2_endpoint)
country_codes<-get_data (country_endpoint)  


#Clean and merge the different datasets from the API endpoints
RTS_data_clean<-clean_and_merge_datasets(country_codes,flow_type_codes,region_codes,SITC_codes,RTS_data)

saveRDS(RTS_data_clean, file = "RTS_data_clean", ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)
#RTS_data_clean_in<-readRDS("RTS_data_clean", refhook = NULL)

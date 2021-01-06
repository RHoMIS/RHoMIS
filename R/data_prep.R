#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#' RHoMIS Data Cleaning Functions
#'
#' A series of functions that can be used to clean RHoMIS data.
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#' read raw rhomis data
#'
#' A function for loading raw RHoMIS data.
#'
#' @param path The file-path where the RHoMIS raw data is located
#'
#' @return Data-frame of rhomis dataset.
#' @export
#'
#' @examples
#' df <- read_raw_data('C:/User/me/my_data.csv')
#'
read_raw_data <- function(path)
{
  read_csv(path, na = c("n/a","<NA>", "999", 'NA'), col_types = cols())
}
#-------------------------------------------------------------------------
#' Add Project ID
#'
#' Adds a Project ID to the RHoMIS data. The ID comes in the form AA_BBB_YYYY.
#' Where AA is the two letter ISO country-code (https://www.iban.com/country-codes)
#' Where BBB is a three letter code assigned to the project being processed
#' Where YYYY is the year the survey was conducted
#'
#' @param data_set The data set being processed
#' @param project_id The ID being assigned to the project (see function description)
#'
#' @return Returns the dataframe with a column for project ID
#' @export
#'
#' @examples
#' df <- add_project_id(df, "KE_TST_2016")
add_project_id <- function(data_set, project_id){

  YEAR<- gsub('.*_','', project_id)
  COUNTRY_CODE<- gsub('_.*','', project_id)
  PROJECT<- gsub(paste0('_',unique(YEAR)), '', project_id)
  PROJECT<- gsub(paste0(unique(COUNTRY_CODE),'_'), '', PROJECT)

  data_set$project_ID <- project_id
  data_set$COUNTRY_CODE <- COUNTRY_CODE
  data_set$YEAR <- YEAR
  data_set$PROJECT <- PROJECT

  return(data_set)

}
#-------------------------------------------------------------------------
#' Shorten Column Names
#'
#' A function to shorten the column names of RHoMIS data.
#' Many of the raw datasets in RHoMIS have column names in the format "section/group.xxx/repeat.1/crop_name".
#' This would be converted in the format "crop_name_1".
#'
#' @param data_set The dataset that needs columns shortened
#'
#' @return Returns the dataframe with the columns shortened
#' @export
#'
#' @examples
#' df <- shorten_column_names(dataset)
shorten_column_names <- function(data_set){
  core_cols <- colnames(data_set)

  core_cols_new<-core_cols
  b=array(0,20)
  for (i in 1:length(core_cols)) {
    a<-unlist(strsplit(core_cols[i], "[.]"))
    #test for repeats
    b[1]<-grepl('repeat.1.',core_cols[i], fixed=TRUE)
    b[2]<-grepl('repeat.2.',core_cols[i], fixed=TRUE)
    b[3]<-grepl('repeat.3.',core_cols[i], fixed=TRUE)
    b[4]<-grepl('repeat.4.',core_cols[i], fixed=TRUE)
    b[5]<-grepl('repeat.5.',core_cols[i], fixed=TRUE)
    b[6]<-grepl('repeat.6.',core_cols[i], fixed=TRUE)
    b[7]<-grepl('repeat.7.',core_cols[i], fixed=TRUE)
    b[8]<-grepl('repeat.8.',core_cols[i], fixed=TRUE)
    b[9]<-grepl('repeat.9.',core_cols[i], fixed=TRUE)
    b[10]<-grepl('repeat.10.',core_cols[i], fixed=TRUE)

    b[11]<-grepl('repeat.11.',core_cols[i], fixed=TRUE)
    b[12]<-grepl('repeat.12.',core_cols[i], fixed=TRUE)
    b[13]<-grepl('repeat.13.',core_cols[i], fixed=TRUE)
    b[14]<-grepl('repeat.14.',core_cols[i], fixed=TRUE)
    b[15]<-grepl('repeat.15.',core_cols[i], fixed=TRUE)
    b[16]<-grepl('repeat.16.',core_cols[i], fixed=TRUE)
    b[17]<-grepl('repeat.17.',core_cols[i], fixed=TRUE)
    b[18]<-grepl('repeat.18.',core_cols[i], fixed=TRUE)
    b[19]<-grepl('repeat.19.',core_cols[i], fixed=TRUE)
    b[20]<-grepl('repeat.20.',core_cols[i], fixed=TRUE)
    if (length(which((tolower(as.character(b))=='1')))>0) {
      core_cols_new[i]<-paste0(a[length(a)],'_',as.character(which(tolower(as.character(b))=='1')))
    } else {
      core_cols_new[i]<-paste0(a[length(a)])
    }
  }

  colnames(data_set) <- core_cols_new

  return(data_set)


}
#-------------------------------------------------------------------------
#' Which HDDS
#'
#' RHoMIS has used two indicators for HDDS. One made up of 10 food groups, and one made up of 14 food-groups
#'
#' @param data_set The dataset where we need to identify which hdds type used
#'
#' @return Returns '10' if 10 groups were used. Returns '14' if 14 groups were used.
#'
#' @examples
#' hdds_type <- which_hdds_type_used(data_set)
which_hdds_type_used <- function(data_set){

  Ten_groups_true_false<-0
  Fourteen_groups_true_false<-0

  Ten_groups<-c("GrainsRootsTubers", "Legumes", "Nuts_Seeds", "Veg_Leafy", "VitA_Veg_Fruit", "Vegetables", "Fruits", "Meat", "Eggs", "Milk_Dairy")
  Fourteen_Groups<-c("grains", "roots_tubers", "pulses", "nuts_seeds", "milk", "organ_meat", "meat_poultry", "fish_seafood", "eggs", "green_veg", "vitA_veg", "vitA_fruits", "other_veg", "other_fruits")


  if (all(sapply(Ten_groups, function(x) any(grepl(x,colnames(dat_all))))))
  {
    return(10)
  }

  if (all(sapply(Fourteen_Groups, function(x) any(grepl(x,colnames(dat_all))))))
  {
    return(14)
  }

}
#-------------------------------------------------------------------------
#' Title
#'
#' @param data_set
#'
#' @return
#' @export
#'
#' @examples
age_loop_calculation <- function(data_set){

}






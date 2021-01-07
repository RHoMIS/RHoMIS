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
#' @export
#'
#'
#' @examples
#' hdds_type <- which_hdds_type_used(data_set)
which_hdds_type_used <- function(data_set){

  Ten_groups_true_false<-0
  Fourteen_groups_true_false<-0

  Ten_groups<-c("GrainsRootsTubers", "Legumes", "Nuts_Seeds", "Veg_Leafy", "VitA_Veg_Fruit", "Vegetables", "Fruits", "Meat", "Eggs", "Milk_Dairy")
  Fourteen_Groups<-c("grains", "roots_tubers", "pulses", "nuts_seeds", "milk", "organ_meat", "meat_poultry", "fish_seafood", "eggs", "green_veg", "vitA_veg", "vitA_fruits", "other_veg", "other_fruits")


  if (all(sapply(Ten_groups, function(x) any(grepl(x,colnames(data_set))))))
  {
    return(10)
  }

  if (all(sapply(Fourteen_Groups, function(x) any(grepl(x,colnames(data_set))))))
  {
    return(14)
  }

}
#-------------------------------------------------------------------------
#' Age loop conversion
#'
#' @param data_set the dataset that you want to convert
#'
#' @return Returns a data dataframe with household members placed into categories e.g. "children aged 4 to 10"
#' @export
#'
#' @examples
#'
#' df <- age_loop_calculation(data_set)
age_loop_calculation <- function(data_set){
  startwith_person<-grep("^person|^head_person", colnames(data_set))
  end_with_num<-grep("[1-9$]", colnames(data_set))
  person_rep<- data_set[startwith_person[startwith_person%in%end_with_num]]

  loop_number<-as.numeric(unique(gsub('.*_', '', colnames(person_rep))))


  data_set$children_under_4<-rep(0, nrow(data_set))
  data_set$children_4to10<-rep(0, nrow(data_set))
  data_set$males11to24<-rep(0, nrow(data_set))
  data_set$females11to24<-rep(0, nrow(data_set))
  data_set$males25to50<-rep(0, nrow(data_set))
  data_set$females25to50<-rep(0, nrow(data_set))
  data_set$malesover50<-rep(0, nrow(data_set))
  data_set$femalesover50<-rep(0, nrow(data_set))

  rowSums(!is.na(data.frame(data_set$age_femalehead,data_set$age_malehead)))

  person_age<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(data_set)))
  person_sex<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(data_set)))
  head_age<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(data_set)))
  head_sex<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(data_set)))

  for (i in loop_number)
  {
    age_temp<- as.numeric(as.character(person_rep[,  colnames(person_rep)==paste0("person_age_",i)]))
    sex_temp<-as.character(person_rep[,  colnames(person_rep)==paste0("person_gender_",i)])

    person_age[,i]<-age_temp
    person_sex[,i]<-sex_temp

    for (j in 1:nrow(data_set))
    {
      if (!is.na(age_temp[j]) && !is.na(sex_temp[j]))
      {
        if (age_temp[j]>0 && age_temp[j] <=4) {data_set$children_under_4[j]<- data_set$children_under_4[j]+1}
        if (age_temp[j]>4 && age_temp[j] <=10) {data_set$children_4to10[j]<-data_set$children_4to10[j]+1}
        if (age_temp[j]>10 && age_temp[j] <=24 && sex_temp[j]=="M") {data_set$males11to24[j]<-data_set$males11to24[j]+1}
        if (age_temp[j]>10 && age_temp[j] <=24 && sex_temp[j]=="F") {data_set$females11to24[j]<-data_set$females11to24[j]+1}
        if (age_temp[j]>24 && age_temp[j] <=50 && sex_temp[j]=="M") {data_set$males25to50[j]<-data_set$males25to50[j]+1}
        if (age_temp[j]>24 && age_temp[j] <=50 && sex_temp[j]=="F") {data_set$females25to50[j]<-data_set$females25to50[j]+1}
        if (age_temp[j]>50 &&  sex_temp[j]=="M") {data_set$malesover50[j]<-data_set$malesover50[j]+1}
        if (age_temp[j]>50 && sex_temp[j]=="F") {data_set$femalesover50[j]<-data_set$femalesover50[j]+1}

      }
    }
  }
  return(data_set)
}

#-------------------------------------------------------------------------

#' Number of Crop Loops
#'
#'A function used to find the number of crop loops in the survey
#'
#' @param data_set The dataset in question
#'
#' @return Returns a single number
#' @export
#'
#' @examples
#' no_crop <- number_of_crop_loops(dataset
#' )
number_of_crop_loops <- function(data_set){
  number_of_crop_loops<-length(grep("crop_sold_income_", colnames(dat_all)))
  return(number_of_crop_loops)
}

#-------------------------------------------------------------------------
#' Looking at all the unique values which need to be converted
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
unique_values_for_survey <- function(dataset, columns){
  subset <- dataset[,columns]
  all_values <- gather(subset)
  all_values <- all_values$value
  unique_values <- unique(all_values)

  return(unique_values)
}

find_unique_crops <-function(dataset){
  crop_columns <- c("crop_name_1","crop_name_2","crop_name_3","crop_name_4","crop_name_5","crop_name_6","crop_name_7","crop_name_8","crops_other1","crops_other2","crops_other3")
  crop_name_columns_in_data_set <- crop_columns[crop_columns %in% colnames(data_set)]
  return(unique_values_for_survey(dataset,crop_name_columns_in_data_set))
}

find_unique_livestock <-function(dataset){
  livestock_columns <- c("livestock_name_1","livestock_name_2","livestock_name_3","livestock_name_4","livestock_name_5","livestock_other1","livestock_other2", "livestock_other3")
  livestock_columns_in_data_set <- livestock_columns[livestock_columns %in% colnames(data_set)]
  return(unique_values_for_survey(dataset,crop_name_columns_in_data_set))
}

#-------------------------------------------------------------------------

#' Clean Gender info
#'
#' @param data_set
#'
#' @return
#' @export
#'
#' @examples
clean_gender_ownership_columns <- function(data_set){
  data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) as.character(tolower(x))))
  data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) as.character(gsub('female_child', 'female_youth_or_child', x))))
  data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) as.character(gsub('male_child', 'male_youth_or_child', x))))
  #data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) as.character(x)))


  replace_gendered_values<- function(value){

    if (is.na(value))
    {
      if (value=='male'&!is.na(value)) {
        value<-'male_adult'
      }
      if (value=='female'&!is.na(value)) {
        value<-'female_adult'
      }
      if (value=='male female'&!is.na(value)) {
        value<-'male_adult female_adult'
      }
      if (value=='female_youth'&!is.na(value)) {
        value<-'female_youth_or_child'
      }
      if (value=='male_youth'&!is.na(value)) {
        value<-'male_youth_or_child'
      }
      if (value=='child'&!is.na(value)) {
        value<-'male_youth_or_child'
      }
      if (value=='c(\"male\", \"female\", \"child\")'&!is.na(value)) {
        value<-'male_adult female_adult male_youth_or_child'
      }
      if (value=='c(\"female\", \"child\")'&!is.na(value)) {
        value<-'female_adult male_youth_or_child'
      }
      if (value=='c(\"male\", \"female\", \"youth\")'&!is.na(value)) {
        value<-'male_adult female_adult male_youth_or_child'
      }
      if (value=='c(\"female\", \"youth\")'&!is.na(value)) {
        value<-'male_adult male_youth_or_child'
      }
      if (value=='c(\"male\", \"youth\")'&!is.na(value)) {
        value<-'male_adult male_youth_or_child'
      }
      if (value=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(value)) {
        value<-'male_adult female_adult male_youth_or_child'
      }
      if (value=='c(\"male\", \"female\")'&!is.na(value)) {
        value<-'male_adult female_adult'
      }
      if (value=='null)'&!is.na(value)) {
        value<-'NA'
      }
    }
    return(value)
  }
  check_male_youth_or_male_youth_or_child<- function(value){

    if (grepl('male_youth',value)&!grepl('male_youth_or_child',value)) {
      value<-gsub(' male_youth',' male_youth_or_child',value)
    }
    if (grepl('female_youth',value)&!grepl('female_youth_or_child',value)) {
      value<-gsub('female_youth','female_youth_or_child',value)
    }
    return(value)
  }
  remove_string_duplicates<- function (seperation, string){
    if (!is.na(string))
    {
      temp_list<-strsplit(string, seperation)
      if (length(temp_list)>1)
      {
        temp_list<-unique(temp_list)
        string_to_return<-paste0(temp_list, collapse = " ")
        return(string_to_return)
      }else{return(string)}
    }else{return(string)}
  }


  for (i in grep('control|who|ownership', colnames(data_set)))
  {
    data_set[,i]<-as.character(data_set[,i])

    if (length(data_set[,i])>0)
    {
      data_set[,i]<-unlist(lapply(data_set[,i], function(x)trimws(x)))

      data_set[,i]<-unlist(data_set[,i], function (x) lapply(replace_gendered_values(x)))




      #first female then male!!!
      data_set[,i]<-gsub('woman_single','female_adult',data_set[,i])
      data_set[,i]<-gsub('man_single','male_adult',data_set[,i])
      data_set[,i]<-gsub(' child','male_youth_or_child',data_set[,i])
      data_set[,i]<-gsub('other_family_female','female_adult',data_set[,i])
      data_set[,i]<-gsub('other_family_male','male_adult',data_set[,i])
      data_set[,i]<-gsub('female_youth ','female_youth_or_child ',data_set[,i])
      data_set[,i]<-gsub('male_youth ','male_youth_or_child ',data_set[,i])
      data_set[,i]<-gsub('female_child ','female_youth_or_child ',data_set[,i])
      data_set[,i]<-gsub(' female_child',' female_youth_or_child',data_set[,i])
      data_set[,i]<-gsub('male_child ','male_youth_or_child ',data_set[,i])
      data_set[,i]<-gsub(' male_child',' male_youth_or_child',data_set[,i])

      data_set[,i]<-gsub('female ','female_adult ',data_set[,i])
      data_set[,i]<-gsub('male ','male_adult ',data_set[,i])
      data_set[,i]<-gsub('female_head ','female_adult ',data_set[,i])
      data_set[,i]<-gsub('male_head ','male_adult ',data_set[,i])


      data_set[,i]<-gsub('female_head','female_adult',data_set[,i])
      data_set[,i]<-gsub('male_head','male_adult',data_set[,i])

      #check for male_youth; if also male_youth_or_child is true do not change

      data_set[,i]<-unlist(data_set[,i], function (x) lapply(check_male_youth_or_male_youth_or_child(x)))
      data_set[,i]<-unlist(data_set[,i], function (x) lapply(remove_string_duplicates(" ",x)))




      index<-which(data_set[,i]=='joint' && !is.na(data_set[,i]))
      data_set[index,i]<-'male_adult female_adult'
    }



  }


  data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) gsub('youth_or_child', 'youth', x)))
  data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) gsub('female_head', 'female_adult', x)))
  data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) gsub('male_head', 'male_adult', x)))

  #data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) gsub('male_head', 'male_adult', x)))
  data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) gsub("(adult)(female)", "\\1 \\2", x)))
  data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) gsub("(adult)(male)", "\\1 \\2", x)))
  data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) gsub("(youth)(female)", "\\1 \\2", x)))
  data_set[,grep('control|who|ownership', colnames(data_set))]<-data.frame(lapply(data_set[,grep('control|who|ownership', colnames(data_set))], function(x) gsub("(youth)(male)", "\\1 \\2", x)))


}







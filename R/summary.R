
#' FAERS description
#'
#' @param tabular_faers_data a tibble corresponding to the unified FAERS tabular format. Output of function \link{unify_tabular_ascii} 
#'
#' @return A list with a findings summary
#' @export
summary_faersdata <- function(tabular_faers_data){
  
  
  tabular_faers_data_summary <- list()
  
  #ADR for each year summary
  is.date <- function(x) inherits(x, 'Date')
  for (date_var in names(tabular_faers_data)){
    
    if(is.date(tabular_faers_data[[date_var]])){
      tabular_faers_data_summary[[date_var]] <- 
        tabular_faers_data %>% 
        mutate_at(date_var, ~ format.Date(., "%Y")) %>% 
        group_by_at(vars(date_var)) %>% 
        summarize(n_ADR = n(),
                  n_safetyreport = length(unique(primaryid))) %>% 
        ungroup()
    }
  }
  
  #by reaction
  tabular_faers_data_summary$reaction <- 
    tabular_faers_data %>%
    mutate_at("pt", ~stringr::str_squish(.)) %>% 
    mutate(age_YR = case_when(age_cod == "YR" ~ as.numeric(age),
                           age_cod == "MON" ~ as.numeric(age)/30,
                           age_cod == "DY" ~ as.numeric(age)*365,
                           TRUE ~ as.numeric(age))) %>% 
    group_by(pt) %>% 
    summarize(n_ADR = n(),
              age_mean = round(mean(as.numeric(age_YR), na.rm = TRUE)),
              weight_mean = round(mean(as.numeric(wt), na.rm = TRUE))) %>% 
    ungroup() %>% 
    arrange(desc(n_ADR)) 

  #by drugname
  drugname_v <- tibble()
  for (drug_element in tabular_faers_data$patient_drug){
    
    drug <- drug_element[,c("drugname", "role_cod")] %>%
      tidyr::separate_longer_delim(drugname, delim = "\\") %>% 
      mutate_at("drugname", ~stringr::str_squish(.)) 
    
    drugname_v <- bind_rows(drugname_v, drug)
  }
  tabular_faers_data_summary$drug_name_ps <- drugname_v %>% 
    group_by(drugname, role_cod) %>% 
    summarize( n = n(), .groups = "keep") %>% 
    ungroup() %>% 
    pivot_wider(names_from = role_cod, values_from = n, names_prefix = "n_") %>% 
    arrange(desc(.[,2]))
  
  
  #by indication
  message("summarizing drugname")
  indication_v <- tibble()
  for (drug_element in tabular_faers_data$patient_drug){
    
    indi <- drug_element[,c("indi_pt", "role_cod")] %>%
      tidyr::separate_longer_delim(indi_pt, delim = "\\") %>% 
      mutate_at("indi_pt", ~stringr::str_squish(.)) 
    
    indication_v <- bind_rows(indication_v, indi)
  }
  tabular_faers_data_summary$drugname <- indication_v %>% 
    group_by(indi_pt, role_cod) %>% 
    summarize( n = n(), .groups = "keep") %>% 
    ungroup() %>% 
    pivot_wider(names_from = role_cod, values_from = n, names_prefix = "n_") %>% 
    arrange(desc(.[,2]))
  
  
  #by indication
  message("summarizing drug name")
  indication_v <- tibble()
  for (drug_element in tabular_faers_data$patient_drug){
    
    indi <- drug_element[,c("indi_pt", "role_cod")] %>%
      tidyr::separate_longer_delim(indi_pt, delim = "\\") %>% 
      mutate_at("indi_pt", ~stringr::str_squish(.)) 
    
    indication_v <- bind_rows(indication_v, indi)
  }
  tabular_faers_data_summary$indication <- indication_v %>% 
    group_by(indi_pt, role_cod) %>% 
    summarize( n = n(), .groups = "keep") %>% 
    ungroup() %>% 
    pivot_wider(names_from = role_cod, values_from = n, names_prefix = "n_") %>% 
    arrange(desc(.[,2]))
  
  
  #by active ingredient
  message("summarizing active ingredient")
  prod_ai_v <- tibble()
  for (drug_element in tabular_faers_data$patient_drug){
    
    ai <- drug_element[,c("prod_ai", "role_cod")] %>%
      tidyr::separate_longer_delim(prod_ai, delim = "\\") %>% 
      mutate_at("prod_ai", ~stringr::str_squish(.)) 
    
    prod_ai_v <- bind_rows(prod_ai_v, ai)
  }
  tabular_faers_data_summary$prod_ai <- prod_ai_v %>% 
    group_by(prod_ai, role_cod) %>% 
    summarize( n = n(), .groups = "keep") %>% 
    ungroup() %>% 
    pivot_wider(names_from = role_cod, values_from = n, names_prefix = "n_") %>% 
    arrange(desc(.[,2]))       
  
  
  #by safetyreportid
  tabular_faers_data_summary$safetyreporid <- 
    tabular_faers_data %>% 
    mutate(age_YR = case_when(age_cod == "YR" ~ as.numeric(age),
                           age_cod == "MON" ~ as.numeric(age)/30,
                           age_cod == "DY" ~ as.numeric(age)*365,
                           TRUE ~ as.numeric(age))) %>% 
    group_by(primaryid) %>% 
    summarize(mfr_sndr = unique(mfr_sndr),
              age_YR = as.numeric(unique(age_YR)),
              sex = unique(sex),
              weight_kg = as.numeric(unique(wt)),
              reporter_country = unique(reporter_country),
              occr_country = unique(occr_country)) %>% 
    ungroup() %>% 
    select(-primaryid) %>% 
    tableone::CreateTableOne(data = ., includeNA = FALSE )
  
  return(tabular_faers_data_summary)
  
}

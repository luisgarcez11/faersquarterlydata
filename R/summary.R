
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
        group_by_at(all_of(vars(date_var))) %>% 
        summarize(n_ADR = n(),
                  n_safetyreport = length(unique(primaryid))) %>% 
        ungroup()
    }
  }
  
  #by reaction
  tabular_faers_data_summary$reaction <- 
    tabular_faers_data %>%
    mutate(age = case_when(age_cod == "YR" ~ as.numeric(age),
                           age_cod == "MON" ~ as.numeric(age)/30,
                           age_cod == "DY" ~ as.numeric(age)*365,
                           TRUE ~ as.numeric(age))) %>% 
    group_by(pt) %>% 
    summarize(n_ADR = n(),
              age_mean = round(mean(as.numeric(age), na.rm = TRUE)),
              weight_mean = round(mean(as.numeric(wt), na.rm = TRUE))) %>% 
    ungroup() %>% 
    arrange(desc(n_ADR)) 
  
  
  #by safetyreportid
  tabular_faers_data_summary$safetyreporid <- 
    tabular_faers_data %>% 
    mutate(age = case_when(age_cod == "YR" ~ as.numeric(age),
                           age_cod == "MON" ~ as.numeric(age)/30,
                           age_cod == "DY" ~ as.numeric(age)*365,
                           TRUE ~ as.numeric(age))) %>% 
    group_by(primaryid) %>% 
    summarize(i_f_code = unique(i_f_code),
              rept_cod = unique(rept_cod),
              mfr_sndr = unique(mfr_sndr),
              age = as.numeric(unique(age)),
              sex = unique(sex),
              e_sub = unique(e_sub),
              wt = as.numeric(unique(wt)),
              reporter_country = unique(reporter_country),
              occr_country = unique(occr_country),
              indi_pt_all = unique(indi_pt_all),
              indi_pt_ps = unique(unique(indi_pt_ps)),
              drugname_all = unique(drugname_all),
              drugname_ps = unique(drugname_ps)) %>% 
    ungroup() %>% 
    select(-primaryid) %>% 
    tableone::CreateTableOne(data = ., includeNA = FALSE )
  
  return(tabular_faers_data_summary)
  
}

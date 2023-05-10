#' Convert a date string into a date format
#'
#' @param date_string A string vector with multiple formats (8, 6 or 4 digits)
#'
#' @return A converted Date
#' @export
#' @examples arrange_date("2020")
#' arrange_date("202006")
#' arrange_date("20200601")
arrange_date <- function(date_string) {
  if (any(stringr::str_detect(
    string = date_string,
    pattern = "[:alpha:]"
  ), na.rm = TRUE)) {
    stop("string contains letters")
  }

  date_string_modif <- dplyr::case_when(
    (nchar(date_string) == 4) ~ paste0(date_string, "0630"),
    (nchar(date_string) == 6) ~ paste0(date_string, "15"),
    (nchar(date_string) == 8) ~ as.character(date_string),
    TRUE ~ as.character(date_string)
  )

  date_string_modif <- as.Date(date_string_modif, format = "%Y%m%d")
  return(unname(date_string_modif))
}

#' Read FAERS ascii files
#'
#' @param ascii_dir directory path where ascii files are
#' @param cache_path (optional) a string. Must have a ".Rdata" extension to save the read tabular formats in each loop.
#' @param drug_indication_pattern (optional) a string. 
#' @param drug_pattern (optional) a string. 
#' @param primary_suspect (optional) a string. 
#'
#' @return A list with binded tibbles retrieved from files.
#' @export
retrieve_faersascii <- function(ascii_dir, cache_path = NULL, drug_indication_pattern = NULL, drug_pattern = NULL,
                                 primary_suspect = TRUE) {
  if (!dir.exists(ascii_dir)) {
    stop("directory does not exist")
  }

  ascii_files <- list.files(ascii_dir, full.names = TRUE, pattern = ".txt")
  ascii_drug_files <- ascii_files[which(stringr::str_detect(ascii_files, pattern = "drug|DRUG|indi|INDI"))]

  # gather drug files
  drug_info <- tibble::tibble()
  indi_info <- tibble::tibble()
  drug_indi_info <- tibble::tibble()

  # iterating over drug and indication files to filter the chosen ADRs
  for (ascii_drug_file in ascii_drug_files) {
    message(paste(
      "Retrieving drug/indication information: binding", ascii_drug_file,
      which(ascii_drug_file == ascii_drug_files), "out of", length(ascii_drug_files)
    ))


    if (stringr::str_detect(ascii_drug_file, "drug|DRUG")) {
      read_file <- data.table::fread(ascii_drug_file, sep = "$", quote = "", fill = FALSE) %>%
        {
          if ("nda_num" %in% names(.)) {
            .$nda_num <- as.double(.$nda_num)
            .
          } else {
            .
          }
        } %>%
        {
          if ("dose_amt" %in% names(.)) {
            .$dose_amt <- as.double(.$dose_amt)
            .
          } else {
            .
          }
        } %>%
        mutate_at(vars(ends_with("_dt")), list(~ arrange_date(.)))

      drug_info <- bind_rows(drug_info, read_file %>%
        {
          if (primary_suspect) {
            filter(., role_cod == "PS")
          } else {
            .
          }
        } %>%
        {
          if (!is.null(drug_pattern)) {
            filter(., stringr::str_detect(string = drugname, pattern = drug_pattern))
          } else {
            .
          }
        })
    }

    if (stringr::str_detect(ascii_drug_file, "indi|INDI")) {
      read_file <- data.table::fread(ascii_drug_file, sep = "$", quote = "", fill = FALSE) %>%
        mutate_at(vars(ends_with("_dt")), list(~ arrange_date(.)))

      indi_info <- bind_rows(indi_info, read_file %>%
        {
          if (!is.null(drug_indication_pattern)) {
            filter(., stringr::str_detect(string = indi_pt, pattern = drug_indication_pattern))
          } else {
            .
          }
        })
    }
  }

  if (!is.null(drug_indication_pattern) & !is.null(drug_pattern)) {
    drug_indi_info <- drug_info %>%
      inner_join(indi_info %>% group_by(primaryid, caseid, indi_drug_seq) %>%
        summarise(indi_pt = paste(sort(unique(indi_pt)), collapse = "/")) %>%
        ungroup(), by = c("primaryid", "caseid", "drug_seq" = "indi_drug_seq"))
  }

  if (!is.null(drug_indication_pattern) & is.null(drug_pattern)) {
    if (primary_suspect == TRUE) {
      drug_indi_info <- drug_info %>%
        inner_join(indi_info %>% group_by(primaryid, caseid, indi_drug_seq) %>%
          summarise(indi_pt = paste(sort(unique(indi_pt)), collapse = "/")) %>%
          ungroup(), by = c("primaryid", "caseid", "drug_seq" = "indi_drug_seq"))
    } else {
      drug_indi_info <- indi_info
    }
  }


  if (is.null(drug_indication_pattern) & !is.null(drug_pattern)) {
    drug_indi_info <- drug_info
  }

  if (is.null(drug_indication_pattern) & is.null(drug_pattern)) {
    drug_indi_info <- drug_info %>%
      full_join(indi_info %>% group_by(primaryid, caseid, indi_drug_seq) %>%
        summarise(indi_pt = paste(sort(unique(indi_pt)), collapse = "/")) %>%
        ungroup(), by = c("primaryid", "caseid", "drug_seq" = "indi_drug_seq"))
  }



  drug_info <- drug_info %>% filter(primaryid %in% drug_indi_info$primaryid)
  indi_info <- indi_info %>% filter(primaryid %in% drug_indi_info$primaryid)



  # cache objects to be filled
  cache <- list()
  cache_files <- c()

  # files to be filled
  demo_info <- tibble::tibble()
  outc_info <- tibble::tibble()
  reac_info <- tibble::tibble()
  rpsr_info <- tibble::tibble()
  ther_info <- tibble::tibble()

  for (ascii_file in ascii_files) {
    message(paste("binding", ascii_file, which(ascii_file == ascii_files), "out of", length(ascii_files)))



    if (stringr::str_detect(ascii_file, "demo|DEMO")) {
      demo_info <- demo_info %>% bind_rows(data.table::fread(ascii_file, sep = "$", quote = "", fill = FALSE) %>%
        mutate_at(vars(ends_with("_dt")), list(~ arrange_date(.))) %>%
        {
          if ("nda_num" %in% names(.)) {
            .$nda_num <- as.integer(.$nda_num)
            .
          } else {
            .
          }
        } %>%
        mutate_at(c("age", "wt"), ~ as.integer(.)) %>%
        filter(primaryid %in% drug_indi_info$primaryid))
    }
    if (stringr::str_detect(ascii_file, "outc|OUTC")) {
      outc_info <- outc_info %>% bind_rows(data.table::fread(ascii_file, sep = "$", quote = "", fill = FALSE) %>%
        {
          if ("outc_cod" %in% names(.)) {
            dplyr::rename(., "outc_code" = "outc_cod")
          } else {
            .
          }
        } %>%
        mutate_at(vars(ends_with("_dt")), list(~ arrange_date(.))) %>%
        filter(primaryid %in% drug_indi_info$primaryid))
    }
    if (stringr::str_detect(ascii_file, "reac|REAC")) {
      reac_info <- reac_info %>% bind_rows(data.table::fread(ascii_file, sep = "$", quote = "", fill = FALSE) %>%
        mutate_at(vars(ends_with("_dt")), list(~ arrange_date(.))) %>%
        filter(primaryid %in% drug_indi_info$primaryid))
    }
    if (stringr::str_detect(ascii_file, "rpsr|RPSR")) {
      rpsr_info <- rpsr_info %>% bind_rows(data.table::fread(ascii_file, sep = "$", quote = "", fill = FALSE) %>%
        mutate_at(vars(ends_with("_dt")), list(~ arrange_date(.))) %>%
        filter(primaryid %in% drug_indi_info$primaryid))
    }
    if (stringr::str_detect(ascii_file, "ther|THER")) {
      ther_info <- ther_info %>% bind_rows(data.table::fread(ascii_file, sep = "$", quote = "", fill = FALSE) %>%
        mutate_at(vars(ends_with("_dt")), list(~ arrange_date(.))) %>%
        {
          if ("dur" %in% names(.)) {
            .$dur <- as.double(.$dur)
            .
          } else {
            .
          }
        } %>%
        filter(primaryid %in% drug_indi_info$primaryid))
    }


    if (!is.null(cache_path)) {
      message(paste("saving", ascii_file, "into cache:", cache_path))

      cache_files <- c(cache_files, ascii_file)
      ascii_list <- list(
        demographics = demo_info,
        drug = drug_info,
        indication = indi_info,
        outcome = outc_info,
        reaction = reac_info,
        report_source = rpsr_info,
        therapy = ther_info
      )
      cache$files <- cache_files
      save(cache, file = cache_path)
    }
  }
  return(list(
    demographics = demo_info,
    drug = drug_info,
    indication = indi_info,
    outcome = outc_info,
    reaction = reac_info,
    report_source = rpsr_info,
    therapy = ther_info
  ))
}



#' Unify the list to a tabular format
#' 
#' Turn the list elements returned from \link{retrieve_faersascii} into a tabular format
#'
#' @param ascii_list list from \link{retrieve_faersascii}
#'
#' @return A data frame representing FAERS data, with all components from the list joined.
#' @export
unify_tabular_ascii <- function(ascii_list) {
  drug_indi_info <- ascii_list$drug %>%
    left_join(ascii_list$indication %>% group_by(primaryid, caseid, indi_drug_seq) %>%
      summarise(indi_pt = paste(sort(unique(indi_pt)), collapse = "/")) %>%
      ungroup(), by = c("primaryid", "caseid", "drug_seq" = "indi_drug_seq"))

  outcome_info <- ascii_list$outcome %>%
    group_by(primaryid, caseid) %>%
    tidyr::pivot_wider(
      id_cols = c("primaryid", "caseid"), names_from = "outc_code",
      values_fill = "0", values_from = "outc_code", values_fn = ~ ifelse(. == "0", "0", "1"),
      names_prefix = "outcome_"
    )

  report_source_info <- ascii_list$report_source %>%
    group_by(primaryid, caseid) %>%
    tidyr::pivot_wider(
      id_cols = c("primaryid", "caseid"), names_from = "rpsr_cod",
      values_fill = "0", values_from = "rpsr_cod", values_fn = ~ ifelse(. == "0", "0", "1"),
      names_prefix = "report_source_"
    )

  unified_faers <- ascii_list$reaction %>%
    left_join(ascii_list$demographics, by = c("primaryid", "caseid")) %>%
    mutate(patient_drug = lapply(primaryid, FUN = function(x) {
      drug_indi_info %>%
        filter(primaryid == x) %>%
        suppressMessages()
    })) %>%
    left_join(outcome_info, by = c("primaryid", "caseid")) %>%
    left_join(report_source_info, by = c("primaryid", "caseid")) %>%
    left_join(drug_indi_info %>%
      group_by(primaryid, caseid) %>%
      summarise(
        indi_pt_all = paste(sort(unique(indi_pt)), collapse = "/"),
        drugname_all = paste(sort(unique(drugname)), collapse = "/")
      ) %>%
      ungroup(), by = c("primaryid", "caseid")) %>%
    left_join(drug_indi_info %>%
      filter(role_cod == "PS") %>%
      group_by(primaryid, caseid) %>%
      summarise(
        indi_pt_ps = paste(sort(unique(indi_pt)), collapse = "/"),
        drugname_ps = paste(sort(unique(drugname)), collapse = "/")
      ) %>%
      ungroup(), by = c("primaryid", "caseid"))



  return(unified_faers)
}

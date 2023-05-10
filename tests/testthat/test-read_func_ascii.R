testthat::test_that("arrange_date", {
  testthat::expect_error(arrange_date("hju"))
  testthat::expect_equal(arrange_date("2020"), as.Date("2020-06-30") )
  testthat::expect_equal(arrange_date("202006"), as.Date("2020-06-15") )
  testthat::expect_equal(arrange_date("20200601"), as.Date("2020-06-01") )
})


testthat::test_that("retrieve_faersascii", {

  testthat::expect_error(object = retrieve_faersascii(object = "resources/nothing"))
  testthat::expect_true( object = is.list(retrieve_faersascii(ascii_dir = "resources/test_zip_ex_dir/ASCII", 
                                                                     cache_path = "resources/cache/cache_test.rdata")) )
  testthat::expect_true( object = is.list(retrieve_faersascii(ascii_dir = "resources/test_zip_ex_dir/ASCII", 
                                                               cache_path = "resources/cache/cache_test.rdata", drug_indication_pattern = "Parkinson")) )
  testthat::expect_true( object = is.list(retrieve_faersascii(ascii_dir = "resources/test_zip_ex_dir/ASCII", 
                                                               cache_path = "resources/cache/cache_test.rdata", drug_pattern = "Levodopa")) )
  testthat::expect_true( object = is.list(retrieve_faersascii(ascii_dir = "resources/test_zip_ex_dir/ASCII", 
                                                               cache_path = "resources/cache/cache_test.rdata",  
                                                               drug_indication_pattern = "Parkinson",
                                                               drug_pattern = "Levodopa")) )
})


testthat::test_that("unify_tabular_ascii", {
  
  faers_ascii_data <- retrieve_faersascii(ascii_dir = "resources/test_zip_ex_dir/ASCII",
                                          drug_indication_pattern = "Parkinson")
  faers_ascii_data_unified <- unify_tabular_ascii(ascii_list = faers_ascii_data)
  
  testthat::expect_true( object = is.data.frame(faers_ascii_data_unified))
})

# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

test_stations_metadata_colnames <-
  # Function takes df as argument. Prints either pass or fail depending on boolean value of evaluated expressions.
  function(df) {
    # create vector containing the expected values/names of each column [1:5] in transform_metadata_to_df.
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    # Evaluates whether expected column names are equal to those in transform_metadata_to_df. Prints a statement indicating pass if TRUE.
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    # If one or more of the evaluated column names differ from expected names, the function prints statement indicating fail if FALSE.
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }




test_stations_metadata_nrows <-
  
  function(df) {
    # store max and min expected row values, respectively.
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    # Checks if total number of rows in the df passed as argument in the function (transform_metadata_to_df) is between min and max values above.
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
      # Depending on whether there are too many or too few rows in df, a statement is printed indicating the problem.
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

test_stations_metadata_coltypes <-
  function(df) {
    # Store vector of expected column types in correct order.
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    # Evaluates if all column types in df equals to expected column types in correct order and prints statements indicating pass or fail.
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }
  
test_stations_metadata_nmissing <-
  function(df) {
    # Store max allowed missing values
    max_miss_vals <- 200
    # evaluates the sum of missing values and checks if it is less than the max allowable limit above.
    # Prints pass if TRUE.
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {

      print("PASS: Amount of missing values is reasonable")
      # Prints fail if false.
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }

test_stations_metadata_latestdata_timezone <-
  function(df) {
    # Evaluates if the attribute of column named latestData is has timezone equal to "UTC"
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
      # if timezone differs from UTC, a statement indicating fail is printed.
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }


test_stations_metadata <-
  # Function which passes df as argument in all tests above and run all tests providing 5 lines indicating pass or fail of each test.
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }






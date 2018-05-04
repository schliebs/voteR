#' Title
#'
#' @param dataset SPSS dataframe in tbl_df format imported with haven.
#' @param format specify whether you want a wide (each variable one row) or wide format (each value one row).
#'
#' @return a data.fram with meta information which can by querried with R-Studio functionalities.
#' @export
#'
#' @examples
make_dictionary <- function(dataset,format = c("wide","long")){
  #Function extracts metainformation from haven imported datasets
  #dataset: spss dataset imported with the library haven
  #format: specifies if wide or long format should be created

  #require("magrittr")
  #require("plyr")

  # Check input
  if(!("tbl_df" %in% class(dataset))) stop("function needs a dataset in 'tbl_df' format imported with 'haven'.")

  if(!(format %in% c("long","wide"))) stop("please specify either 'long' or 'wide' format (default)")


  # Helper functions
  helper1 <- function(x) paste(x,names(x),sep = ":")
  helper2 <- function(x) helper1(x) %>% paste(collapse = " | ")

  helper3 <- function(x){
    if(length(attr(x,"label")) == 0) attr(x,"label") <- NA
    if(length(attr(x,"labels")) == 0) attr(x,"labels") <- NA
    return(x)
  }

  # Assign NA to empty Labels
  dataset %<>% llply(helper3)

  # Basic information
  variables <- names(dataset)
  variable_label <-  laply(dataset,attr,"label")
  value_labels <- llply(dataset,attr,"labels")
  repeat_vars <- laply(value_labels,length)

  # Combine information
  if(format == "wide"){
    meta_info <- data.frame("variables" = variables,
                            "variable_label" = variable_label,
                            "value_labels" = value_labels %>% laply(helper2)
    )
  }else if(format == "long"){
    meta_info <- data.frame("variables" = rep(variables,repeat_vars),
                            "variable_label" = rep(variable_label,repeat_vars),
                            "value_labels" = value_labels %>%
                              llply(helper1) %>%
                              unlist()
    )
  }

  meta_info$value_labels[meta_info$value_labels == "NA:"] <- NA

  return(meta_info)
}
#----------------------------------------------------------------------------#

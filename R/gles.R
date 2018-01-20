# Recode Variable in GLES

#' gles_recode_partyvar
#' @description Recode a multiparty-variable in GESIS-Datasets such as the GERMAN LONGITUDINAL ELECTION STUDY (GLES)
#' @param year year the GLES-study is from. Defaults to 2017.
#' @param dataset_input Character string of the name of a dataframe containing the raw data.
#' @param dataset_output Character string of the name the output data frame (may already exist or not).
#' @param varname Character string of the name of the original variable.
#' @param own May apply: Different variable name for own position (on left-right scales, e.g.)
#' @param varlabel Character string of the to-be-assigned variable label.
#' @param key Character vector containing original alphabetic party keys.
#' @param partynames Character vector containing shortname party keys.
#' @param NAs Numeric vector containing to-be-assigned NAs/Missing values.

#' @return A data frame containing output dataframe including newly appended new-variables.
#' @examples
#' gles_recode_partyvar(year = 2017,
#'                      dataset_input = "gles2017",
#'                      dataset_output = "gles2017_out",
#'                      varname = "q52",
#'                      varlabel = "soz",
#'                      key = c("a","b","c","d","e","f","g"),
#'                      partynames = c("cdu","csu","spd","linke","gruene","fdp","afd"),
#'                      NAs = c(-97,-98,-99))
#' @export
gles_recode_partyvar <- function(year = 2017,
                        dataset_input = "gles2017",
                        dataset_output = "gles2017_out",
                        varname = "q52",
                        own = NULL,
                        varlabel = "soz",
                        key = c("a","b","c","d","e","f","g"),
                        partynames = c("cdu","csu","spd","linke","gruene","fdp","afd"),
                        NAs = c(-97,-98,-99)){

  try(if(length(key) != length(partynames)) stop("key and partynames vector not same length"))

  data_in <- eval(parse(text = paste0(dataset_input)))

  if(!exists(dataset_output)){eval(parse(text = paste0(dataset_output," <- data.frame(id = 1:nrow(",dataset_input,"))")))}
  eval(parse(text = paste0(dataset_output,"$year <- ",year)))

  for(q in 1:length(key)){

    eval(parse(text = paste0(
      dataset_output,"$",varlabel,"_",partynames[q]," <- ",dataset_input,"$",varname,key[q]
    )))

    eval(parse(text = paste0(
      dataset_output,"$",varlabel,"_",partynames[q],"[",dataset_input,"$",varname,key[q],"%in% c(",NAs,")] <- NA"
    )))
  }

  if(!is.null(own)){
    eval(parse(text = paste0(
      dataset_output,"$",varlabel,"_selbst <- ",dataset_input,"$",own
    )))

    eval(parse(text = paste0(
      dataset_output,"$",varlabel,"_selbst [",dataset_input,"$",own,"%in% c(",NAs,")] <- NA"
    )))
  }

  out_long <-
    tidyr::gather(data = dataset_output,key = party,value = value)

  head(out_long)
  ggplot(out_long) +
    geom_bar(aes(x = value)) +
    facet_wrap(~ party, ncol = 3, scales = "free")

  return(eval(parse(text = paste0(dataset_output))))
}

# test <- recode_gles(year = 2017,
#                     dataset_input = "gles2017",
#                     dataset_output = "gles2017_out",
#                     varname = "q52",
#                     varlabel = "soz",
#                     key = c("a","b","c","d","e","f","g"),
#                     partynames = c("cdu","csu","spd","linke","gruene","fdp","afd"),
#                     NAs = c(-97,-98,-99))


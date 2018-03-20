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
#' @param plot Logical T/F: Show relative frequency barplots while plotting.

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
                                 NAs = "<0",
                                 plot = TRUE){

  try(if(length(key) != length(partynames)) stop("key and partynames vector not same length"))

  data_in <- eval(parse(text = paste0(dataset_input)))

  if(!exists(dataset_output)){eval(parse(text = paste0(dataset_output," <- data.frame(id = 1:nrow(",dataset_input,"))")))}
  eval(parse(text = paste0(dataset_output,"$year <- ",year)))


  na_statement <- ifelse(!is.character(NAs),paste0("%in% c(",NAs,")"),paste0(NAs))

  for(q in 1:length(key)){

    eval(parse(text = paste0(
      dataset_output,"$",varlabel,"_",partynames[q]," <- ",dataset_input,"$",varname,key[q]
    )))

    eval(parse(text = paste0(
      dataset_output,"$",varlabel,"_",partynames[q],"[",dataset_input,"$",varname,key[q],na_statement,"] <- NA"
    )))
  }

  if(!is.null(own)){
    eval(parse(text = paste0(
      dataset_output,"$",varlabel,"_selbst <- ",dataset_input,"$",own
    )))

    eval(parse(text = paste0(
      dataset_output,"$",varlabel,"_selbst [",dataset_input,"$",own,na_statement,"] <- NA"
    )))
  }


  if(plot == TRUE){

    eval(parse(text = paste0("plot_df <- ",dataset_output,"%>% mutate_all(.funs = funs(as.character(.)))")))

    plot_df <- select(plot_df,contains(varlabel))

    out_long <-
      tidyr::gather(data = plot_df,key = party,value = value) %>%
      mutate_at(vars(value),funs(as.numeric(.))) %>%
      group_by(party) %>%
      mutate(n_group = n()) %>%
      group_by(n_group,party,value) %>%
      summarise(n = n()) %>%
      mutate(perc = n/n_group) %>%
      ungroup() %>%
      as.data.frame()

    gg <-
      ggplot(out_long) +
      geom_bar(aes(x = reorder(value,value),
                   y = perc),
               stat = "identity",
               position = "identity",na.rm = T) +
      facet_wrap(~ party, ncol = 3, scales = "free")+
      labs(x = NULL,
           y = "rel. frequency",
           title = paste0("Relative Frequencies ",varlabel," ",year," (",varname,")"))
    print(gg)
  }

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


# Calculate mean koalition issue position and create new variables

#' Get coalition members
#' @description Calculate mean koalition issue position and create new variables
#' @param data_in Character string containing the name the dataset.
#' @param coalition Character string containing the name of the coalition.
#' @param issue Character string containing the issue.
#' @return The treated dataset.
#' @examples
#' koa_members(data_in = "gles2017_out",
#'             coalition = "schwarzgelb",
#'             issue = "soz")
#' @export
koa_positions <- function(data_in = "gles2017_out",
                          coalition = "schwarzgelb",
                          issue = "soz"){

  members <- koa_members(coalition)

  eval(parse(text = paste0("out <- ",data_in,"%>% mutate(",issue,"_",coalition," = (",  paste0(issue,"_",members,collapse = " + "),")/",length(members),")")))

  return(out)

}


# Distance function

#' Calculate Distances
#' @description Calculate Distances from different parties/koalitions
#' @return The vector including all coalitions.
#' @examples
#' distance_function(data_in = "gles2017_out",
#'                   who = "schwarzgelb",
#'                   issue = "soz")
#' @export
distance_function <- function(data_in = "gles2017_out",
                              who = "schwarzgelb",
                              issue = "soz"){

  eval(parse(text = paste0("out <- ",data_in,"%>% mutate(",issue,"_distance_",who," = ",issue,"_",who,"-",issue,"_selbst)")))

  return(out)

}

# Intra-Coalition Heterogeneity function

#' Calculate Intra-Coalition Heterogeneity
#' @description Calculate Intra-Coalition Heterogeneity from different parties/koalitions
#' @return The vector including all coalitions.
#' @examples
#' distance_function(input = "gles2017_out",
#'                   who = "schwarzgelb",
#'                   issue = "soz")
#' @export
intrakoadistanz <- function(who = "schwarzgelb",
                            issue = "lr",
                            input = "gles2017_out",
                            year = 2017){

  parties <- koa_members(who)
  data <- eval(parse(text = input))

   values <-
    data %>%
    select(matches(paste0(parties,collapse="|"))) %>%
    select(matches(paste0(issue))) %>%
    select(-matches(paste0("dist"))) %>%
    mutate(rmax = do.call(pmax, .),
           rmin = do.call(pmin, .)) %>%
    mutate(hetero = abs(rmax-rmin)) %>%
    select(hetero) %>%
    as.data.frame()

  string1 <- paste0(input,"$hetero_",issue,"_",who," <<- as.vector(values$hetero)")
  eval(parse(text = string1))

  values2 <-
    gles %>%
    filter (year == year) %>%
    select(matches(paste0(c(parties,"selbst"),collapse="|"))) %>%
    select(matches(paste0(issue))) %>%
    select(-matches(paste0("dist"))) %>%
    mutate(rmax = do.call(pmax, .[,-ncol(.)]),
           rmin = do.call(pmin, .[,-ncol(.)])) %>%
    as.data.frame()

  selbst <- values2 %>% select(contains("selbst"))
  max = values2$rmax
  min = values2$rmin

  innerhalb <-
    ifelse(selbst <= max & selbst >= min,1,0) %>% as.numeric()

  string2 <- paste0(input,"$inside_",issue,"_",who," <<- innerhalb")
  eval(parse(text = string2))
}

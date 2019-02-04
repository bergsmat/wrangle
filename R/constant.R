globalVariables(c('constant_var','constant_val'))
#' Identify Constant Features of an Object
#' 
#' Identifies constant features of an object.  Generic, with methods for data.frame and grouped_df.
#' 
#' @export
#' @family constant
#' @param x object
#' @param ... passed arguments
constant <- function(x,...)UseMethod('constant')

#' Identify Constant Features of a Data Frame
#' 
#' Returns columns of a data.frame whose values do not vary within subsets defined by columns named in \dots.
#' 
#' @export
#' @family constant
#' @param x object
#' @param ... grouping columns
#' @return data.frame
#' @import dplyr
#' @importFrom tidyr gather
#' @importFrom rlang f_rhs syms as_string
#' @examples 
#' constant(Theoph)
#' constant(Theoph, Subject) # Wt Dose
#' Theoph$Study <- 1
#' constant(Theoph)
#' constant(Theoph, Study)
#' constant(Theoph, Study, Subject)
constant.data.frame <- function(x,...){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)
  stopifnot(all(vars %in% names(x)))
  tars <- setdiff(names(x),vars)
  if(!length(tars))return(unique(x))
  if(any(c('constant_var', 'constant_val') %in% names(x)))stop('constant_var, constant_val are reserved')
  y <- x
  suppressWarnings(y %<>% gather(constant_var, constant_val, !!!tars))
  y %<>% group_by(!!!syms(as.list(vars)),constant_var)
  y %<>% summarize(constant_val = length(unique(constant_val)))
  y %<>% group_by(constant_var) %>% select(constant_var, constant_val)
  y %<>% summarize(constant_val = max(constant_val))
  y %<>% filter(constant_val == 1)
  keep <- names(x)[names(x) %in% c(vars, y$constant_var)]
  x <- unique(x[,keep,drop = FALSE])
  x
}

#' Identify Constant Features of a Grouped Data Frame
#' 
#' Returns columns of a grouped_df whose values do not vary within subsets defined by groups. 
#' If any grouping arguments (dots) are supplied, existing groups are over-ridden.
#' 
#' @export
#' @family constant
#' @param x object
#' @param ... grouping columns
#' @return grouped data.frame
#' @import dplyr
constant.grouped_df <- function(x,...){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)
  groups <- vars
  if(!length(vars))  groups <- unlist(groups(x))
  x <- ungroup(x)
  y <- constant(x, !!!groups)
  y <- group_by(y, !!!groups)
  y
}


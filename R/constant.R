#' Identify Constant Features of an Object
#' 
#' Identifies constant features of an object.  Generic, with methods for data.frame and grouped_df.
#' 
#' @export
#' @keywords internal
#' @family constant
#' @param x object
#' @param ... passed arguments
constant <- function(x,...)UseMethod('constant')

#' Identify Constant Features of a Data Frame
#' 
#' Returns columns of a data.frame whose values do not vary within subsets 
#' defined by columns named in \dots. Defaults to groups(x) if none supplied, 
#' or all columns otherwise.
#' 
#' 
#' @export
#' @family constant
#' @param x object
#' @param ... optional grouping columns (named arguments are ignored)
#' @return data.frame (should be same class as x)
#' @import dplyr
#' @importFrom tidyr gather
#' @importFrom rlang f_rhs syms as_string
#' @importFrom magrittr %<>%
#' @examples
#' library(dplyr)
#' constant(Theoph)                      # data frame with 0 columns and 1 row
#' constant(Theoph, Subject)             # Subject Wt Dose Study
#' Theoph$Study <- 1
#' constant(Theoph)                      # Study
#' constant(Theoph, Study)               # Study
#' constant(Theoph, Study, Subject)      # Subject Wt Dose Study
#' Theoph <- group_by(Theoph, Subject)
#' constant(Theoph)                      # Subject Wt Dose Study
#' constant(Theoph, Study)               # Study
constant.data.frame <- function(x,...){
  
  # determine the legitimate un-named arguments
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  vars <- sapply(vars, as.character)
  if(!length(vars)) vars <- character(0) # else was named list
  stopifnot(all(vars %in% names(x)))
  tars <- setdiff(names(x),vars) # target vars to summarize
  # reconcile group_vars() with supplied groups (vars)
  
  # we consciously avoid group_by(),
  # which can change the class of the object
  # instead, we invoke the explicit grouping 
  # mechanism mutate(.by)
  
  # however, mutate(grouped_df, .by = ) is illegal.
  # thus, any reconciliation between vars and group_vars()
  # needs to defer to group_vars()
  # conflict only exists when x is grouped_df
  # (or less restrictively, when group_vars has length?)
  # AND vars has length (implying attempted over-ride)
  # least restrictive reconciliation is to re-group with vars
  grouped <- inherits(x, 'grouped_df')
  if(grouped && length(vars)) x %<>% group_by(across(all_of(vars)))
  
  # regardless above, tars cannot include group_vars
  tars %<>% setdiff(group_vars(x))
 
  # capture names of columns where within-cell values 
  # are singular for all cells as defined by groups
  y <- data.frame() # placeholder
  if( grouped) y <- mutate(x, across(all_of(tars), ~length(unique(.x)))) # already grouped
  if(!grouped) y <- mutate(x, across(all_of(tars), ~length(unique(.x))), .by = all_of(vars))
  
  # test for singularities across groups, if any
  y %<>% distinct %>% select(group_cols() | all_of(vars) | where(~ all(.x == 1))) 
  nms <- names(y)
  
  # recover the order of these as in x
  nms <- intersect(names(x), nms)
  
  # limit x to just these columns
  x %<>% select(all_of(nms))
  
  # find distinct combinations of values
  x %<>% distinct # per help file, columns not modified since ... is empty.
  return(x)
}

# #' Identify Constant Features of a Grouped Data Frame
# #' 
# #' Returns columns of a grouped_df whose values do not vary within subsets defined by groups. 
# #' If any grouping arguments (dots) are supplied, existing groups are over-ridden.
# #' 
# #' @export
# #' @family constant
# #' @param x object
# #' @param ... grouping columns
# #' @return grouped data.frame
# #' @import dplyr
# constant.grouped_df <- function(x,...){
#   args <- quos(...)
#   args <- lapply(args,f_rhs)
#   vars <- args[names(args) == '']
#   other <- args[names(args) != '']
#   vars <- sapply(vars, as.character)
#   groups <- vars
#   if(!length(vars))  groups <- unlist(groups(x))
#   x <- ungroup(x)
#   y <- constant(x, !!!groups)
#   y <- group_by(y, !!!groups)
#   y
# }


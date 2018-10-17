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
#' @importFrom rlang f_rhs
#' @examples 
#' constant(Theoph, Subject) # Wt Dose
constant.data.frame <- function(x,...){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)
  stopifnot(all(vars %in% names(x)))
  INDEX <- if(length(vars)) x[,vars,drop = FALSE] else rep(TRUE, nrow(x))
  FUN <- function(x)length(unique(x))
  order <- function(x, INDEX, fun) max(tapply(x,INDEX=INDEX,FUN=fun))
  ord <- sapply(x, FUN = order, INDEX = INDEX, fun = FUN)
  cols <- ord == 1
  x <- unique(x[,cols,drop = FALSE])
  x
}

#' Identify Constant Features of a Grouped Data Frame
#' 
#' Returns columns of a grouped_df whose values do not vary within subsets defined by groups.
#' 
#' @export
#' @family constant
#' @param x object
#' @param ... grouping columns
#' @return data.frame
#' @import dplyr
constant.grouped_df <- function(x,...){
  groups <- unlist(groups(x))
  x <- ungroup(x)
  y <- constant(x, !!!groups)
  y <- group_by(y, !!!groups)
  y
}


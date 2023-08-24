globalVariables(c('static_','original_'))

#' Arrange by groups.
#'
#' As of 0.5, dplyr::arrange ignores groups. This function gives the old behavior as a method for generic base::sort. Borrowed from Ax3man at https://github.com/hadley/dplyr/issues/1206.
#' @param x grouped_df
#' @param decreasing logical (ignored)
#' @param ... further sort criteria
#' @import dplyr magrittr
#' @importFrom tidyr spread
#' @export
#' @family sort
#' @return grouped_df
#' @examples
#' library(dplyr)
#' head(sort(group_by(Theoph, Subject, Time)))


sort.grouped_df <- function(x, decreasing = FALSE, ...) {
  x <- group_by(x, ..., .add = TRUE)
  x <- dplyr::arrange(x, .by_group = TRUE )
  x
}

#' Sort column subsets.
#' 
#' Sort column subsets.
#' @param x data.frame
#' @param ... columns to sort
#' @export
#' @family util
#' @keywords internal
#' @return grouped_df

detect <-  function(x,...) x %>%  
  ungroup %>%  
  transmute(...) %>%  
  group_by(across(everything())) %>%  
  sort


#' Show unique combinations of items in specified columns
#' 
#' Shows unique combinations of items in specified columns (unquoted).
#' @param x data.frame
#' @param ... columns to show
#' @export
#' @family util
#' @return grouped_df
#' @examples 
#' itemize(mtcars, cyl, gear, carb)
itemize <- function(x,...)x %>%  detect(...) %>%  unique

#' Count unique combinations of items in specified columns.
#' 
#' Counts unique combinations of items in specified columns (unquoted).
#' @param x data.frame
#' @param ... columns to show
#' @export
#' @family util
#' @return grouped_df
#' @examples
#' enumerate(mtcars, cyl, gear, carb)
enumerate <- function(x,...)x %>%  detect(...) %>%  summarise(count=n())

# #' Fetch the key.
# #' 
# #' Fetches the key of an object.
# #' @param x object of dispatch
# #' @param ... other arguments
# #' @family key
# #' @keywords internal
# #' @export
# #' 
# key <-            function(x,...)UseMethod('key')

#' Calculate naGroups.
#' 
#' Calculates naGroups.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
#' @family naGroups
#' @keywords internal
naGroups <- function(x,...)UseMethod('naGroups')
#' Calculate dupGroups.
#' 
#' Calculates dupGroups.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
#' @family dupGroups
#' @keywords internal
dupGroups <- function(x,...)UseMethod('dupGroups')
#' Report status.
#' 
#' Reports the status of an object.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
#' @family status
#' @keywords internal
#' @examples 
#' library(dplyr)
#' status(group_by(Theoph, Subject, Time))
status <- function(x,...)UseMethod('status')
#' Show unsorted elements.
#' 
#' Shows unsorted elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{unsorted.data.frame}}
#' @export
#' @family unsorted
#' @keywords internal
unsorted <- function(x,...)UseMethod('unsorted')

#' Show misplaced elements.
#' 
#' Shows misplaced elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
#' @family unsorted
#' @keywords internal
misplaced <- function(x,...)UseMethod('misplaced')

#' Index records whose relative positions would change if sorted.
#' 
#' Indexes records whose relative positions would change if sorted, i.e. records that would not have the same nearest neighbors (before and after).  unsorted() returns the records corresponding to this index.
#' @param x data.frame
#' @param ... optional grouping columns (named arguments are ignored)
#' @export
#' @family unsorted
#' @seealso \code{\link{na}} \code{\link{dup}}
#' @return logical with length nrow(x)
#' @importFrom dplyr arrange

misplaced.data.frame <- function(x,...){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  vars <- sapply(vars, as.character)
  if(!length(vars)) vars <- character(0) # else was named list
  stopifnot(all(vars %in% names(x)))
  if(length(vars)) x %<>% group_by(across(all_of(vars)))
  
  x$original_ <- as.double(seq_len(nrow(x)))
  x$leads_ <- lead(x$original_, default = Inf)
  x$lags_ <- lag(x$original_, default = -Inf)
  
  x %<>% arrange(.by_group = TRUE) # does nothing if no groups present
  x$now_leads_ <- lead(x$original_, default = Inf)
  x$now_lags_ <- lag(x$original_, default = -Inf)
  x$static_ <- with(x, leads_ == now_leads_ & lags_ == now_lags_)
  x %<>% arrange(original_)
  return(!x$static_)
}

#' Extract records whose relative positions would change if sorted.
#' 
#' Extracts records whose relative positions would change if sorted, i.e. records that would not have the same nearest neighbors (before and after). misplaced() returns the index that extracts these records.
#' @param x data.frame
#' @param ... optional grouping columns (named arguments are ignored)
#' @export
#' @family unsorted
#' @seealso \code{\link{na}} \code{\link{dup}}
#' @return data.frame, possibly grouped_df
#' @importFrom dplyr arrange

unsorted.data.frame <- function(x,...)x[misplaced(x, ...), , drop = FALSE]


#' Index records with NA values of grouping variables.
#' 
#' Indexes records with NA values of grouping variables.
#' @param x data.frame
#' @param ... optional grouping columns (named arguments are ignored)
#' @export
#' @family naGroups
#' @return logical

naGroups.data.frame <- function(x, ...){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  vars <- sapply(vars, as.character)
  if(!length(vars)) vars <- character(0) # else was named list
  stopifnot(all(vars %in% names(x)))
  if(length(vars)) x %<>% group_by(across(all_of(vars)))
  
  key <- group_vars(x)
  if (!all(key %in% names(x))) 
    stop("nonexistent groups(s)")
  if (nrow(x) == 0) 
    return(logical(0))
  if(!length(key)){ # i.e. no groups, therefore no NA groups
    return(rep(FALSE, nrow(x)))
  }
  y <- sapply(key, function(k) is.na(x[[k]]))
  if (nrow(x) == 1) 
    dim(y) <- c(1, length(y))
  as.logical(apply(y, 1, sum))
}

#' Index records with with duplicate or duplicated values of grouping variables.
#' 
#' Indexes records with with duplicate or duplicated values of grouping variables. If b follows a and and is the same, then b is a duplicate, a is duplicated, and both are shown.
#' @param x data.frame
#' @param ... optional grouping columns (named arguments are ignored)
#' @return grouped_df
#' @export
#' @family dupGroups
#' @return logical
dupGroups.data.frame <- function(x, ...){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  vars <- sapply(vars, as.character)
  if(!length(vars)) vars <- character(0) # else was named list
  stopifnot(all(vars %in% names(x)))
  if(length(vars)) x %<>% group_by(across(all_of(vars)))
  
  # if there are no groups, then none are duplicated
  if(!length(group_vars(x))) return(rep(FALSE, nrow(x)))
  x %<>% select(group_cols())
  # https://www.statology.org/dplyr-find-duplicates/
  # duplicated(y) | duplicated(y, fromLast = TRUE)
  x %<>% group_by(across(everything()))
  x %<>% mutate(wrangle_dup_ = n() > 1)
  x$wrangle_dup_
}

#' Report status with respect to grouping variables.
#' 
#' Reports status with respect to grouping variables.
#' @param x data.frame
#' @param ... optional grouping columns (named arguments are ignored)
#' @export
#' @family status
#' @return returns x invisibly (as originally grouped)
#' @examples 
#' library(dplyr)
#' status(Theoph)
#' status(Theoph, Subject)
#' status(group_by(Theoph, Subject, Time))
#' @seealso 
#' \code{\link{na}} 
#' \code{\link{dup}}
#' \code{\link{unsorted}}
#' \code{\link{informative}}
#' \code{\link{ignore}}
#' \code{\link{itemize}}
#' \code{\link{enumerate}}
#' \code{\link{sort.grouped_df}}

status.data.frame <- function (x, ...) 
{
  o <- x
  # determine the legitimate un-named arguments
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  vars <- sapply(vars, as.character)
  if(!length(vars)) vars <- character(0) # else was named list
  stopifnot(all(vars %in% names(x)))
  if(length(vars)) x %<>% group_by(across(all_of(vars)))
  
  cat("Source: local data frame ", dplyr::dim_desc(x), "\n", sep = "")
  cat("Groups: ",                  group_vars(x), "\n", sep = " ")
  cat("NAs: ",                     sum(naGroups(x)), "\n", sep = "")
  cat("duplicates: ",              sum(dupGroups(x)), "\n", sep = "")
  cat("unsorted: ",                sum(misplaced(x)), "\n", sep = "")
  cat("\n")
  invisible(o)
}

#' Show na elements.
#' 
#' Shows na elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{na.data.frame}} \code{\link{dup}} \code{\link{weak}} \code{\link{unsorted}}
#' @export
#' @keywords internal
#' @family na
na  <- function(x, ...)UseMethod('na')
#' Show duplicate or duplicated elements.
#' 
#' Shows duplicate or duplicated elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{dup.data.frame}} \code{\link{na}} \code{\link{weak}}  \code{\link{unsorted}}
#' @export
#' @keywords internal
#' @family dup
dup <- function(x,...)UseMethod('dup')
#' Show na, duplicate, or duplicated elements.
#' 
#' Shows na, duplicate, or duplicated elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{weak.data.frame}}
#' @export
#' @keywords internal
#' @family weak
weak <- function(x,...)UseMethod('weak')

#' Show records with NA values of grouping variables.
#' 
#' Shows records with NA values of grouping variables.
#' @param x data.frame
#' @param ... optional grouping columns (named arguments are ignored)
#' @export
#' @family na
#' @return data.frame

na.data.frame <- function(x,...)x[naGroups(x, ...), , drop = FALSE]

#' Show records with duplicate or duplicated values of grouping variables.
#' 
#' Shows records with duplicate or duplicated values of grouping variables.
#' @param x data.frame
#' @param ... optional grouping columns (named arguments are ignored)
#' @export
#' @family dup
#' @return data.frame
#' @examples 
#' library(dplyr)
#' dupGroups(mtcars)
#' dupGroups(group_by(mtcars, mpg))
#' dup(group_by(mtcars, mpg))
dup.data.frame <- function(x, ...) x[dupGroups(x, ...), , drop = FALSE]


#' Show records with NA, duplicate or duplicated values of grouping variables.
#' 
#' Shows records with NA, duplicate or duplicated values of grouping variables.
#' @param x data.frame
#' @param ... optional grouping columns (named arguments are ignored)
#' @export
#' @family weak
#' @return data.frame
weak.data.frame <- function(x,...)x[naGroups(x, ...) | dupGroups(x, ...), , drop = FALSE]


singular <- function(x,...)length(unique(x)) == 1

#' Find unique records for subset of columns with one unique value.
#' 
#' Finds unique records for subset of columns with one unique value.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family util
#' @return data.frame
static <- function(x,...){
  s <- x %>% summarise_each(funs(singular))
  nms <- names(s)[sapply(s,function(col)all(col == TRUE))]
  x <- select_(x, .dots=as.list(nms))
  x <- x %>% unique
  x
}

#' Drop columns in x that are present in y.
#' 
#' Drops columns in x that are present in y.
#' @param x data.frame
#' @param y data.frame
#' @param ... ingored
#' @export
#' @family ignore
#' @return data.frame

ignore <- function(x,y,...){
  x[,! names(x) %in% names(y), drop=FALSE]
}

#' Drop columns in x that are entirely NA.
#' 
#' Drops columns in x that are entirely NA.
#' @param x object of dispatch
#' @param ... passed
#' @seealso \code{\link{informative.data.frame}}
#' @examples 
#' head(Theoph)
#' Theoph$Dose <- NA
#' head(informative(Theoph))
#' @export
#' @family informative

informative <- function(x,...)UseMethod('informative')

#' Drop columns in x that are entirely NA.
#' 
#' Drops columns in x that are entirely NA.
#' @param x data.frame
#' @param ... ingored
#' @export
#' @family informative
#' @return data.frame


informative.data.frame <- function(x,...)x[,sapply(x,function(col)any(!is.na(col))),drop=FALSE]


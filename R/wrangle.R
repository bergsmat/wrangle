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
itemize <-         function(x,...)x %>%  detect(...) %>%  unique

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
enumerate <-      function(x,...)x %>%  detect(...) %>%  summarise(count=n())

# #' Fetch the key.
# #' 
# #' Fetches the key of an object.
# #' @param x object of dispatch
# #' @param ... other arguments
# #' @family key
# #' @aliases NULL key_generic
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
#' @aliases NULL naGroups_generic
#' @keywords internal
naGroups <-       function(x,...)UseMethod('naGroups')
#' Calculate dupGroups.
#' 
#' Calculates dupGroups.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
#' @family dupGroups
#' @aliases NULL dupGroups_generic
#' @keywords internal
dupGroups <-      function(x,...)UseMethod('dupGroups')
#' Report status.
#' 
#' Reports the status of an object.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
#' @family status
#' @aliases NULL status_generic
#' @keywords internal
#' @examples 
#' library(dplyr)
#' status(group_by(Theoph, Subject, Time))
status <-         function(x,...)UseMethod('status')
#' Show unsorted elements.
#' 
#' Shows unsorted elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{unsorted.data.frame}}
#' @export
#' @family unsorted
#' @aliases NULL unsorted_generic
#' @keywords internal
unsorted <-       function(x,...)UseMethod('unsorted')

# #' Fetch the key for a grouped_df as character vector
# #' 
# #' Fetches the key for a grouped_df as character vector
# #' @param x data.frame
# #' @param ... columns to show
# #' @return character
# #' @family key
# #' @importFrom dplyr group_vars
# #' @export
# key.data.frame <- function(x,...)group_vars(x)

#' Find records whose relative positions would change if sorted.
#' 
#' Finds records whose relative positions would change if sorted, i.e. records that would not have the same nearest neighbors (before and after).
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family unsorted
#' @aliases unsorted
#' @seealso \code{\link{na}} \code{\link{dup}}
#' @return data.frame
#' @importFrom dplyr arrange


unsorted.data.frame <- function(x,...){
  x$original_ <- as.double(seq_len(nrow(x)))
  x$leads_ <- lead(x$original_, default = Inf)
  x$lags_ <- lag(x$original_, default = -Inf)
  x %<>% sort
  x$now_leads_ <- lead(x$original_, default = Inf)
  x$now_lags_ <- lag(x$original_, default = -Inf)
  x$static_ <- with(x, leads_ == now_leads_ & lags_ == now_lags_)
  x %<>% filter(static_==FALSE) 
  #x <- x[order(x$original_),]
  x %<>% arrange(original_)
  x %<>% select(-(original_:static_))
  x
}

#' Count records with NA values of grouping variables.
#' 
#' Counts records with NA values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @aliases naGroups
#' @family naGroups
#' @return logical

naGroups.data.frame <- function(x, ...){
  key <- group_vars(x)
  if (!all(key %in% names(x))) 
    stop("nonexistent groups(s)")
  if (nrow(x) == 0) 
    return(logical(0))
  y <- sapply(key, function(k) is.na(x[[k]]))
  if (nrow(x) == 1) 
    dim(y) <- c(1, length(y))
  as.logical(apply(y, 1, sum))
}

#' Count records with with duplicate or duplicated values of grouping variables.
#' 
#' Counts records with with duplicate or duplicated values of grouping variables. If b follows a and and is the same, then b is a duplicate, a is duplicated, and both are shown.
#' @param x data.frame
#' @param ... ignored
#' @return grouped_df
#' @export
#' @aliases dupGroups
#' @family dupGroups
#' @return logical
dupGroups.data.frame <- function(x, ...){
  # key <- key(x)
  # if (!all(key %in% names(x))) 
  #   stop("nonexistent groups(s)")
  # y <- x[, key, drop = FALSE]
  
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
#' @param ... ignored
#' @export
#' @family status
#' @aliases status
#' @return returns x invisibly
#' @examples 
#' library(dplyr)
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
  cat("Source: local data frame ", dplyr::dim_desc(x), "\n", sep = "")
  cat("Groups: ",                  group_vars(x), "\n", sep = " ")
  cat("NAs: ",                     sum(naGroups(x)), "\n", sep = "")
  cat("duplicates: ",              sum(dupGroups(x)), "\n", sep = "")
  cat("unsorted: ",                nrow(unsorted(x)), "\n", sep = "")
  cat("\n")
  invisible(x)
}

#' Show na elements.
#' 
#' Shows na elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{na.data.frame}} \code{\link{dup}} \code{\link{weak}} \code{\link{unsorted}}
#' @export
#' @aliases NULL na_generic
#' @keywords internal
#' @family na
na  <-             function(x, ...)UseMethod('na')
#' Show duplicate or duplicated elements.
#' 
#' Shows duplicate or duplicated elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{dup.data.frame}} \code{\link{na}} \code{\link{weak}}  \code{\link{unsorted}}
#' @export
#' @aliases NULL dup_generic
#' @keywords internal
#' @family dup
dup <-             function(x,...)UseMethod('dup')
#' Show na, duplicate, or duplicated elements.
#' 
#' Shows na, duplicate, or duplicated elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{weak.data.frame}}
#' @export
#' @aliases NULL weak_generic
#' @keywords internal
#' @family weak
weak <-            function(x,...)UseMethod('weak')

#' Show records with NA values of grouping variables.
#' 
#' Shows records with NA values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family na
#' @aliases na
#' @return data.frame


na.data.frame <-   function(x,...)x[naGroups(x),]

#' Show records with duplicate or duplicated values of grouping variables.
#' 
#' Shows records with duplicate or duplicated values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family dup
#' @return data.frame
#' @aliases dup
#' @examples 
#' library(dplyr)
#' dupGroups(mtcars)
#' dupGroups(group_by(mtcars, mpg))
#' dup(group_by(mtcars, mpg))
dup.data.frame <-  function(x,...) x[dupGroups(x),]


#' Show records with NA, duplicate or duplicated values of grouping variables.
#' 
#' Shows records with NA, duplicate or duplicated values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @aliases weak
#' @family weak
#' @return data.frame
weak.data.frame <- function(x,...)x[naGroups(x) | dupGroups(x),]


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
  x[,! names(x) %in% names(y),drop=FALSE]
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


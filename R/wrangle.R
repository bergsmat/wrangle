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
# @describeIn wrangle

sort.grouped_df <- function(x, decreasing = FALSE, ...) {
  x <- group_by(x, ..., .add = TRUE)
  x <- dplyr::arrange(x, .by_group = TRUE )
  x
}

# Group by all columns.
#
# Groups by all columns.
# @param x data.frame
# @param ... ignored
# @export
# @family group_by_all
# @return grouped_df
# @describeIn wrangle

#
#group_by_all <-   function(x,...)x %>%  group_by_(.dots=as.list(names(x)))
#group_by_all <-   function(x,...)do.call(group_by,c(list(.data=x),lapply(names(x),as.symbol)))


#' Sort column subsets.
#' 
#' Sort column subsets.
#' @param x data.frame
#' @param ... columns to sort
#' @export
#' @family util
#' @return grouped_df
# @describeIn wrangle
detect <-  function(x,...)x %>%  ungroup %>%  transmute(...) %>%  group_by(across()) %>%  sort


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
# @describeIn wrangle
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
# @describeIn wrangle
enumerate <-      function(x,...)x %>%  detect(...) %>%  summarise(count=n())

#' Fetch the key.
#' 
#' Fetches the key of an object.
#' @param x object of dispatch
#' @param ... other arguments
#' @family key
#' @export
#' 
key <-            function(x,...)UseMethod('key')
#' Calculate naGroups.
#' 
#' Calculates naGroups.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
#' @family naGroups
naGroups <-       function(x,...)UseMethod('naGroups')
#' Calculate dupGroups.
#' 
#' Calculates dupGroups.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
#' @family dupGroups
dupGroups <-      function(x,...)UseMethod('dupGroups')
#' Report status.
#' 
#' Reports the status of an object.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
#' @family status
#' @examples 
#' library(dplyr)
#' status(group_by(Theoph, Subject, Time))
status <-         function(x,...)UseMethod('status')
#' Show unsorted elements.
#' 
#' Shows unsorted elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{unsorted.grouped_df}}
#' @export
#' @family unsorted
unsorted <-       function(x,...)UseMethod('unsorted')

#' Fetch the key for a grouped_df as character vector
#' 
#' Fetches the key for a grouped_df as character vector
#' @param x data.frame
#' @param ... columns to show
#' @return character
#' @family key
#' @export

key.grouped_df <- function(x,...)sapply(groups(x),as.character)

#' Find records whose relative positons would change if sorted.
#' 
#' Finds records whose relative positons would change if sorted, i.e. records that would not have the same nearest neighbors (before and after).
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family unsorted
#' @seealso \code{\link{na}} \code{\link{dup}}
#' @return grouped_df
# @describeIn wrangle

unsorted.grouped_df <- function(x,...){
  x$original_ <- seq_len(nrow(x))
  x$leads_ <- lead(x$original_,default=Inf)
  x$lags_ <- lag(x$original_,default=-Inf)
  x %<>% sort
  x$now_leads_ <- lead(x$original_,default=Inf)
  x$now_lags_ <- lag(x$original_,default=-Inf)
  x$static_ <- with(x, leads_ == now_leads_ & lags_ == now_lags_)
  x <- x[order(x$original_),]
  x %<>% filter(static_==FALSE) %>% select(-(original_:static_))
  x
}

#' Count records with NA values of grouping variables.
#' 
#' Counts records with NA values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family naGroups
#' @return numeric

naGroups.grouped_df <- function(x, ...){
  key <- key(x)
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
#' @family dupGroups
dupGroups.grouped_df <- function(x, ...){
  key <- key(x)
  if (!all(key %in% names(x))) 
    stop("nonexistent groups(s)")
  y <- x[, key, drop = FALSE]
  duplicated(y) | duplicated(y, fromLast = TRUE)
}

#' Report status with respect to grouping variables.
#' 
#' Reports status with respect to grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family status
#' @aliases wrangle
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
# @describeIn wrangle
status.grouped_df <- function (x, ...) 
{
  cat("Source: local data frame ", dplyr::dim_desc(x), "\n", sep = "")
  cat("Groups: ",                  key(x), "\n", sep = " ")
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
#' @seealso \code{\link{na.grouped_df}} \code{\link{dup}} \code{\link{weak}} \code{\link{unsorted}}
#' @export
#' @family na
na  <-             function(x, ...)UseMethod('na')
#' Show duplicate or duplicated elements.
#' 
#' Shows duplicate or duplicated elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{dup.grouped_df}} \code{\link{na}} \code{\link{weak}}  \code{\link{unsorted}}
#' @export
#' @family dup
dup <-             function(x,...)UseMethod('dup')
#' Show na, duplicate, or duplicated elements.
#' 
#' Shows na, duplicate, or duplicated elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @seealso \code{\link{weak.grouped_df}}
#' @export
#' @family weak
weak <-            function(x,...)UseMethod('weak')

#' Show records with NA values of grouping variables.
#' 
#' Shows records with NA values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family na
#' @return grouped_df
# @describeIn wrangle

na.grouped_df <-   function(x,...)x[naGroups(x),]

#' Show records with duplicate or duplicated values of grouping variables.
#' 
#' Shows records with duplicate or duplicated values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family dup
#' @return grouped_df
#' @examples 
#' library(dplyr)
#' dup(group_by(mtcars, mpg))
# @describeIn wrangle
dup.grouped_df <-  function(x,...)x[dupGroups(x),]


#' Show records with NA, duplicate or duplicated values of grouping variables.
#' 
#' Shows records with NA, duplicate or duplicated values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family weak
#' @return grouped_df
# @describeIn wrangle
weak.grouped_df <- function(x,...)x[naGroups(x) | dupGroups(x),]


singular <- function(x,...)length(unique(x)) == 1

#' Find unique records for subset of columns with one unique value.
#' 
#' Finds unique records for subset of columns with one unique value.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @family util
#' @return data.frame
# @describeIn wrangle
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
# @describeIn wrangle
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
# @describeIn wrangle
informative <- function(x,...)UseMethod('informative')

#' Drop columns in x that are entirely NA.
#' 
#' Drops columns in x that are entirely NA.
#' @param x data.frame
#' @param ... ingored
#' @export
#' @family informative
#' @return data.frame
# @describeIn wrangle

informative.data.frame <- function(x,...)x[,sapply(x,function(col)any(!is.na(col))),drop=FALSE]

# #' Unstack a grouped_df.
# #' 
# #' Unstacks a grouped_df.
# #' @param data passed
# #' @param key_col passed
# #' @param value_col passed
# #' @param fill passed
# #' @param convert passed
# #' @param drop passed
# #' @export
# #' @return grouped_df
# # @describeIn wrangle
# spread_.grouped_df <- function (
#   data, 
#   key_col, 
#   value_col, 
#   fill = NA, 
#   convert = FALSE, 
#   drop = TRUE
# ) {
#   y <- NextMethod()
#   grp <- groups(data)
#   good <- sapply(grp,function(g)g != key_col)
#   grp <- grp[good]
#   y <- group_by_(y, .dots=grp)
#   y  
# }
# 
# #' Mutate a grouped_df.
# #' 
# #' Mutates a grouped_df.
# #' @param .data data.frame
# #' @param ... passed
# #' @export
# #' @return grouped_df
# # @describeIn wrangle
# mutate_.grouped_df <- function (.data, ...) 
# {
#   y <- NextMethod()
#   y <- group_by_(y, .dots=groups(.data))
#   y
# }
# 
# #' Filter a grouped_df.
# #' 
# #' Filters a grouped_df.
# #' @param .data data.frame
# #' @param ... passed
# #' @export
# #' @return grouped_df
# # @describeIn wrangle
# filter_.grouped_df <- function (.data, ...) 
# {
#   y <- NextMethod()
#   y <- group_by_(y, .dots=groups(.data))
#   y
# }
# 
# #' Anti-join a grouped_df.
# #' 
# #' Anti-joins a grouped_df.
# #' @param .data data.frame
# #' @param ... passed
# #' @export
# #' @return grouped_df
# # @describeIn wrangle
# anti_join_.grouped_df <- function (.data, ...) 
# {
#   y <- NextMethod()
#   y <- dplyr::group_by_(y, .dots=groups(.data))
#   y
# }
# #' Semi-join a grouped_df.
# #' 
# #' Semi-joins a grouped_df.
# #' @param .data data.frame
# #' @param ... passed
# #' @export
# #' @return grouped_df
# # @describeIn wrangle
# semi_join_.grouped_df <- function (.data, ...) 
# {
#   y <- NextMethod()
#   y <- dplyr::group_by_(y, .dots=groups(.data))
#   y
# }
# #' Test whether item has only one unique value.
# #' 
# #' Tests whether item has only one unique value.
# #' @param x vector
# #' @param ... ignored
# #' @return logical


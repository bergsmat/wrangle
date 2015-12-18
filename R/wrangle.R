#' Group by all columns.
#'
#' Groups by all columns.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @return grouped_df
# @describeIn wrangle

#'
#group_by_all <-   function(x,...)x %>%  group_by_(.dots=as.list(names(x)))
group_by_all <-   function(x,...)do.call(group_by,c(list(.data=x),lapply(names(x),as.symbol)))


#' Sort column subsets.
#' 
#' Sort column subsets.
#' @param x data.frame
#' @param ... columns to sort
#' @export
#' @return grouped_df
# @describeIn wrangle
detect <-  function(x,...)x %>%  ungroup %>%  transmute(...) %>%  group_by_all %>%  arrange


#' Show unique combinations of items in specified columns
#' 
#' Shows unique combinations of items in specified columns.
#' @param x data.frame
#' @param ... columns to show
#' @export
#' @return grouped_df
# @describeIn wrangle
itemize <-         function(x,...)x %>%  detect(...) %>%  distinct

#' Count unique combinations of items in specified columns.
#' 
#' Counts unique combinations of items in specified columns.
#' @param x data.frame
#' @param ... columns to show
#' @export
#' @return grouped_df
# @describeIn wrangle
enumerate <-      function(x,...)x %>%  detect(...) %>%  summarise(count=n())

#' Fetch the key.
#' 
#' Fetches the key of an object.
#' @param x object of dispatch
#' @param ... other arguments
key <-            function(x,...)UseMethod('key')
#' Calculate naGroups.
#' 
#' Calculates naGroups.
#' @param x object of dispatch
#' @param ... other arguments
naGroups <-       function(x,...)UseMethod('naGroups')
#' Calculate dupGroups.
#' 
#' Calculates dupGroups.
#' @param x object of dispatch
#' @param ... other arguments
dupGroups <-      function(x,...)UseMethod('dupGroups')
#' Report status.
#' 
#' Reports the status of an object.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
status <-         function(x,...)UseMethod('status')
#' Show unsorted elements.
#' 
#' Shows unsorted elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
unsorted <-       function(x,...)UseMethod('unsorted')

#' Fetch the key for a grouped_df as character vector
#' 
#' Fetches the key for a grouped_df as character vector
#' @param x data.frame
#' @param ... columns to show
#' @return character

key.grouped_df <- function(x,...)sapply(groups(x),as.character)

#' Find records whose relative positons would change if sorted.
#' 
#' Finds records whose relative positons would change if sorted.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @return grouped_df
# @describeIn wrangle

unsorted.grouped_df <- function(x,...){
  x$original_ <- seq_len(nrow(x))
  x$leads_ <- lead(x$original_,default=Inf)
  x$lags_ <- lag(x$original_,default=-Inf)
  x %<>% arrange
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
#' Counts records with with duplicate or duplicated values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @return grouped_df
#' @export
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
#' @return returns x invisibly
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
#' @export
na  <-             function(x, ...)UseMethod('na')
#' Show duplicate or duplicated elements.
#' 
#' Shows duplicate or duplicated elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
dup <-             function(x,...)UseMethod('dup')
#' Show na, duplicate, or duplicated elements.
#' 
#' Shows na, duplicate, or duplicated elements.
#' @param x object of dispatch
#' @param ... other arguments
#' @export
weak <-            function(x,...)UseMethod('weak')

#' Show records with NA values of grouping variables.
#' 
#' Shows records with NA values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @return grouped_df
# @describeIn wrangle

na.grouped_df <-   function(x,...)x[naGroups(x),]

#' Show records with duplicate or duplicated values of grouping variables.
#' 
#' Shows records with duplicate or duplicated values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @return grouped_df
# @describeIn wrangle
dup.grouped_df <-  function(x,...)x[dupGroups(x),]


#' Show records with NA, duplicate or duplicated values of grouping variables.
#' 
#' Shows records with NA, duplicate or duplicated values of grouping variables.
#' @param x data.frame
#' @param ... ignored
#' @export
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
#' @return data.frame
# @describeIn wrangle
static <- function(x,...){
  s <- x %>% summarise_each(funs(singular))
  nms <- names(s)[sapply(s,function(col)all(col == TRUE))]
  x <- select_(x, .dots=as.list(nms))
  x <- x %>% distinct
  x
}

#' Drop columns in x that are present in y.
#' 
#' Drops columns in x that are present in y.
#' @param x data.frame
#' @param y data.frame
#' @param ... ingored
#' @export
#' @return data.frame
# @describeIn wrangle
ignore <- function(x,y,...){
  x[,! names(x) %in% names(y)]
}

#' Drop columns in x that are entirely NA.
#' 
#' Drops columns in x that are entirely NA.
#' @param x object of dispatch
#' @param ... passed
# @describeIn wrangle
informative <- function(x,...)UseMethod('informative')

#' Drop columns in x that are entirely NA.
#' 
#' Drops columns in x that are entirely NA.
#' @param x data.frame
#' @param ... ingored
#' @export
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


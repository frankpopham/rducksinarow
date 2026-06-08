#' Copy tables within and between databases and ducklakes
#'
#' @param connection a duckdb connection
#' @param from name of new table
#' @param to name of existing table
#'
#' @returns NULL (invisibly) but copies a table within or between databases.
#' @export
#'
copy_table <- function(connection, from, to) {

  to_sql <- DBI::SQL(to)

  from_sql <- DBI::SQL(from)

  copy_sql <- glue::glue_sql("CREATE TABLE {to_sql} AS FROM {from_sql};", .con = connection)

  DBI::dbSendQuery(connection, copy_sql)

  invisible()
}

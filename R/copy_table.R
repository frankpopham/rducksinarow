#' Copy tables within and between databases and ducklakes in same process
#'
#' @param connection a duckdb connection
#' @param from name of existing table
#' @param to name of new table
#'
#' @returns NULL (invisibly) but copies a table within or between databases.
#' @export
#'
copy_table <- function(connection, from, to) {

  check_duckdb_connection(connection)

  rlang::check_string(from)
  rlang::check_string(to)

  to_sql <- DBI::SQL(to)

  from_sql <- DBI::SQL(from)

  copy_sql <- glue::glue_sql("CREATE TABLE {to_sql} AS FROM {from_sql};", .con = connection)

  DBI::dbSendQuery(connection, copy_sql)

  invisible()
}

make_temp_con <- function() {

  duckdb_temp <- tempfile(pattern = "duckdb_con", fileext = ".duckdb")
  DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_temp)

}

check_duckdb_connection <-  function(connection) {
  stopifnot("Not a duckdb connection" = class(connection) == "duckdb_connection")

}

install_load_ext <- function(connection) {

for(i in c("ducklake", "httpfs", "sqlite"))  {
DBI::dbSendQuery(connection, glue::glue_sql("INSTALL {i};", .con = connection))
DBI::dbSendQuery(connection, glue::glue_sql("LOAD {i};", .con = connection))

}
}

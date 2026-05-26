#' Attach to a duckdb database. If database does not exist it will create a new one (if read only is FALSE).
#'
#' @param connection a duckdb connection. If NULL (the default), creates a connection in the temporary directory
#' @param filename the filename of an existing (or new) duckdb database (required)
#' @param alias an alias for the database (required)
#' @param read_only default is TRUE, set to FALSE if you need to write to the database or create a new database
#' @param encrypt set to TRUE if the database is encrypted (or you want to encrypt the new database), will ask for your password (key)
#'
#' @returns The duckdb connection with the database attached. Should be assigned if connection is NULL.
#' @export
#'
#' @examples
attach_database <- function(connection = NULL,
                            filename,
                            alias,
                            read_only = TRUE,
                            encrypt = FALSE) {

if(is.null(connection)) {

temp_con <- make_temp_con()

temp_duckdb <- unlist(strsplit(fs::path_file(temp_con@driver@dbdir), split = ".", fixed = TRUE))[1]

} else{

check_duckdb_connection(connection)
temp_con <- connection
}

rlang::check_string(filename)

rlang::check_string(alias)
rlang::check_bool(read_only)
rlang::check_bool(encrypt)

if(!encrypt) {

  attach_sql <- glue::glue_sql("ATTACH {filename} AS {`alias`}
                 (READ_ONLY {read_only});", .con = temp_con)

  DBI::dbSendQuery(temp_con, attach_sql)


} else if(encrypt) {

  attach_sql <- glue::glue_sql("ATTACH {filename} AS {`alias`}
                 (READ_ONLY {read_only}, ENCRYPTION_KEY ?);", .con = temp_con)

  DBI::dbSendQuery(temp_con, attach_sql, params = askpass::askpass())
}

switch_default_duck(temp_con, alias)

if(is.null(connection)) {
detach_duck(temp_con, temp_duckdb)
}

invisible(temp_con)

}


#' Attach to a ducklake.
#'
#' @description
#'  If database does not exist it will create a new one (if read only is FALSE).
#'
#' @param connection a duckdb connection. If NULL (the default), creates a connection in the temporary directory
#' @param type database (catlog) of the ducklake (duckdb or sqlite at moment)
#' @param filename the filename of an existing (or new) ducklake (required)
#' @param alias  a simpler alias for the ducklake (required)
#' @param read_only default is TRUE, set to FALSE if you need to write to the ducklake.
#' @param encrypt set to TRUE if the ducklake is encrypted (or you want to encrypt your new ducklake), will ask for your password (key)
#' @param parquet_encrypt Only applies if creating a new ducklake, if TRUE parquet files that hold the data in the ducklake are encrypted
#' @param parquet_directory Only needs setting if creating a new ducklake. give a directory name for parquet files (will create directory if doesn't exist)
#' @param override_parquet_directory For an existing ducklake want to use an alternative parquet directory. Default is FALSE
#'
#' @returns
#' @export
#'
#' @examples
attach_ducklake <- function(connection = NULL,
                        type = "duckdb",
                        filename,
                        alias,
                        read_only = TRUE,
                        encrypt = FALSE,
                        parquet_encrypt = FALSE,
                        parquet_directory,
                        override_parquet_directory = FALSE
                        ) {

  if(is.null(connection)) {

    temp_con <- make_temp_con()

    temp_duckdb <- unlist(strsplit(fs::path_file(temp_con@driver@dbdir), split = ".", fixed = TRUE))[1]

  } else{

    check_duckdb_connection(connection)
    temp_con <- connection
  }

  rlang::check_string(filename)

  rlang::check_string(alias)
  rlang::check_bool(read_only)
  rlang::check_bool(encrypt)
  rlang::check_bool(encrypt)
  rlang::arg_match(type, values = c("duckdb", "sqlite"))

  if(type == "sqlite" && encrypt == TRUE) {
    stop("If sqlite is the catalog, encryption of the database is not supported")
  }


  if(type == "duckdb") {
  duck_filename <- DBI::dbQuoteLiteral(temp_con, paste0("ducklake:", filename))
  } else if(type == "sqlite")  {
  duck_filename <- DBI::dbQuoteLiteral(temp_con, paste0("ducklake:sqlite:", filename))
}

  if(!encrypt && file.exists(filename)) {

    attach_sql <- glue::glue_sql("ATTACH {duck_filename} AS {`alias`}
    (READ_ONLY {read_only},
    OVERRIDE_DATA_PATH {override_parquet_directory}, DATA_PATH {parquet_directory});",
                                 .con = temp_con)

    DBI::dbSendQuery(temp_con, attach_sql)

} else if(encrypt && file.exists(filename)) {

    attach_sql <- glue::glue_sql("ATTACH {duck_filename} AS {`alias`}
                 (READ_ONLY {read_only}, OVERRIDE_DATA_PATH {override_parquet_directory},
                  META_ENCRYPTION_KEY ?, DATA_PATH {parquet_directory});", .con = temp_con)

    DBI::dbSendQuery(temp_con, attach_sql, params = askpass::askpass())
} else if(!encrypt && !file.exists(filename)) {

  attach_sql <- glue::glue_sql("ATTACH {duck_filename} AS {`alias`}
    (ENCRYPTED {parquet_encrypt}, DATA_PATH {parquet_directory});",
                               .con = temp_con)

  DBI::dbSendQuery(temp_con, attach_sql)

} else if(encrypt && !file.exists(filename)) {

  attach_sql <- glue::glue_sql("ATTACH {duck_filename} AS {`alias`}
    (ENCRYPTED {parquet_encrypt}, DATA_PATH {parquet_directory}, META_ENCRYPTION_KEY ?);",
                               .con = temp_con)

  DBI::dbSendQuery(temp_con, attach_sql, params = askpass::askpass())
}

  switch_default_duck(temp_con, alias)

  if(is.null(connection)) {
    detach_duck(temp_con, temp_duckdb)
  }


  invisible(temp_con)
}

make_temp_con <- function() {

  duckdb_temp <- tempfile(pattern = "duckdb_con", fileext = ".duckdb")
  DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_temp)

}

check_duckdb_connection <-  function(conduckdb) {
  stopifnot("Not a duckdb connection" = class(conduckdb) == "duckdb_connection")

}



switch_default_duck <- function(conduckdb, new_default) {

  check_duckdb_connection(conduckdb)

  pd <- DBI::SQL(new_default)

  use_sql <- glue::glue_sql("USE {`pd`};", .con = conduckdb)

  DBI::dbSendQuery(conduckdb, use_sql)

  default_duck(conduckdb)

}

#' Check the current default database on a duckdb connection
#'
#' @inheritParams attach_ducklake
#'
#' @returns the name of the default database. Use `switch_default_database()` to change the default database
#' @export

default_duck <- function(conduckdb) {

  check_duckdb_connection(conduckdb)

  cd <- as.character(DBI::dbGetQuery(conduckdb, "SELECT current_database();"))

  cd2 <- paste(cd, "is the default database")
  message(cd2)

  invisible(cd)

}

#' What databases are attached to the duckdb connection
#'
#' @inheritParams attach_ducklake
#'
#' @returns a dataframe with the attached database names
#' @export

attached_ducks <- function(conduckdb) {

  check_duckdb_connection(conduckdb)

  ad <- DBI::dbGetQuery(conduckdb, "SHOW databases;")

  ad <- as.data.frame(ad[!startsWith(ad$database_name, prefix = "__"), ])

  names(ad) <- "attached_ducks"

  return(ad)
}

#' Detach a ducklake from a duckdb connection
#'
#' @inheritParams attach_ducklake
#' @param new_default A new default database (default is memory).
#' Run `attached_databases()` for a list of currently attached databases
#'
#' @returns detaches and returns remaining attached databases
#' @export
#'
#' @inherit attach_ducklake examples

detach_duck <- function(conduckdb, alias) {

  check_duckdb_connection(conduckdb)

  da <- DBI::SQL(alias)

  detach_sql <- glue::glue_sql("DETACH {`da`};", .con = conduckdb)

  DBI::dbSendQuery(conduckdb, detach_sql)

  attached_ducks(conduckdb)

}


copy_table <- function(conduckdb, from, to) {

  to_sql <- DBI::SQL(to)

  from_sql <- DBI::SQL(from)

  copy_sql <- glue::glue_sql("CREATE TABLE {to_sql} AS FROM {from_sql};", .con = conduckdb)

  DBI::dbSendQuery(my_con, copy_sql)

  invisible()
}








#' Attach and manage duckdb databases / ducklakes
#'
#' @description
#' Attach to an existing, or create a new, duckdb database or ducklake (`attach_duck()`)
#'
#' Use `attached_ducks()` to see attached databases or lakes and `detach_duck()` to detach.
#'
#' Use `default_duck()` to check current default database / ducklake and `change_default_duck()` to change the default
#'
#' @param connection a duckdb connection. If NULL (the default), creates a connection in the temporary directory
#' @param filename the filename of an existing (or new) duckdb database  or ducklake (required)
#' @param alias an alias for the database or ducklake (required)
#' @param read_only default is FALSE
#' @param encrypt set to TRUE if the database or ducklake catalog is encrypted (or you want to encrypt if creating a new one), will ask for your password (key)
#' @param what Do you want to create or attach to a database or a ducklake
#' @param type For a ducklake do you want to use duckdb (the default) as catalog database or sqlite.
#' @param parquet_encrypt Only applies if creating a new ducklake, if TRUE parquet files that hold the data in the ducklake are encrypted
#' @param parquet_directory Only needs setting if creating a new ducklake. give a directory name for parquet files (will create directory if doesn't exist)
#' @param override_parquet_directory For an existing ducklake want to use an alternative parquet directory. Default is FALSE
#'
#' @rdname attach
#' @export
#'
#' @returns The duckdb connection with the database or ducklake attached. Should be assigned if connection is NULL.
#' @export
#'
#' @examples
#'
#' tempfilename <- tempfile(pattern = "example_duckdb", fileext = ".duckdb")
#'
#' my_con <- attach_duck(filename = tempfilename,
#'                 alias = "example1",
#'                 what = "database")
#'
#' tempfilename_2 <- tempfile(pattern = "example_duckdb", fileext = ".ducklake")
#'
#' attach_duck(connection = my_con,
#'                 filename = tempfilename_2,
#'                 alias = "example2",
#'                 what = "ducklake",
#'                 parquet_directory = "data_files")
#'
#' attached_ducks(my_con)
#'
#' default_duck(my_con)
#'
#' change_default_duck(my_con, "example1")
#'
#' detach_duck(my_con, "example2")
#'
#' attached_ducks(my_con)
#'
#' DBI::dbDisconnect(my_con)


attach_duck <- function(connection = NULL,
                        filename,
                        alias,
                        read_only = FALSE,
                        encrypt = FALSE,
                        what = "database",
                        type = "duckdb",
                        parquet_encrypt = FALSE,
                        parquet_directory = NULL,
                        override_parquet_directory = FALSE) {

  if(is.null(connection)) {

    temp_con <- make_temp_con()

    temp_duckdb <- unlist(strsplit(fs::path_file(temp_con@driver@dbdir), split = ".", fixed = TRUE))[1]

  } else{

    check_duckdb_connection(connection)
    temp_con <- connection
  }

  # install and load (if needed) extensions  - see utils for function
  install_load_ext(temp_con)

  # argument checks

  rlang::check_string(filename)
  rlang::check_string(alias)
  rlang::check_bool(read_only)
  rlang::check_bool(encrypt)
  rlang::check_bool(parquet_encrypt)
  rlang::arg_match(what, values = c("database", "ducklake"))
  rlang::arg_match(type, values = c("duckdb", "sqlite"))
  if(what == "ducklake" && type == "sqlite" && encrypt) {
    stop("If sqlite is the catalog, encryption of the database is not supported")
  }
  if(what == "ducklake" && !file.exists(filename) && is.null(parquet_directory)) {
    stop("If creating a new ducklake you must give a file storage directory in parquet_directory")
  }

  # generate attach code

  if(what == "ducklake" && type == "duckdb") {
    duck_filename <- DBI::dbQuoteLiteral(temp_con, paste0("ducklake:duckdb:", filename))
  } else if(what == "ducklake" && type == "sqlite")  {
    duck_filename <- DBI::dbQuoteLiteral(temp_con, paste0("ducklake:sqlite:", filename))
  } else {
    duck_filename <- filename
  }

  # read only

  ronly <- glue::glue_sql("READ_ONLY {read_only}", .con = temp_con)

  attach_sql_base <- glue::glue_sql("ATTACH {duck_filename} AS {`alias`} ({ronly}", .con = temp_con)

  # encrypt database

  if(encrypt && what == "database") {

    enc <- DBI::SQL("ENCRYPTION_KEY ?")

  } else if(encrypt && what == "ducklake") {

    enc <- DBI::SQL("META_ENCRYPTION_KEY ?")

  }

  # parquet options

  par_enc <- glue::glue_sql("ENCRYPTED {parquet_encrypt}", .con = temp_con)

  par_dir <- glue::glue_sql("DATA_PATH {parquet_directory}", .con = temp_con)

  par_over <- glue::glue_sql("OVERRIDE_DATA_PATH {override_parquet_directory}", .con = temp_con)

  # build

  # database final and ducklake base

  if(what == "database" && !encrypt) {

    attach_sql <- glue::glue_sql("{attach_sql_base});", .con = temp_con)
}
  else if (what == "database" && encrypt) {

    attach_sql <- glue::glue_sql("{attach_sql_base}, {enc});", .con = temp_con)

  }

  # finish ducklake


  if(what == "ducklake" && !file.exists(filename)) {

    attach_sql <- glue::glue_sql("{attach_sql_base}, {par_dir},  {par_enc}",
                                 .con = temp_con)
  }

  if(what == "ducklake" && file.exists(filename)) {

    attach_sql <- glue::glue_sql("{attach_sql_base}, {par_dir},  {par_over}",
                                 .con = temp_con)
    }


  if(what == "ducklake" && encrypt) {

    attach_sql <-  glue::glue_sql("{attach_sql}, {enc}",
                                  .con = temp_con)
  }

  if(what == "ducklake") {

    attach_sql <-  glue::glue_sql("{attach_sql});",
                                  .con = temp_con)
  }


  if(encrypt) {

    DBI::dbSendQuery(temp_con, attach_sql, params = askpass::askpass())

  } else {

    DBI::dbSendQuery(temp_con, attach_sql)
  }

  change_default_duck(temp_con, alias)

  if(is.null(connection)) {
    detach_duck(temp_con, temp_duckdb)
  }


  invisible(temp_con)
}





#' What databases are attached to the duckdb connection (`attached_ducks()`)
#'
#' @returns `attached_ducks()` returns a dataframe with the attached database and lake names
#' @rdname attach
#' @export

attached_ducks <- function(connection) {

  check_duckdb_connection(connection)

  ad <- DBI::dbGetQuery(connection, "SHOW databases;")

  ad <- as.data.frame(ad[!startsWith(ad$database_name, prefix = "__"), ])

  names(ad) <- "attached_ducks"

  return(ad)
}


#' @returns `detach_duck()` detaches and returns remaining attached databases or ducklakes
#' @rdname attach
#' @export

detach_duck <- function(connection, alias) {

  check_duckdb_connection(connection)

  da <- DBI::SQL(alias)

  detach_sql <- glue::glue_sql("DETACH {`da`};", .con = connection)

  DBI::dbSendQuery(connection, detach_sql)

  attached_ducks(connection)

}

#' @param new_default The alias of the new default
#'
#' @returns `change_default_duck()` Sets a database or ducklake as the new default
#'
#' @rdname attach
#'
#' @export


change_default_duck <- function(connection, new_default) {

  check_duckdb_connection(connection)

  pd <- DBI::SQL(new_default)

  use_sql <- glue::glue_sql("USE {`pd`};", .con = connection)

  DBI::dbSendQuery(connection, use_sql)

  default_duck(connection)

}

#' @returns `default_duck()` returns the name of the default database.
#' @rdname attach
#' @export

default_duck <- function(connection) {

  check_duckdb_connection(connection)

  cd <- as.character(DBI::dbGetQuery(connection, "SELECT current_database();"))

  cd2 <- paste(cd, "is the default database")
  message(cd2)

  invisible(cd)

}





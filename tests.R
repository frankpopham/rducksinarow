my_con <- attach_database(connection = NULL,
                filename = "test1.duckdb",
                alias = "test1",
                encrypt = TRUE,
                read_only = FALSE)

attach_ducklake(connection = my_con,
                type = "duckdb",
                filename = "test2.ducklake",
                alias = "test2",
                read_only = FALSE,
                encrypt = TRUE,
                parquet_encrypt = TRUE,
                parquet_directory = "data_lake_1")

attach_ducklake(connection = my_con,
                type = "duckdb",
                filename = "test3.ducklake",
                alias = "test3",
                read_only = FALSE,
                encrypt = FALSE,
                parquet_encrypt = TRUE,
                parquet_directory = "data_lake_2")

default_duck(my_con)

dbWriteTable(my_con,  "mtcars", mtcars)

tbl(my_con, Id(catalog = "test2", table = "mtcars")) %>%
  compute(Id("test3.mtcars2"), temporary = FALSE)


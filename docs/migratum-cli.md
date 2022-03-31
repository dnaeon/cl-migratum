# migratum

`migratum` -- migration tool

## Usage

``` shell
migratum [global-options] [<command>] [command-options] [arguments ...]
```

## Options

`migratum` accepts the following options:

``` shell
      --help                         display usage information and exit
      --version                      display version and exit
  -d, --driver <VARIANT>             migration driver to use [env: $MIGRATUM_DRIVER] [choices: dbi,
                                     rdbms-pgsql]
  -l, --log-level <VARIANT>          log level [default: info] [env: $MIGRATUM_LOG_LEVEL] [choices: off,
                                     info, debug]
  -p, --provider <VARIANT>           migration resource provider to use [env: $MIGRATUM_PROVIDER] [choices:
                                     local-path]

DBI driver options:
      --dbi-db-host <VALUE>          database host [default: localhost] [env: $MIGRATUM_DBI_DB_HOST]
      --dbi-db-kind <VARIANT>        database kind [env: $MIGRATUM_DBI_DB_KIND] [choices: sqlite3,
                                     postgres, mysql]
      --dbi-db-name <VALUE>          database name [env: $MIGRATUM_DBI_DB_NAME]
      --dbi-db-pass <VALUE>          database password [env: $MIGRATUM_DBI_DB_PASSWORD]
      --dbi-db-port <INT>            database port [env: $MIGRATUM_DBI_DB_PORT]
      --dbi-db-user <VALUE>          database username [env: $MIGRATUM_DBI_USER]

RDBMS-PGSQL driver options:
      --rdbms-pgsql-db-host <VALUE>  database host [default: localhost] [env: $PGHOST]
      --rdbms-pgsql-db-name <VALUE>  database name [env: $PGDATABASE]
      --rdbms-pgsql-db-pass <VALUE>  database password [env: $PGPASSWORD]
      --rdbms-pgsql-db-port <INT>    database port [default: 5432] [env: $PGPORT]
      --rdbms-pgsql-db-user <VALUE>  database username [env: $PGUSER]

local-path provider options:
      --resources <PATH>             migration resources path [env: $MIGRATUM_LP_RESOURCES]

```

## Sub Commands

`migratum` provides the following sub commands:

``` shell
  applied           list applied migrations
  create, new       create new migration
  migrate           apply pending migrations
  pending           list pending migrations
  print-doc         print the documentation
  reset             revert all migrations and re-apply them
  revert, rollback  revert latest applied migration(s)
  status            get status info
  zsh-completions   generate the Zsh completions script

```

## Authors

* Marin Atanasov Nikolov <dnaeon@gmail.com>

## License

BSD 2-Clause

# migratum applied

`migratum applied` -- list applied migrations

## Usage

``` shell
migratum [global-options] applied [options] [arguments ...]
```

## Options

`migratum applied` accepts the following options:

``` shell
      --help          display usage information and exit
      --limit <INT>   fetch this number of migrations at max [default: 100]
      --offset <INT>  fetch applied migrations at the given offset [default: 0]
      --version       display version and exit

```

# migratum create

`migratum create` -- create new migration

## Usage

``` shell
migratum [global-options] create [options] [arguments ...]
```

## Options

`migratum create` accepts the following options:

``` shell
      --description <VALUE>  description of the migration
      --help                 display usage information and exit
      --kind <VARIANT>       migration kind [choices: sql, lisp]
      --version              display version and exit

```

# migratum migrate

`migratum migrate` -- apply pending migrations

## Usage

``` shell
migratum [global-options] migrate [options] [arguments ...]
```

## Options

`migratum migrate` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# migratum pending

`migratum pending` -- list pending migrations

## Usage

``` shell
migratum [global-options] pending [options] [arguments ...]
```

## Options

`migratum pending` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# migratum print-doc

`migratum print-doc` -- print the documentation

## Usage

``` shell
migratum print-doc 
```

## Options

`migratum print-doc` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# migratum reset

`migratum reset` -- revert all migrations and re-apply them

## Usage

``` shell
migratum [global-options] reset [options] [arguments ...]
```

## Options

`migratum reset` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# migratum revert

`migratum revert` -- revert latest applied migration(s)

## Usage

``` shell
migratum [global-options] revert [options] [arguments ...]
```

## Options

`migratum revert` accepts the following options:

``` shell
      --count <INT>  number of migrations to revert [default: 1]
      --help         display usage information and exit
      --version      display version and exit

```

# migratum status

`migratum status` -- get status info

## Usage

``` shell
migratum [global-options] status [options] [arguments ...]
```

## Options

`migratum status` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# migratum zsh-completions

`migratum zsh-completions` -- generate the Zsh completions script

## Usage

``` shell
migratum zsh-completions 
```

## Options

`migratum zsh-completions` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```


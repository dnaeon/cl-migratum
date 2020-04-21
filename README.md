# Database Schema Migration System for Common Lisp

`cl-migratum` is a Common Lisp system, which provides facilities for
performing [database schema
migrations](https://en.wikipedia.org/wiki/Schema_migration).

## Requirements

* [Quicklisp](https://www.quicklisp.org/beta/)

## Installation

Clone the [cl-migratum](https://github.com/dnaeon/cl-migratum) repo in your
[Quicklisp local-projects directory](https://www.quicklisp.org/beta/faq.html).

``` shell
git clone https://github.com/dnaeon/cl-migratum.git
```

Load the system.

``` shell
CL-USER> (ql:quickload :cl-migratum)
```

## Concepts

The `cl-migratum` system uses the following concepts to describe
the process of discovering and applying schema migrations, so it is
important that you get familiar with them first.

### Migration

A `migration` represents a resource that provides information about a
schema change, e.g. it provides the unique id of the change, the
required scripts that can be used to upgrade and/or downgrade the
database.

Migration resources are discovered via `providers` and are being
used by `drivers` during the process of upgrade/downgrade of the
schema.

### Provider

The `provider` is responsible for discovering migration resources.

For example a provider can discover migrations from local path by
scanning files that match a given pattern or fetch migrations from
a remote endpoint (e.g. HTTP service).

The `provider` is also responsible for creating new migration
sequences.

The following providers are supported by `cl-migratum`.

| Name         | Description                                                      |
|--------------|------------------------------------------------------------------|
| `local-path` | Provider that can discover migration resources from a local path |

### Driver

The `driver` carries out the communication with the
database against which schema changes will be applied.

It is responsible for applying schema changes, registering
successfully applied migrations and unregistering them when
reverting back to a previous state.

A `driver` uses a `provider` in order to discover `migrations`, which
can be applied against the database it is connected to.

The following drivers are supported by `cl-migratum`.

| Name  | Description                                                                                                        |
|-------|--------------------------------------------------------------------------------------------------------------------|
| `sql` | Driver for performing schema migrations against a SQL database using [CL-DBI](https://github.com/fukamachi/cl-dbi) |

## Usage

The following section contains some examples to get you started.

### Create Provider

First, we will create a new `provider` that can discover migration files from a
local path. In order to create a local-path provider we will use the
`MIGRATUM:MAKE-LOCAL-PATH-PROVIDER` function.

``` common-lisp
CL-USER> (defparameter *provider*
           (migratum:make-local-path-provider #P"~/Projects/lisp/cl-migratum/migrations"))
*PROVIDER*
```

The `LOCAL-PATH-PROVIDER` discovers migration files which match the following pattern.

* `<id>-<description>.up.sql` - upgrade script
* `<id>-<description>.down.sql` - downgrade script

A provider can optionally be initialized, which can be done using the
`MIGRATUM:PROVIDER-INIT` generic function. Not all providers would
require initialization, but some will and therefore it is good that
always initialize them first.

In order to list the migrations provided by a `provider` you can use
the `MIGRATUM:PROVIDER-LIST-MIGRATIONS` generic function, e.g.

``` common-lisp
CL-USER> (migratum:provider-list-migrations *provider*)
(#<CL-MIGRATUM.PROVIDER.LOCAL-PATH:LOCAL-PATH-MIGRATION {100527E5D3}>
 #<CL-MIGRATUM.PROVIDER.LOCAL-PATH:LOCAL-PATH-MIGRATION {100527E673}>
 #<CL-MIGRATUM.PROVIDER.LOCAL-PATH:LOCAL-PATH-MIGRATION {100527E713}>)
```

The following generic functions can be used to interact with
discovered migrations.

* `MIGRATUM:MIGRATION-ID`
* `MIGRATUM:MIGRATION-DESCRIPTION`
* `MIGRATUM:MIGRATION-APPLIED`
* `MIGRATUM:MIGRATION-LOAD-UP-SCRIPT`
* `MIGRATUM:MIGRATION-LOAD-DOWN-SCRIPT`

For example in order to collect the unique IDs of migration resources you can
evaluate the following expression.

``` common-lisp
CL-USER> (mapcar #'migratum:migration-id
                 (migratum:provider-list-migrations *provider*))
(20200421180337 20200421173908 20200421173657)
```

### Create Driver

Once we have a driver for discovering migration resources we need to
create a driver, which can be used to communicate with the database
we want to apply migrations on.

Here is how we can create a `SQL` driver. The SQL driver
uses [CL-DBI](https://github.com/fukamachi/cl-dbi) interface
to communicate with the database, so we will first create a
database connection.

``` common-lisp
CL-USER> (defparameter *conn*
           (dbi:connect :sqlite3 :database-name "/Users/mnikolov/cl-migratum.db"))
*CONN*
```

And now we can instantiate our SQL driver using the previously created
provider and connection.

``` common-lisp
CL-USER> (defparameter *driver*
           (migratum:make-sql-driver *provider* *conn*))
*DRIVER*
```

### Initialize Driver

Each driver may require some initialization steps to be executed
before being able to apply migrations, so make sure that you
initialize a driver once you create it.

An example requirement for a driver might be to create some required
database schema used to track which migrations have been applied
already, so lets initialize our driver first.

``` common-lisp
CL-USER> (migratum:driver-init *driver*)
```

## Displaying Pending Migrations

In order to get the list of pending (not applied yet) migrations we can
use the `MIGRATUM:LIST-PENDING` function, e.g.

``` common-lisp
CL-USER> (migratum:list-pending *driver*)
(#<CL-MIGRATUM.PROVIDER.LOCAL-PATH:LOCAL-PATH-MIGRATION {10052A0583}>
 #<CL-MIGRATUM.PROVIDER.LOCAL-PATH:LOCAL-PATH-MIGRATION {10052A0623}>
 #<CL-MIGRATUM.PROVIDER.LOCAL-PATH:LOCAL-PATH-MIGRATION {10052A06C3}>)
```

Or, we can display a table of the pending migrations using the
`MIGRATUM:DISPLAY-PENDING` function instead.

``` common-lisp
CL-USER> (migratum:display-pending *driver*)
.-----------------------------------.
|        PENDING MIGRATIONS         |
+----------------+------------------+
| ID             | DESCRIPTION      |
+----------------+------------------+
| 20200421173657 | create-table-foo |
| 20200421173908 | create-table-bar |
| 20200421180337 | create-table-qux |
+----------------+------------------+
| TOTAL          |                3 |
+----------------+------------------+
NIL
```

The migrations will be sorted in the order they need to be applied.

## Applying Pending Migrations

The following functions are used for applying pending migrations.

* `MIGRATUM:APPLY-PENDING` - applies all pending migrations
* `MIGRATUM:APPLY-NEXT` - applies the next pending migration

This is how we can apply all pending migrations for example.

``` common-lisp
CL-USER> (migratum:apply-pending *driver*)
 <INFO> [18:10:14] cl-migratum.core core.lisp (apply-pending) -
  Found 3 pending migration(s) to be applied
 <INFO> [18:10:14] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421173657 - create-table-foo
 <INFO> [18:10:14] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421173908 - create-table-bar
 <INFO> [18:10:14] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421180337 - create-table-qux
NIL
```

## Displaying Applied Migrations

The following functions can be used to get and display the
list of applied database migrations.

* `MIGRATUM:DRIVER-LIST-APPLIED`
* `MIGRATUM:DISPLAY-APPLIED`

This is how we can get the list of applied migrations.

``` common-lisp
CL-USER> (migratum:driver-list-applied *driver*)
(#<CL-MIGRATUM.CORE:MIGRATION {1006095B23}>
 #<CL-MIGRATUM.CORE:MIGRATION {1006095B73}>
 #<CL-MIGRATUM.CORE:MIGRATION {1006095BC3}>)
```

Or we can display a nice table of the applied migrations instead.

``` common-lisp
CL-USER> (migratum:display-applied *driver*)
.---------------------------------------------------------.
|                   APPLIED MIGRATIONS                    |
+----------------+------------------+---------------------+
| ID             | DESCRIPTION      | APPLIED             |
+----------------+------------------+---------------------+
| 20200421180337 | create-table-qux | 2020-04-21 15:17:46 |
| 20200421173908 | create-table-bar | 2020-04-21 15:14:13 |
| 20200421173657 | create-table-foo | 2020-04-21 15:12:52 |
+----------------+------------------+---------------------+
|                | TOTAL            |                   3 |
+----------------+------------------+---------------------+
NIL
```

The output of these functions will be the applied migrations in
descending order by their id, first one being the most recent one.

## Finding Migration by ID

TODO: Document me

## Stepping through migrations

TODO: Document me

 - apply-next
 - revert-last

## Implemeting MIGRATION resources

TODO: Document me

### MIGRATION-LOAD-UP-SCRIPT

TODO: Document me

### MIGRATION-LOAD-DOWN-SCRIPT

TODO: Document me

## Implementing PROVIDERs

TODO: Document me

### PROVIDER-INIT

TODO: Document me

### PROVIDER-LIST-MIGRATIONS

TODO: Document me

### PROVIDER-CREATE-MIGRATION

TODO: Document me

## Implementing DRIVERs

TODO: Document me

### DRIVER-INIT

TODO: Document me

### DRIVER-LIST-APPLIED

TODO: Document me

### DRIVER-APPLY-UP-MIGRATION

TODO: Document me

### DRIVER-APPLY-DOWN-MIGRATION

TODO: Document me

### DRIVER-REGISTER-MIGRATION

TODO: Document me

### DRIVER-UNREGISTER-MIGRATION

TODO: Document me

## Contributing

`cl-migratum` is hosted on
[Github](https://github.com/dnaeon/cl-migratum). Please contribute by
reporting issues, suggesting features or by sending patches using pull
requests.

## Authors

* Marin Atanasov Nikolov <dnaeon@gmail.com>

## License

[BSD License](http://opensource.org/licenses/BSD-2-Clause).

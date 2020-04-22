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
           (dbi:connect :sqlite3 :database-name "/Users/dnaeon/cl-migratum.db"))
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

### Pending Migrations

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

### Applying Migrations

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

### Get Latest Migration

You can use the following function to get the latest applied
migration.

* `MIGRATUM:LATEST-MIGRATION`

The following expression will return the latest migration id.

``` common-lisp
CL-USER> (migratum:migration-id (migratum:latest-migration *driver*))
20200421180337
```

The following predicate can be used to query whether there are any
migrations applied.

* `MIGRATUM:CONTAINS-APPLIED-MIGRATIONS-P`

For example:

``` common-lisp
CL-USER> (migratum:contains-applied-migrations-p *driver*)
T
```

### Displaying Applied Migrations

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

### Stepping through migrations

Using the following functions you can step through migrations.

* `MIGRATUM:APPLY-NEXT`
* `MIGRATUM:REVERT-LAST`

This is useful in situations when you don't want to apply
all migrations at once, but rather do it one at a time. Both of these
functions accept a keyword parameter `:count` which specifies the
number of migrations to apply or revert.

Consider the following pending migrations.

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

We can apply them one by one and verify them as we go.

``` common-lisp
CL-USER> (migratum:apply-next *driver*)
 <INFO> [20:04:25] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421173657 - create-table-foo
NIL
CL-USER> (migratum:apply-next *driver*)
 <INFO> [20:04:28] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421173908 - create-table-bar
NIL
CL-USER> (migratum:apply-next *driver*)
 <INFO> [20:04:29] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421180337 - create-table-qux
NIL
```

If we want to revert the last three migrations we can use the
`MIGRATUM:REVERT-LAST` function, e.g.

``` common-lisp
CL-USER> (migratum:revert-last *driver* :count 3)
 <INFO> [20:06:00] cl-migratum.core core.lisp (revert-and-unregister) -
  Reverting migration 20200421180337 - create-table-qux
 <INFO> [20:06:00] cl-migratum.core core.lisp (revert-and-unregister) -
  Reverting migration 20200421173908 - create-table-bar
 <INFO> [20:06:00] cl-migratum.core core.lisp (revert-and-unregister) -
  Reverting migration 20200421173657 - create-table-foo
NIL
```

### Creating New Migrations

The following generic function creates a new migration
sequence, when supported by the provider that you are using.

* `MIGRATUM:PROVIDER-CREATE-MIGRATION`

Here's one example of creating a new migration, which also specifies
the upgrade and downgrade scripts as part of the keyword parameters.

``` common-lisp
CL-USER> (defparameter *migration*
           (migratum:provider-create-migration *provider*
                                               :description "create-table-fubar"
                                               :up "CREATE TABLE fubar (id INTEGER PRIMARY KEY);"
                                               :down "DROP TABLE fubar;"))
*MIGRATION*
```

We can also inspect the newly created migration, e.g.

``` common-lisp
CL-USER> (migratum:migration-id *migration*)
20200421201406
CL-USER> (migratum:migration-description *migration*)
"create-table-fubar"
CL-USER> (migratum:migration-load-up-script *migration*)
"CREATE TABLE fubar (id INTEGER PRIMARY KEY);"
CL-USER> (migratum:migration-load-down-script *migration*)
"DROP TABLE fubar;"
```

## Implemeting new migration resources

Generally new migration resources will be implemented along with a
`provider`, which discovers them.

You can implement a new migration resource by subclassing the
`MIGRATUM:BASE-MIGRATION` class.

The following generic functions should be implemented on the newly
defined class, which need to return the upgrade and downgrade
scripts respectively for executing a migration.

* `MIGRATUM:MIGRATION-LOAD-UP-SCRIPT`
* `MIGRATUM:MIGRATION-LOAD-DOWN-SCRIPT`

The following generic functions can be overriden, if needed.

* `MIGRATUM:MIGRATION-ID`
* `MIGRATUM:MIGRATION-DESCRIPTION`
* `MIGRATUM:MIGRATION-APPLIED`

Example implementation that loads migration resources from a remote
HTTP server might look like this. The following code uses
[Dexador](https://github.com/fukamachi/dexador) as the HTTP client.

``` common-lisp
(defun http-get (url)
  "HTTP GETs the given URL"
  (let ((contents (dex:get url :force-string t)))
    contents))

(defclass http-migration (base-migration)
  ((up-script-url
    :initarg :up-script-url
    :initform (error "Must specify URL to upgrade script")
    :accessor http-migration-up-script-url
    :documentation "URL to the upgrade script")
   (down-script-url
    :initarg :down-script-url
    :initform (error "Must specify URL to downgrade script")
    :accessor http-migration-down-script-url
    :documentation "URL to the downgrade script"))
  (:documentation "HTTP migration resource"))

(defmethod migration-load-up-script ((migration http-migration) &key)
  (http-get (http-migration-up-script-url migration)))

(defmethod migration-load-down-script ((migration http-migration) &key)
  (http-get (http-migration-down-script-url migration)))
```

A provider should simply `MAKE-INSTANCE` of the `HTTP-MIGRATION` class by
providing values for the required slots while discovering migrations via
the `PROVIDER-LIST-MIGRATIONS` function.

## Implementing new providers

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

## Tests

TODO: Document me

## Contributing

`cl-migratum` is hosted on
[Github](https://github.com/dnaeon/cl-migratum). Please contribute by
reporting issues, suggesting features or by sending patches using pull
requests.

## Authors

* Marin Atanasov Nikolov (dnaeon@gmail.com)

## License

This project is Open Source and licensed under the [BSD
License](http://opensource.org/licenses/BSD-2-Clause).

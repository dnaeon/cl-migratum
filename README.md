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

| Name         | Description                                                      | System                            |
|--------------|------------------------------------------------------------------|-----------------------------------|
| `local-path` | Provider that can discover migration resources from a local path | `cl-migratum.provider.local-path` |

### Driver

The `driver` carries out the communication with the
database against which schema changes will be applied.

It is responsible for applying schema changes, registering
successfully applied migrations and unregistering them when
reverting back to a previous state.

A `driver` uses a `provider` in order to discover `migrations`, which
can be applied against the database it is connected to.

The following drivers are supported by `cl-migratum`.

| Name  | Description                                                                                                        | System                   |
|-------|--------------------------------------------------------------------------------------------------------------------|--------------------------|
| `dbi` | Driver for performing schema migrations against a SQL database using [CL-DBI](https://github.com/fukamachi/cl-dbi) | `cl-migratum.driver.dbi` |
| `rdbms-postgresql` | Driver for performing schema migrations against a SQL database using [hu.dwim.rdbms](http://dwim.hu/darcs/hu.dwim.rdbms/) | `cl-migratum.driver.rdbms-postgresql` |

## Usage

The following section contains some examples to get you started.

### Create Provider

First, we will create a new `provider` that can discover migration files from a
local path. In order to create a `local-path` provider we need to load the
system for the respective provider.

``` common-lisp
CL-USER> (ql:quickload :cl-migratum.provider.local-path)
```

Once you load the system we can create a `local-path` provider. The
`local-path` provider can discover migrations from multiple paths.

Typical example where having multiple paths with migration resources
might be useful is when you have a set of base migrations you want to
apply to all environments, and then have a separate path for each
environment with their own migrations. For example you might want to
run specific migrations only in your `dev` environment, but don't want
them in your `production` environment.

In that case it makes sense to separate the migration resources in
multiple paths, for each environment respectively.

``` common-lisp
CL-USER> (defparameter *provider*
           (migratum.provider.local-path:make-local-path-provider (list #P"~/Projects/lisp/cl-migratum/t/migrations/")))
*PROVIDER*
```

The `LOCAL-PATH-PROVIDER` discovers migration files which match the following pattern.

| Pattern                       | Description      |
|-------------------------------|------------------|
| `<id>-<description>.up.sql`   | Upgrade script   |
| `<id>-<description>.down.sql` | Downgrade script |

A provider can optionally be initialized, which can be done using the
`MIGRATUM:PROVIDER-INIT` generic function. Not all providers would
require initialization, but some will and therefore it is good that
you always initialize them first.

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

| Method                                | Description                                             |
|---------------------------------------|---------------------------------------------------------|
| `MIGRATUM:MIGRATION-ID`               | Returns the unique migration id                         |
| `MIGRATUM:MIGRATION-DESCRIPTION`      | Returns the description of the migration                |
| `MIGRATUM:MIGRATION-APPLIED`          | Returns the timestamp of when the migration was applied |
| `MIGRATUM:MIGRATION-LOAD-UP-SCRIPT`   | Returns the upgrade script of the migration             |
| `MIGRATUM:MIGRATION-LOAD-DOWN-SCRIPT` | Returns the downgrade script of the migration           |

For example in order to collect the unique IDs of migration resources you can
evaluate the following expression.

``` common-lisp
CL-USER> (mapcar #'migratum:migration-id
                 (migratum:provider-list-migrations *provider*))
(20200421180337 20200421173908 20200421173657)
```

### Create Driver

Once we have a provider for discovering migration resources we need to
create a driver, which can be used to communicate with the database
we want to apply migrations on.

#### DBI Driver

Here is how we can create a `dbi` driver. First, lets load the system,
which provides that driver.

``` common-lisp
CL-USER> (ql:quickload :cl-migratum.driver.dbi)
```

The `dbi` driver uses [CL-DBI](https://github.com/fukamachi/cl-dbi)
interface to communicate with the database, so we will need to create
a database connection.

``` common-lisp
CL-USER> (defparameter *conn*
           (dbi:connect :sqlite3 :database-name "/Users/dnaeon/cl-migratum.db"))
*CONN*
```

And now we can instantiate our SQL driver using the previously created
provider and connection.

``` common-lisp
CL-USER> (defparameter *driver*
           (migratum.driver.dbi:make-driver *provider* *conn*))
*DRIVER*
```

#### RDBMS PostgreSQL Driver

The process to create an `rdbms-postgresql` driver is analogous.  The
driver needs a connection specification to create a DB connection. In
this example we use the same environment variables which are used by
the PostgreSQL client `psql` to override the defaults:

``` common-lisp
(ql:quickload :cl-migratum.driver.rdbms-postgresql)
(defparameter *driver*
  (migratum.driver.rdbms-postgresql:make-driver
   *provider*
   `(:host ,(or (uiop:getenv "PGHOST") "localhost")
     :database ,(or (uiop:getenv "PGDATABASE") "migratum")
     :user-name ,(or (uiop:getenv "PGUSER") "migratum")
     :password ,(or (uiop:getenv "PGPASSWORD") "tiger"))))
```

### Initialization

A `driver` and `provider` may require some initialization steps to be
executed before being able to apply migrations, so make sure that you
initialize them.

In order to initialize your `provider` use the
`MIGRATUM:PROVIDER-INIT` function.

``` common-lisp
CL-USER> (migratum:provider-init *provider*)
```

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
| 20200421173657 | create_table_foo |
| 20200421173908 | create_table_bar |
| 20200421180337 | create_table_qux |
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
  Applying migration 20200421173657 - create_table_foo
 <INFO> [18:10:14] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421173908 - create_table_bar
 <INFO> [18:10:14] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421180337 - create_table_qux
NIL
```

### Get Latest Migration

You can use the `MIGRATUM:LATEST-MIGRATION` function to get the latest
applied migration, e.g.

``` common-lisp
CL-USER> (migratum:migration-id (migratum:latest-migration *driver*))
20200421180337
```

The `MIGRATUM:CONTAINS-APPLIED-MIGRATIONS-P` predicate can be used to
query whether there are any migrations applied, e.g.

``` common-lisp
CL-USER> (migratum:contains-applied-migrations-p *driver*)
T
```

### Displaying Applied Migrations

The following functions can be used to get and display the
list of applied database migrations.

| Function                       | Description                            |
|--------------------------------|----------------------------------------|
| `MIGRATUM:DRIVER-LIST-APPLIED` | Returns the list of applied migrations |
| `MIGRATUM:DISPLAY-APPLIED`     | Display a table of applied migrations  |

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
| 20200421180337 | create_table_qux | 2020-04-21 15:17:46 |
| 20200421173908 | create_table_bar | 2020-04-21 15:14:13 |
| 20200421173657 | create_table_foo | 2020-04-21 15:12:52 |
+----------------+------------------+---------------------+
|                | TOTAL            |                   3 |
+----------------+------------------+---------------------+
NIL
```

The output of these functions will be the applied migrations in
descending order by their id, first one being the most recent one.

The `dbi` and `rdbms-postgresql` drivers by default will fetch the
last 100 applied migrations. You can control this behaviour by using
the `:offset` and `:limit` keyword parameters, which allows you to
fetch applied migrations in pages.

For example, if you are interested only in the last ten applied
migrations you can evaluate the following expression.

``` common-lisp
CL-USER> (migratum:display-applied *driver* :limit 10)
```

Or if you want to skip the first ten migrations, you can evaluate
this expression instead.

``` common-lisp
CL-USER> (migratum.display-applied *driver* :offset 10 :limit 10)
```

### Stepping through migrations

Using the following functions you can step through migrations.

| Function               | Description                          |
|------------------------|--------------------------------------|
| `MIGRATUM:APPLY-NEXT`  | Apply the next pending migration(s)  |
| `MIGRATUM:REVERT-LAST` | Revert the last applied migration(s) |

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
| 20200421173657 | create_table_foo |
| 20200421173908 | create_table_bar |
| 20200421180337 | create_table_qux |
+----------------+------------------+
| TOTAL          |                3 |
+----------------+------------------+
NIL
```

We can apply them one by one and verify them as we go.

``` common-lisp
CL-USER> (migratum:apply-next *driver*)
 <INFO> [20:04:25] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421173657 - create_table_foo
NIL
CL-USER> (migratum:apply-next *driver*)
 <INFO> [20:04:28] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421173908 - create_table_bar
NIL
CL-USER> (migratum:apply-next *driver*)
 <INFO> [20:04:29] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421180337 - create_table_qux
NIL
```

If we want to revert the last three migrations we can use the
`MIGRATUM:REVERT-LAST` function, e.g.

``` common-lisp
CL-USER> (migratum:revert-last *driver* :count 3)
 <INFO> [20:06:00] cl-migratum.core core.lisp (revert-and-unregister) -
  Reverting migration 20200421180337 - create_table_qux
 <INFO> [20:06:00] cl-migratum.core core.lisp (revert-and-unregister) -
  Reverting migration 20200421173908 - create_table_bar
 <INFO> [20:06:00] cl-migratum.core core.lisp (revert-and-unregister) -
  Reverting migration 20200421173657 - create_table_foo
NIL
```

### Creating New Migrations

The `MIGRATUM:PROVIDER-CREATE-MIGRATION` generic function creates a
new migration sequence, when supported by the provider that you are
using.

Here's one example of creating a new migration, which also specifies
the upgrade and downgrade scripts as part of the keyword parameters.

``` common-lisp
CL-USER> (defparameter *migration*
           (migratum:provider-create-migration *provider*
                                               :description "create table fubar"
                                               :up "CREATE TABLE fubar (id INTEGER PRIMARY KEY);"
                                               :down "DROP TABLE fubar;"))
*MIGRATION*
```

We can also inspect the newly created migration, e.g.

``` common-lisp
CL-USER> (migratum:migration-id *migration*)
20200421201406
CL-USER> (migratum:migration-description *migration*)
"create_table_fubar"
CL-USER> (migratum:migration-load-up-script *migration*)
"CREATE TABLE fubar (id INTEGER PRIMARY KEY);"
CL-USER> (migratum:migration-load-down-script *migration*)
"DROP TABLE fubar;"
```

### Multiple Statements

If you need to run multiple statements when using the `dbi` or
`rdbms-postgresql` driver you can separate each statement in the
migration using the `--;;` separator.

The following example migration would create two tables as part of a
single transaction.

``` sql
CREATE TABLE foo (
    id INTEGER PRIMARY KEY
);
--;;
CREATE TABLE bar (
    id INTEGER PRIMARY KEY
);
```

### Debug logging

`cl-migratum` uses [log4cl](https://github.com/sharplispers/log4cl),
so you can enable debug logging, if needed.

``` common-lisp
CL-USER> (log:config :debug)
```

### Shutdown / Teardown

Once done with the migrations you should call the cleanup functions of
the `provider` and `driver`.

``` common-lisp
CL-USER> (migratum:provider-shutdown *provider*)
CL-USER> (migratum:driver-shutdown *driver*)
```

## Implemeting new migration resources

Generally new migration resources will be implemented along with a
`provider`, which discovers them.

You can implement a new migration resource by subclassing the
`MIGRATUM:BASE-MIGRATION` class.

The following generic functions should be implemented on the newly
defined class.

| Method                                | Description                  |
|---------------------------------------|------------------------------|
| `MIGRATUM:MIGRATION-LOAD-UP-SCRIPT`   | Returns the upgrade script   |
| `MIGRATUM:MIGRATION-LOAD-DOWN-SCRIPT` | Returns the downgrade script |

The following generic functions can be overriden, if needed.

| Method                           | Description                                         |
|----------------------------------|-----------------------------------------------------|
| `MIGRATUM:MIGRATION-ID`          | Returns the unique migration id                     |
| `MIGRATUM:MIGRATION-DESCRIPTION` | Returns description of the migration                |
| `MIGRATUM:MIGRATION-APPLIED`     | Returns timestamp of when the migration was applied |

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

You can implement custom `providers`, which can discover migration
resources from various sources, e.g. local path, remote HTTP endpoints,
etc.

Each `provider` determines the rules, which identify a resource as a
valid migration, so custom logic for discovering them can be
implemented by using your own provider. For example the `local-path`
provider considers files to be valid migrations, if they match a given
regex pattern.

In order to create a new provider you can subclass the
`MIGRATUM:BASE-PROVIDER` class and implement the following generic
functions on your newly defined class.

| Method                               | Description                                |
|--------------------------------------|--------------------------------------------|
| `MIGRATUM:PROVIDER-LIST-MIGRATIONS`  | Responsible for discovering migrations     |
| `MIGRATUM:PROVIDER-CREATE-MIGRATION` | Creates a new migration using the provider |

The following methods can be overriden, if needed.

| Method                          | Description                                                  |
|---------------------------------|--------------------------------------------------------------|
| `MIGRATUM:PROVIDER-NAME`        | Returns a human-friendly name of the provider                |
| `MIGRATUM:PROVIDER-INIT`        | Initializes the provider, if needed                          |
| `MIGRATUM:PROVIDER-INITIALIZED` | Returns `T` if provider is initialized, `NIL` otherwise      |
| `MIGRATUM:PROVIDER-SHUTDOWN`    | Shutdowns the provider and cleans up any allocated resources |

You can also check the [local-path
provider](./src/provider/local-path.lisp) implementation for some
example code.

## Implementing new drivers

A `driver` is responsible for communicating with the database we are
migrating and actually executing the upgrade and downgrade scripts.

The driver also takes care of registering applied migrations after
applying an upgrade script and also unregistering them during
downgrade, thus it is the drivers' decision how to implement
registering and unregistering. For example the `dbi` builtin driver
registers applied migrations on the same database it is migrating, but
a custom driver could choose a different stategy instead, e.g. use a
key/value store, local files, or some remote endpoint instead.

New drivers can be implemented by subclassing the
`MIGRATUM:BASE-DRIVER` class. The following methods should be
implemented on drivers.

| Method                                 | Description                                |
|----------------------------------------|--------------------------------------------|
| `MIGRATUM:DRIVER-LIST-APPLIED`         | Returns the list of applied migrations     |
| `MIGRATUM:DRIVER-REGISTER-MIGRATION`   | Registers a successfully applied migration |
| `MIGRATUM:DRIVER-UNREGISTER-MIGRATION` | Unregisters previously applied migration   |
| `MIGRATUM:DRIVER-APPLY-UP-MIGRATION`   | Executes the upgrade script                |
| `MIGRATUM:DRIVER-APPLY-DOWN-MIGRATION` | Executes the downgrade script              |

The following methods can be overriden, if needed.

| Method                        | Description                                                |
|-------------------------------|------------------------------------------------------------|
| `MIGRATUM:DRIVER-INIT`        | Initializes the driver, if needed                          |
| `MIGRATUM:DRIVER-NAME`        | Returns the human-friendly name of the driver              |
| `MIGRATUM:DRIVER-PROVIDER`    | Returns the `provider` used by the `driver`                |
| `MIGRATUM:DRIVER-INITIALIZED` | Returns `T` if driver is initialized, `NIL` otherwise      |
| `MIGRATUM:DRIVER-SHUTDOWN`    | Shutdowns the driver and cleans up any allocated resources |

You can check the [dbi driver](./src/driver/dbi.lisp) implementation
for some example code.

## Tests

Tests are provided as part of the `cl-migratum.test` system.

In order to run the tests you can evaluate the following expressions.

``` common-lisp
CL-USER> (ql:quickload :cl-migratum.test)
CL-USER> (asdf:test-system :cl-migratum.test)
```

Or you can run the tests in a Docker container instead.

First, build the Docker image.

``` shell
docker build -t cl-migratum .
```

Run the tests.

``` shell
docker run --rm cl-migratum
```

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

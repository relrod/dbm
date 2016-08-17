# dbm

A **simple** database migration tool.

## Philosophy

The idea here is to create the simplest, stupidest, dumbest thing that could
possibly work.

`dbm` is made to sit outside of other projects - it's not tied to any framework
and although it's written in Haskell it can certainly be used with projects in
other environments.

To use it, simply create a `sql/` directory in your project's root directory,
with your migrations. Name the migrations `###-some-short-description.sql`. e.g.
`001.initial-schema.sql`.

Then create a file, `sql/.dbm`, that describes how to access your database in
certain environments.

For example

```ini
[development]
backend = sqlite
path = /tmp/inventory.sql
```

That's it! Now you can run `dbm initialize <your environment>` which will add a
simple table to your database to keep track of migration data. See `dbm -h` for
a full list of commands, but briefly:

* `dbm initialize <env>` will create the `dbm_migrations` table in your
  database for keeping track of migration status.
* `dbm status <env>` will show you which migrations need to be applied, if any.
* `dbm migrate <env>` will migrate your database to the latest migration.

## What works, what doesn't?

* There is no notation of "undoing" a migration. This is by design to keep
  things simple, although if it is something you need, open an issue. :)
* Right now, only SQLite is supported, but it should be easy to add other
  backends. We use the `$database-simple` Hackage packages, so in theory adding
  at least MySQL and PostgreSQL should be near-trivial.

## License

BSD-3, see `LICENSE`.

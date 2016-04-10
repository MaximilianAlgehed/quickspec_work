* Use the Lazy SmallCheck stuff
* To the Lazy SmallCheck `Result` type add a `toTypeEncoding` field
* Use the same strategy that they use to fill out their printing field to fill out the `toTypeEncoding` field
* When it finds `known False` return an empty list
* When it finds `known True` return a list of type encodings
* The `TypeEncoding` type needs to be able to handle true properties which are also unrefined for some field, probably by resorting to `arbitrary` values

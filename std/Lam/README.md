# `std.Lam`

These modules exist basically to break import cycles. They are required by one
or more foundational modules, and provide just enough to support them. You
typically want to import the `Data.` equivalents instead, as they have the full
set of functions.

For example, `Data.Bool` defines an `Eq` instance for `Bool`, but `Data.Eq`
depends on `Bool` itself (since `eq : a -> a -> Bool`). Thus `Data.Bool` imports
`Data.Eq` and `Data.Eq` imports `Lam.Bool`.

Currently this is a bit rubbish, because we don't support re-exports. Therefore
to use the Bool type you need to import `Lam.Bool` but to use most of the
functions on it you need to import `Data.Bool`. I'm leaving this structure in
place because it will become less rubbish once we improve imports and exports.

let kite = ../kite.dhall
let dep = kite.dependency
let version = kite.version
let eq = version.eq

in
  {
      name = "std"
    , dependencies = [
        dep::{ name = "kite", version = eq { major = 1, minor = 0, patch = 0 } }
      ]
  }

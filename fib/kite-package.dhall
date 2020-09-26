let VersionNum = { major : Natural, minor : Natural, patch : Natural }
let VersionSpec : Type = < Equal : VersionNum | AtLeast : VersionNum >

in {
    name = "fib"
  , dependencies = [{ name = "kite", version = VersionSpec.Equal { major = 1, minor = 0, patch = 0 } }]
  }

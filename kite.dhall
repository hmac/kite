-- Kite package specification

let VersionNum = { major : Natural, minor : Natural, patch : Natural }

let VersionSpec
    : Type
    = < eq : VersionNum | min : VersionNum >

let Source = < remote : { url : Text } | local : { path : Text } >

let defaultSource = Source.remote { url = "https://packages.kite-lang.org" }

let Dependency =
      { Type = { name : Text, version : VersionSpec, source : Source }
      , default.source = defaultSource
      }

let LockedDependency = { name : Text, version : VersionNum, source : Source }

let Hash = < sha256 : Text >

let File = { path : Text, hash : Hash }

let Package =
      { name : Text
      , version : VersionNum
      , hash : Hash
      , files : List File
      , dependencies : List Dependency
      }

in  { version = VersionSpec
    , dependency = Dependency
    , locked_dependency = LockedDependency
    , package = Package
    , hash = Hash
    }

let release = "%%VERSION_NUM%%"

let goblint =
  if BatString.starts_with release "%" then
    ConfigVersion.version
  else
    release

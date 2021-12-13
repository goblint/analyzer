let release = "%%VERSION_NUM%%"

let goblint =
  let commit = ConfigVersion.version in
  if BatString.starts_with release "%" then
    commit
  else
    Format.sprintf "%s (%s)" release commit

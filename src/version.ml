let release = "%%VERSION_NUM%%"
let release_commit = "%%VCS_COMMIT_ID%%"

let goblint =
  let commit = ConfigVersion.version in
  if BatString.starts_with release "%" then
    commit
  else (
    let commit =
      if commit = "n/a" then (* released archive has no .git *)
        release_commit
      else
        commit
    in
    Format.sprintf "%s (%s)" release commit
  )

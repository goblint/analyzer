(** Goblint build info. *)

(** OCaml compiler flambda status. *)
let ocaml_flambda = ConfigOcaml.flambda

(** Dune profile. *)
let dune_profile = ConfigProfile.profile

(** Goblint version from git. *)
let git_version = ConfigVersion.version

(** Goblint version from release archive. *)
let release_version = "%%VERSION_NUM%%"

(** Goblint git commit from release archive. *)
let release_commit = "%%VCS_COMMIT_ID%%"

(** Goblint version. *)
let version =
  let commit = ConfigVersion.version in
  if BatString.starts_with release_version "%" then
    commit
  else (
    let commit =
      if commit = "n/a" then (* released archive has no .git *)
        release_commit
      else
        commit
    in
    Format.sprintf "%s (%s)" release_version commit
  )

(** Statically linked libraries with versions. *)
let statically_linked_libraries = Dune_build_info.statically_linked_libraries

(** Build date and time. *)
let datetime = ConfigDatetime.datetime

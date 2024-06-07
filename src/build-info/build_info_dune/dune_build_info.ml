let statically_linked_libraries =
  List.map (fun lib ->
      let name = Build_info.V1.Statically_linked_library.name lib in
      let version = Option.map Build_info.V1.Version.to_string (Build_info.V1.Statically_linked_library.version lib) in
      (name, version)
    ) (Build_info.V1.Statically_linked_libraries.to_list ())

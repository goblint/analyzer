let () =
  JsonSchema2.convert_schema !GobConfig.json_conf @@ List.map (fun (c, (n, p)) -> (n, (c, p))) !Defaults.registrar

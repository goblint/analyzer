# Configuring

On top of passing options via the command line, Goblint can be configured with `json` files following the schema outlined in [the Options reference](../../jsfh/options.schema.html) (also on the sidebar)
or using one of the default configurations we provide.

# Example Configurations for Goblint

The repository contains some example configurations for the Goblint analyzer in the folder `conf/examples`. If Goblint is installed these can
be accessed without the conf prefix, e.g. with `./goblint --conf examples/large-programs.json`.

- `very-precise.json`: Enables some of the more expensive abstract domains and features, especially useful for smaller programs
- `medium-program.json`: Enables some costly features, but staying away from the very expensive ones. This is very close to, e.g. the configuration we use for SV-COMP
- `large-programs.json`: Minimal configuration for larger programs, should run fast even for large programs, but usually needs to be made more precise by adding further features.

### JSON schema

#### VSCode
In `.vscode/settings.json` add the following:
```json
{
    "json.schemas": [
        {
            "fileMatch": [
                "/conf/*.json",
                "/tests/incremental/*/*.json"
            ],
            "url": "/src/util/options.schema.json"
        }
    ]
}
```

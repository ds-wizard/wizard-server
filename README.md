# Wizard Server

[![User Guide](https://img.shields.io/badge/docs-User%20Guide-informational)](https://guide.ds-wizard.org)
[![License](https://img.shields.io/github/license/ds-wizard/wizard-server)](LICENSE.md)

*Wizard Server is the core application of the Data Stewardship Wizard: knowledge models,
projects, document generation, user and tenant management and real-time collaboration.*

This repository is a read-only distribution: the sources are generated from the development
repository and pushed here, so pull requests cannot be merged directly. Discuss changes in an
issue first, see [CONTRIBUTING](CONTRIBUTING.md).

## Repository layout

```
app/wizard-server   entry point
src/               Shared, WizardLib, RegistryLib, Wizard
test/              hspec suites: shared wizard-lib wizard
config/            configuration, committed templates as *.example
scripts/           build info, config expansion, strict build
```

## Requirements

 - [Stack](https://docs.haskellstack.org) (GHC 9.10.3, snapshot lts-24.37) and `hpack`
 - PostgreSQL 15 and MinIO/S3
 - [**engine-jinja**](https://github.com/ds-wizard/engine-jinja) libraries in `lib/` (document templating)
 - Optional: `fourmolu`, `hlint`, `cspell`

## Build and run

```bash
./scripts/expand-example-files.sh   # creates the gitignored configs from *.example
make build
make run
```

The document templating needs the [engine-jinja](https://github.com/ds-wizard/engine-jinja) libraries in `lib/`.
Download them from the releases or compile them yourself; the `Makefile` exports `PYTHONPATH`,
`LD_LIBRARY_PATH` and `DYLD_LIBRARY_PATH` to that folder.

## Tests and code style

```bash
make test                                    # all suites (needs a running database)
make test-wizard
make check                                   # hlint + fourmolu + cspell
make format
```

## License

This project is licensed under the Apache License v2.0 - see the [LICENSE](LICENSE.md) file.

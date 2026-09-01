# Contributing

When contributing to this repository, please first discuss the change you wish to make via issue, email, or any other
method with the owners of this repository before making a change.

## Development and Code Style

* Use [HLint](https://github.com/ndmitchell/hlint) for Haskell code suggestions (`make lint`)
* Use [Fourmolu](https://github.com/fourmolu/fourmolu) for code formatting (`make format`)
* Run `make check` before committing — it runs everything the CI checks phase runs (module boundaries, hlint,
  fourmolu, cspell)

### Naming conventions

* **Handler** - a module containing handler functions
* **DTO** - a module containing structures which represents request/response in API
* **Middleware** - a module containing middleware functions
* **Service** - a module containing service functions
* **Mapper** - a module containing mapper functions
* **DAO** - a module containing functions for a manipulation with data in database
* **Migration** - a module containing functions for running initial database migrations

### Tests

* New or changed functionality must be covered by tests (`make test`, or one suite via `make test-<suite>`)
* Pay attention to performance and potential edge cases, backend has to be robust

## Git Workflow

* `main` is the only long-lived branch — no develop, no release branches, no pull requests
* Every issue is developed in a git worktree with its own branch, rebased on `main` and merged back with
  `git merge --ff-only`
* A release is a `vX.Y.Z` tag on a commit of `main` — pushing the tag triggers the release phases of the CI
  pipeline (Docker image + API docs)
* [Semantic versioning](https://semver.org) — the matching major and minor version of FAIR Wizard components must
  be compatible

## CI Pipeline

A single workflow (`.github/workflows/pipeline.yml`) runs on every push:

1. **Checks** — hlint, fourmolu, module boundaries, cspell (parallel with build)
2. **Build** — Haskell build with GHC warnings treated as errors, produces the `dist/` artifact
3. **Test** — the five test suites in parallel, each against its own PostgreSQL + MinIO
4. **Image** (`main` and tags only) — a single Docker build pushed to `fairwizard/backend`
   (`main` + `sha-*` from `main`, semver tags from releases; `latest` and the major tag move only for the
   highest version)
5. **API docs** (tags only) — starts the built backend, extracts the Swagger specs of all four APIs and pushes
   them to the `codevence/api-docs` repository

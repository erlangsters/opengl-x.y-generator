# Release Workflow

Use this workflow when changing generated public surface or preparing target
binding repositories from the generator.

## Generator Change Gate

1. Update `binding-specs.conf` or generator code.
2. Run focused resolver/emitter tests for the affected policy family.
3. Run `rebar3 as test eunit`.
4. Run `rebar3 escriptize`.
5. Run non-populating generation smoke checks for all supported targets.
6. Inspect generated `gl.erl`, `gl.hrl`, and `gl.c` for `gl 4.6` and
   `gles 3.2`, plus any target whose surface changed.
7. Populate affected target repositories sequentially.
8. Run target smoke or capability tests with `EGL_PLATFORM=surfaceless`.
9. Run `git diff --check` in the generator and changed target repositories.
10. Commit generator changes and each changed target repository separately.

## Populate Scripts

Populate scripts write generator-root `gl.erl`, `gl.hrl`, and `gl.c` before
copying files into target repositories. Run them sequentially unless each run
is isolated in its own temporary generator output directory.

Use the target-specific maintainer scripts. They assume this repository sits
next to the target clones in the `graphics-stack` workspace:

- `populate-binding-gl-3.3.sh`
- `populate-binding-gl-4.1.sh`
- `populate-binding-gl-4.6.sh`
- `populate-binding-gles-2.0.sh`
- `populate-binding-gles-3.0.sh`
- `populate-binding-gles-3.1.sh`
- `populate-binding-gles-3.2.sh`

## Repository Boundaries

- Do not hand-edit generated files in target binding repositories.
- Do not commit `_build`, `priv`, `doc`, CMake build directories, crash dumps,
  or local generated scratch output unless a repository explicitly tracks that
  path.
- Keep generator commits separate from generated target repository commits.
- Keep docs-only changes separate from generated output changes when possible.

## Docs-Only Gate

For documentation-only work:

1. Edit active docs or target READMEs/extras.
2. Confirm no generated files changed.
3. Run consistency searches for stale target names, stale URLs, and old
   development-process language.
4. Run `git diff --check` in each changed repository.
5. Optionally build ExDoc for `opengl-4.6` and `opengl-es-3.2`.

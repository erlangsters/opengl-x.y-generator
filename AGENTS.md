# opengl-x.y-generator

- Generator in the `graphics-stack` family, currently near-release.
- Source of truth for generated `gl.erl`, `gl.hrl`, and `gl.c` consumed by the OpenGL and OpenGL ES binding repositories.
- Public surface changes start in `binding-specs.conf` and generator code. Do not hand-edit generated files in target repositories.
- Remaining unsupported or design-gated families live in `docs/omissions.md` and need an explicit public contract before they can be generated.
- Default verification pair is `opengl-4.6` and `opengl-es-3.2`. In headless sessions, run target tests with `EGL_PLATFORM=surfaceless`.
- Build with `rebar3 escriptize`. Generate with `./_build/default/bin/opengl_gen <api> <version>`.
- The `scripts/populate-binding-*.sh` helpers assume this repository sits next to the target clones in the `graphics-stack` workspace. They write scratch `gl.erl`, `gl.hrl`, and `gl.c` in the generator root before copying.
- Supported OTP is 28.

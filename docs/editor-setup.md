## Setting up your editor

**Note that this doc is incomplete and sometimes may require tinkering after doing the actions here. We welcome additions, clarifications, and instructions on how to set up other editors.**

### VS Code

VS Code works pretty well when set up to run within the container. Install the
"Remote containers" extension and choose "open folder within container" and it
should work. Install the recommended extensions within the container when
asked.

Use Ionide for F#. It's important to use (at least) version 5, which solves a
few bugs and has the latest version of fantomas (the code formatter).

#### Troubleshooting VScode

Often, the F# plugin will fail to work. This can be solved by doing a full
build of dotnet from the VSCode terminal and then restarting VSCode:

- open the terminal from within VSCode
- run `dotnet tool restore`
- run `dotnet paket restore`
- run `dotnet build`
- if this doesn't work, comment out the failing code until the build fully works, and repeat `dotnet build`
- from VSCode, run the command "Developer: Reload Window"

If that doesn't work, look in the "Output" tabs marked "msbuild" or "F#" for
clues.

### Merlin

Merlin is a language server used by ResCript that provides things like
autocompletion, type information, and go-to-definition functionality. Because
all libraries are installed inside the container and not on your local
machine, merlin needs to be running inside the container. Unfortunately, this
install is not completely isolated and we still need some supporting things
installed on your host machine.

_These instructions are quite possibly out of date, please let us know if this still works._

The way this works is that you'll need merlin on your host machine (to get the editor
support files) and then you'll point those at the `scripts/ocamlmerlin` wrapper
to execute the actual process inside the container. This means that you need
those scripts first in your `$PATH`.

- `export $PATH=[path to dark]/scripts:$PATH` in your shell config
- Install merlin:
  - `brew install opam`
  - `opam init -c 4.06.1`
    - grep `esy.json` for `"ocaml"` to ensure the version matches
    - copy snippet to your bashrc/shell config. DO NOT DO THIS FOR zsh. SEE BELOW.
  - `opam install merlin.3.2.2`
- Install editor integration:
  - vim+plug:
    - make sure you have a python provider for vim. Gvim comes with one. If you're using neovim, you can get one with `pip3 install pynvim`
    - add `Plug '~/.opam/default/share/merlin', { 'for': ['ocaml', 'merlin' ], 'rtp': 'vim' }` to your vim config file
  - others: ??

CAVEAT: If you install the zsh config that merlin recommends, it will
automatically execute the equivalent of `eval $(opam env)` after every command.
This command mucks with your `$PATH`, which means that you will never execute
the `scripts/` wrappers you want. Do not use the shell integration. Instead
call `eval $(opam env)` and then `export $PATH=[path to dark]/scripts:$PATH` (put
this in an alias or something).

### Formatting

You will also want to support formatting in your client. Dark uses Prettier for
JS/HTML/CSS, OCamlformat for ReScript, and Fantomas for F#. The script
`script/format` can be used to format or check formatting, and there is a
pre-commit hook you can use to run it automatically.

For emacs, see [the
readme](https://github.com/ocaml-ppx/ocamlformat#emacs-setup).

For vim:

- install [ALE](https://github.com/w0rp/ale)
- Add to your `.vimrc` (with an appropriate path-to-dark replacement):

```
set rtp+=~/[path to dark]/dark/scripts/ocamlformat
let g:ale_javascript_prettier_executable= '~/path to dark]/dark/scripts/prettier'
let g:ale_fixers =
\ {'rust': ['rustfmt'],
\  'ocaml':['ocamlformat'],
\  'javascript': ['prettier'],
\  'js': ['prettier'],
\  'html': ['prettier'],
\  'css': ['prettier'],
\  'scss': ['prettier']}
```

### Pre-commit hook

You probably also want to install a pre-commit hook that the formatters for
you.
`cp scripts/pre-commit-hook.sh .git/hooks/pre-commit && chmod +x .git/hooks/pre-commit`

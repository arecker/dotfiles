# dotfiles

Each top-level directory is a Stow package that mirrors its target layout under
`$HOME`. E.g. `claude/.claude/skills/vasa` stows to `~/.claude/skills/vasa`,
`bash/.bashrc` stows to `~/.bashrc`.

After adding or moving any file in this repo, run `make stow` from the repo
root to (re)symlink it into place — new files won't show up under `$HOME`
until you do. `make delete` reverses it.

# dotfiles

## Requirements

Sorry this is so bloaty.

- `stow`

## Setup

Setup GPG and SSH.  Clone this repo.

From this directory, run `make` to create all the symlinks.

```
arecker@localhost:~/src/dotfiles$ make
stow --target /Users/arecker --verbose ansible/ scripts/
LINK: .ansible.cfg => src/dotfiles/ansible/.ansible.cfg
LINK: bin/check-dotfiles => ../src/dotfiles/scripts/bin/check-dotfiles
...
```

Open a new shell and run `bootstrap`.  The script will complain if there is something missing, so just keep fixing and re-running until you get a clean exit.

### Lisp

Be sure `sbcl` is installed, then run `bootstrap-quicklisp`

## Removal

From this directory, run `make delete` to delete all the symlinks.

```
arecker@localhost:~/src/dotfiles$ make delete
stow --target /Users/arecker --verbose --delete ansible/ asdf/ scripts/
UNLINK: .ansible.cfg
UNLINK: .asdfrc
...
```

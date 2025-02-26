STOW_DIRS = $(wildcard */)

.PHONY: stow
stow:
	stow --target $(HOME) --verbose $(STOW_DIRS)

.PHONY : delete
delete :
	stow --target $(HOME) --verbose --delete $(STOW_DIRS)

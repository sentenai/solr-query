ghcid:
	ghcid -c "stack ghci --ghci-options=-fno-code"

docs:
	stack build --haddock --no-haddock-deps
	xdg-open `stack path --local-doc-root`/index.html

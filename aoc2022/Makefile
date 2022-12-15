.PHONY=lint
lint:
	@stack exec ormolu -- -m check $$(git ls-files '*.hs')
	@stack exec hlint -- .

.PHONE=fmt
fmt:
	@stack exec ormolu -- -m inplace $$(git ls-files '*.hs')


BIN = _build/default/src/acamlslife.exe
 
.PHONY: all
all:
	cd src; dune build acamlslife.exe

run: $(BIN)
	_build/default/src/acamlslife.exe


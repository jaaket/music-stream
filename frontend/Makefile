.PHONY: all

all: hello.js

hello.js: hello.c
	emcc -I./vendor/build/include hello.c -L./vendor/build/lib -llibvorbisfile -llibvorbis -llibogg \
		-s EXTRA_EXPORTED_RUNTIME_METHODS='["cwrap", "getValue"]' \
		-Werror \
		-s WASM=1 -o hello.js
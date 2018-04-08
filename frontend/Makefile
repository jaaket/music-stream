.PHONY: all

all: vorbis.js

vorbis.js: vorbis.c
	emcc -I./vendor/build/include $^ -L./vendor/build/lib -llibvorbisfile -llibvorbis -llibogg \
		-s EXTRA_EXPORTED_RUNTIME_METHODS='["cwrap", "getValue"]' \
		-Werror \
		-s TOTAL_MEMORY=67108864 \
		-s WASM=1 -o $@
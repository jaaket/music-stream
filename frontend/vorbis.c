#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <vorbis/vorbisfile.h>
#include <emscripten.h>

typedef struct {
    char* data;
    size_t bytes_read;
    size_t bytes_left;
    size_t total_bytes;
} mem_state;

size_t mem_read(void *ptr, size_t size, size_t nmemb, mem_state *datasource) {
    size_t bytes_to_copy;

    if (size * nmemb < datasource->bytes_left) {
        bytes_to_copy = size * nmemb;
    } else {
        bytes_to_copy = datasource->bytes_left;
    }

    /*
    printf("ptr: %p\n", ptr);
    printf("data: %p\n", datasource->data);
    printf("bytes_read: %d\n", datasource->bytes_read);
    printf("bytes_left: %d\n", datasource->bytes_left);
    printf("bytes_to_copy: %d\n", bytes_to_copy);
    */

    memcpy(ptr, datasource->data + datasource->bytes_read, bytes_to_copy);

    datasource->bytes_read += bytes_to_copy;
    datasource->bytes_left -= bytes_to_copy;

    return bytes_to_copy;
}

int mem_seek(mem_state *ds, ogg_int64_t offset, int whence) {
    switch (whence) {
        case SEEK_SET:
            ds->bytes_read = offset;
            ds->bytes_left = ds->total_bytes - offset;
            break;
        case SEEK_END:
            ds->bytes_read = ds->total_bytes;
            ds->bytes_left = 0;
            break;
        case SEEK_CUR:
            ds->bytes_read += offset;
            ds->bytes_left -= offset;
            break;
    }
    return 0;
}

long mem_tell(mem_state *ds) {
    return ds->bytes_read;
}

EMSCRIPTEN_KEEPALIVE
int decodeVorbis(OggVorbis_File *prev_vf, char* input, int n, char** output, long* output_samples, OggVorbis_File **vf) {
    // printf("prev_vf: %p\n", prev_vf);
    // printf("input: %p\n", input);

    ov_callbacks callbacks = {
        .read_func = (size_t (*)(void*, size_t, size_t, void*)) mem_read,
        .seek_func = (int (*)(void*, ogg_int64_t, int)) mem_seek,
        .close_func = NULL,
        .tell_func = (long (*)(void*)) mem_tell
    };

    *vf = calloc(1, sizeof(OggVorbis_File));
    // printf("*vf: %p\n", *vf);

    // TODO: free?
    mem_state *state = malloc(sizeof(mem_state));
    state->data = input;
    state->bytes_read = 0;
    state->bytes_left = n;
    state->total_bytes = n;

    long rv = ov_open_callbacks(state, *vf, NULL, 0, callbacks);

    if (rv < 0) {
        printf("Failed to initialize vorbisfile: %ld\n", rv);
        return rv;
    }

    // printf("offset: %lld\n", (*vf)->offset);
    // printf("end: %lld\n", (*vf)->end);

    ogg_int64_t total_samples = ov_pcm_total(*vf, -1);
    if (total_samples < 0) {
        printf("Could not determine number of samples: %lld\n", total_samples);
        return -1;
    }
    // printf("total_samples: %lld\n", total_samples);

    *output_samples = total_samples;

    // Allocate output buffer
    *output = malloc(total_samples * 2 * 2);
    // printf("*output: %p\n", *output);

    if (prev_vf != NULL) {
        // printf("prev_vf->datasource: %p\n", prev_vf->datasource);
        // printf("(*vf)->datasource: %p\n", (*vf)->datasource);

        // printf("ov_crosslap(%p, %p)\n", prev_vf, *vf);
        rv = ov_crosslap(prev_vf, *vf);
        if (rv < 0) {
            printf("Failed to crosslap: %ld\n", rv);
            return -1;
        }
    }

    long total_read = 0;
    long bytes_read;
    do {
        int bitstream;
        bytes_read = ov_read(*vf, *output + total_read, 4096, 0, 2, 1, &bitstream);
        if (bytes_read < 0) {
            printf("Failed to decode: %ld\n", bytes_read);
            return bytes_read;
        }
        // printf("bytes_read: %ld\n", bytes_read);
        // printf("total_read: %ld\n", total_read);

        total_read += bytes_read;
    } while (bytes_read > 0);

    // printf("total samples read: %ld\n", total_read / 4);

    /*
    long num_streams = ov_streams(&vf);
    printf("num_streams: %ld\n", num_streams);
    long serialnumber = ov_serialnumber(&vf, 1);
    printf("serialnumber: %ld\n", serialnumber);*/

    return 0;
}

int main(int argc, char *argv[]) {
    return 0;
}
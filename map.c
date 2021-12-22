#include "ruse.h"
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct Mapkey {
	uint64_t hash;
	char *str;
	size_t len;
	void *data; // if NULL, this key is empty
};

// djb2 (http://www.cse.yorku.ca/~oz/hash.html)
static uint64_t strhash(const char *str, size_t len) {
	uint64_t hash = 5381;
	int c;
	for (size_t i = 0; i < len && (c = *str++); i++) {
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
	}
	return hash;
}

// ref: https://stackoverflow.com/a/12996028
static uint64_t hash(const void *p) {
    uint64_t x = (uint64_t)p;
    x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
    x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
    x = x ^ (x >> 31);
    return x;
}

Map makemap(void) {
	Map m;
	m.bucketlen = 128; // TODO: test
	m.buckets = calloc(m.bucketlen, sizeof(struct Mapkey));
	return m;
}

void freemap(struct Map *map) {
	free(map->buckets);
}

void mapput(Map *m, char *str, size_t len, void *data) {
	uint64_t hash = strhash(str, len);
	size_t i = hash % m->bucketlen;
	size_t i_orig = i;

	struct Mapkey *k = &m->buckets[i];
	while (k->data) {
		if (hash == k->hash && strncmp(str, k->str, len) == 0)
			assert(!"trying to insert the same key");
		// linear probing
		i = (i + 1) % m->bucketlen;
		k = &m->buckets[i];
		if (i == i_orig)
			assert(!"map is full");
	}
	k->hash = hash;
	k->str = str;
	k->len = len;
	k->data = data;
}

void *mapget(Map *m, char *str, size_t len) {
	uint64_t hash = strhash(str, len);
	size_t i = hash % m->bucketlen;
	size_t i_orig = i;

	struct Mapkey *k = &m->buckets[i];
	while (k->data) {
		if (hash == k->hash && strncmp(str, k->str, len) == 0)
			break;
		// linear probing
		i = (i + 1) % m->bucketlen;
		k = &m->buckets[i];
		if (i == i_orig) {
			// not found
			return NULL;
		}
	}
	return k->data;
}

#include "ruse.h"
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct mapkey {
	uint64_t hash;
	char *str;
	void *data; // if NULL, this key is empty
};

// djb2 (http://www.cse.yorku.ca/~oz/hash.html)
static uint64_t strhash(const char *str) {
	uint64_t hash = 5381;
	int c;
	while ((c = *str++)) {
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

void makemap(struct map *m) {
	m->bucketlen = 128; // TODO: test
	m->buckets = calloc(m->bucketlen, sizeof(struct mapkey));
}

void freemap(struct map *map) {
	for (size_t i = 0; i < map->bucketlen; i++)
		if (map->buckets[i].str)
			free(map->buckets[i].str);
	free(map->buckets);
}

// Returns 0 if a same key is inserted again.
int mapput(struct map *m, const char *str, void *data) {
	uint64_t hash = strhash(str);
	size_t i = hash % m->bucketlen;
	size_t i_orig = i;

	struct mapkey *k = &m->buckets[i];
	while (k->data) {
		if (hash == k->hash && strcmp(str, k->str) == 0)
			return 0;
		// linear probing
		i = (i + 1) % m->bucketlen;
		k = &m->buckets[i];
		if (i == i_orig)
			assert(!"map is full");
	}
	k->hash = hash;
	size_t len = strlen(str);
	k->str = calloc(len + 1, 1);
	strncpy(k->str, str, len);
	k->str[len] = '\0';
	k->data = data;
	return 1;
}

void *mapget(struct map *m, const char *str) {
	uint64_t hash = strhash(str);
	size_t i = hash % m->bucketlen;
	size_t i_orig = i;

	struct mapkey *k = &m->buckets[i];
	while (k->data) {
		if (hash == k->hash && strcmp(str, k->str) == 0)
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

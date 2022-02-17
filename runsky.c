#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

void enforce(bool legal, const char *message) {
  if (!legal) {
    fprintf(stderr, "Program Error: %s\n", message);
    exit(1);
  }
}

typedef struct ski_data *ski;

enum ski_tag { call, k, s, byte, yes, no, nil, cons };

struct ski_data {
  enum ski_tag tag;
  ski left;
  ski right;
  size_t ref;
};

bool function(ski node, enum ski_tag key, int args) {
  if (args == 0) {
    return key == node->tag;
  } else {
    return node->tag == call && function(node->left, key, args - 1);
  }
}

ski make_raw(struct ski_data raw) {
  ski ptr = malloc(sizeof(struct ski_data));
  *ptr = raw;
  return ptr;
}

ski make_call(ski left, ski right) {
  left->ref++;
  right->ref++;
  return make_raw((struct ski_data){call, left, right, 0});
}

ski make(enum ski_tag tag) {
  assert(tag != call);
  return make_raw((struct ski_data){tag, 0, 0, 0});
}

ski local(ski node) {
  node->ref++;
  return node;
}

void deref_contents(ski node) {
  void deref(ski);
  if (node->tag == call) {
    deref(node->left);
    deref(node->right);
  }
}

void deref(ski node) {
  node->ref--;
  if (node->ref == 0) {
    deref_contents(node);
    free(node);
  }
}

void copy(ski target, struct ski_data source) {
  deref_contents(target);
  target->tag = source.tag;
  target->left = source.left;
  target->right = source.right;
}

void move(ski target, struct ski_data source) {
  if (source.tag == call) {
    source.left->ref++;
    source.right->ref++;
  }
  copy(target, source);
}

void reduce(ski node) {
  if (node->tag == call) {
    reduce(node->left);
  }
  if (function(node, k, 2)) {
    ski x = node->left->right;
    move(node, *x);
    reduce(node);
  } else if (function(node, s, 3)) {
    ski x = node->left->left->right;
    ski y = node->left->right;
    ski z = node->right;
    ski left = local(make_call(x, z));
    ski right = local(make_call(y, z));
    copy(node, (struct ski_data){call, left, right});
    reduce(node);
  }
}

unsigned char read_byte(ski index, unsigned char data, int iterations) {
  if (iterations > 0) {
    ski bit = local(make_call(make_call(index->right, make(yes)), make(no)));
    reduce(bit);
    if (function(bit, yes, 0)) {
      data |= 1 << 8 - iterations;
    } else {
      enforce(function(bit, no, 0), "unable to extract bit from byte");
    }
    deref(bit);
    return read_byte(index->left, data, iterations - 1);
  } else {
    return data;
  }
}

void print(ski node) {
  ski list = local(make_call(make_call(node, make(nil)), make(cons)));
  reduce(list);
  if (function(list, cons, 2)) {
    ski head = list->left->right;
    ski tail = list->right;

    ski word = local(make_call(head, make(byte)));
    reduce(word);
    enforce(function(word, byte, 8), "unable to extract byte from stream");
    unsigned char data = read_byte(word, 0, 8);
    putchar(data);

    deref(word);
    print(tail);
  } else {
    enforce(function(list, nil, 0), "bad list element");
  }
  deref(list);
}

ski parse(FILE *file) {
  int token = getc(file);
  enforce(token != -1, "incomplete tape");
  if (token == '0') {
    ski f = parse(file);
    ski x = parse(file);
    return make_call(f, x);
  } else {
    return make(token - '0');
  }
}

int main(int argc, const char **argv) {
  FILE *file = stdin;
  if (argc > 1) {
    file = fopen(argv[1], "r");
    enforce(file, "bad file name");
  }
  ski node = local(parse(file));
  print(node);
  deref(node);
  return 0;
}

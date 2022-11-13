#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  } else if (node->tag == call) {
    return function(node->left, key, args - 1);
  } else {
    return false;
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

char read_bit(ski node) {
  reduce(node);
  if (function(node, yes, 0)) {
    return 1;
  } else if (function(node, no, 0)) {
    return 0;
  } else {
    enforce(false, "unable to extract bit from byte");
  }
}

char read_byte(ski index) {
  reduce(index);
  enforce(function(index, byte, 8), "unable to extract byte from stream");
  char data = 0;
  for (int lower = 0; lower < 8; lower++) {
    ski bit = index->right;
    data |= read_bit(bit) << lower;
    index = index->left;
  }
  return data;
}

// Consumes it's argument. This is used for tail recursion.
void print(ski list) {
  reduce(list);

  if (function(list, cons, 2)) {
    ski head = list->left->right;
    ski tail = list->right;

    putchar(read_byte(head));

    tail->ref++;
    deref(list);
    print(tail);
  } else if (function(list, nil, 0)) {
    deref(list);
  } else {
    enforce(false, "bad list element");
  }
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

ski parse_file(const char *name) {
  FILE *file = strcmp(name, "-") == 0 ? stdin : fopen(name, "r");
  if (!file) {
    fprintf(stderr, "bad file name: %s\n", name);
    exit(1);
  }
  ski node = parse(file);
  fclose(file);
  return node;
}

int main(int argc, const char **argv) {
  if (argc < 2) {
    printf("Usage: %s input1.sky input2.sky ...\n", argv[0]);
    printf("Description: Evaluate sky byte code and print the results\n");
    printf("Inputs:\n");
    printf(" The input files are applied to each other right associatively\n");
    printf(" For example %s f.sky g.sky x.sky is treated as f(g(x))\n",
           argv[0]);
    printf(" The final expression should evaluate to a stream of bytes\n");
    return 0;
  }

  ski expression = parse_file(argv[argc - 1]);
  for (int i = argc - 2; i > 0; i--) {
    expression = make_call(parse_file(argv[i]), expression);
  }
  expression->ref++;
  print(expression);
  return 0;
}

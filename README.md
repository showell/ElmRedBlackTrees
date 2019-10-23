This is an exploration of RedBlack trees that leverages
the Elm implementation of Dict to show them in action.

Elm uses
[left-leaning RedBlack trees](https://en.wikipedia.org/wiki/Left-leaning_red%E2%80%93black_tree),
which are a minor variation of traditional RedBlack trees.  (Essentially, you never allow
a child node to Red; it's either empty or Black.)


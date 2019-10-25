This is an exploration of RedBlack trees that leverages
the Elm implementation of Dict to show them in action.

Elm uses
[left-leaning RedBlack trees](https://en.wikipedia.org/wiki/Left-leaning_red%E2%80%93black_tree),
which are a minor variation of traditional RedBlack trees.  (Essentially, you never allow
a child node to Red; it's either empty or Black.)


DEPLOYMENT:

This is very specific to my environment, but it shows how I just deploy this using github pages:

    (cd ~/PROJECTS/website/ && cp ~/PROJECTS/RedBlack/index.html redblack.html && git commit -am 'latest' && git push origin master)

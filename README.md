# bartleby

[![Latest version published to Clojars](https://img.shields.io/clojars/v/bartleby.svg)](https://clojars.org/bartleby)
[![Travis CI Build Status](https://travis-ci.org/chbrown/bartleby.svg)](https://travis-ci.org/chbrown/bartleby)
[![Coveralls Coverage Status](https://coveralls.io/repos/chbrown/bartleby/badge.svg)](https://coveralls.io/github/chbrown/bartleby)

A very _faithful_ BibTeX parser.

- Preserves comments, order, formatting, string interpolations
- _Mostly_ idempotent at its core
  * Whitespace outside values is always standardized
- Implements various levels of cleaning up / standardization beyond whitespace


## Use as jar

1. Go to [releases](https://github.com/chbrown/bartleby/releases) and download the top (latest) <code>bartleby-<em>x.y.z</em>-standalone.jar</code>
2. Ensure you have `java` installed (`java -version` should print out your version)
3. In your shell, run `java -jar bartleby-*.jar --help`, which will print out a help message describing all command line functionality.
  * For example, `java -jar bartleby-*.jar cat my.bib their.bib` will read each of `my.bib` and `their.bib` and print the formatted BibTeX to `stdout` in that order.


## Compile and install as binary

    lein bin
    cp $(find target -type f -perm +111) /usr/local/bin/bart


## Recipes

Reformat a single .bib file, in-place:

    bart cat <research.bib | sponge research.bib

Delete .bib file entries not used in LaTeX document:

    bart select {paper.aux,research.bib} | sponge research.bib


## Alternatives

* [`bibexport`](https://www.ctan.org/tex-archive/biblio/bibtex/utils/bibexport/)
  - Cleverly uses the BibTeX compiler and a special `export.bst` style file to render BibTeX natively.
  - But it is not very faithful:
    + Unrecognized fields (those outside the standard 22 + 7 special names hard-coded into `export.bst`) are deleted
    + Comments are deleted
    + All fields are ordered alphabetically
    + All entries are ordered by first appearance in the document / `.aux` file
  - And the output format is not customizable (unless you modify the `export.bst` file):
    + All case-insensitive keywords (pubtype and field keys) are lowercased
    + All fields keys are indented with two spaces, and the field values are all aligned at column 19
    + Lines are wrapped at 78 characters


## References

* Interesting blog post on writing parsers in Clojure with monads at <http://albert.rierol.net/clojure-monads.html>, with code at <https://gist.github.com/acardona/3672948>
  - His answer to whether "whether I could write better clojure programs by using monads"?
    "The short answer is that the disadvantages outweight the advantages."
* "[Kern](https://github.com/blancas/kern) is a library of parser combinators for Clojure"
  - 130 stars on GitHub.
  - It doesn't appear to be compatible with ClojureScript.
* "[The Parsatron](https://github.com/youngnh/parsatron) is a functional parser library"
  - 173 stars on GitHub.
  - Ported to ClojureScript.
  - [doc/guide](https://github.com/youngnh/parsatron/blob/master/doc/guide.markdown)

I've also compiled my own [guide / reference / notes](Parsatron.md) for the `parsatron` library.


## Development

### Testing

Run the following to exclude dev/test from [cloverage](https://github.com/cloverage/cloverage)'s report:

    lein cloverage --ns-exclude-regex user


## Release instructions

### Cutting and deploying a new version

Update the version manually in `project.clj` and `README.md`, commit those changes, and push.

Then add a tag and push:

    tag=v$(lein pprint :version | tr -d \")
    git tag $tag
    git push --tags

Finally, deploy to [Clojars](https://clojars.org/):

    # export GPG_TTY=$(tty) # uncomment if gpg fails with "Inappropriate ioctl"
    lein deploy

* _TODO_: customize `:release-tasks` and use `lein release :major / :minor / :patch`


### Adding binaries to [GitHub releases](releases)

This uses [github-release](https://github.com/aktau/github-release) (`go get github.com/aktau/github-release`),
which expects the environment variables `GITHUB_USER` and `GITHUB_TOKEN` to be set.

Build the normal jar and uber(standalone)jar:

    lein uberjar

Create a GitHub "release":

    # set tag if you lost it from earlier:
    #tag=$(git tag --sort=committerdate | tail -1)
    # or:
    #tag=$(git describe --abbrev=0 --tags)
    github-release release -r bartleby -t $tag

Upload the built files:

    ver=$(lein pprint :version | tr -d \")
    github-release upload -r bartleby -t $tag -f target/bartleby-$ver.jar -n bartleby-$ver.jar
    github-release upload -r bartleby -t $tag -f target/bartleby-$ver-standalone.jar -n bartleby-$ver-standalone.jar

(You can check the current tags / releases available on GitHub at any time with: `github-release info -r bartleby`)


### Generating and publishing documentation

Create a fresh clone from the `gh-pages` branch:

    rev=$(git rev-parse --short master)
    repo=${TMPDIR-/tmp/}bartleby-gh-pages
    git clone git@github.com:chbrown/bartleby.git -b gh-pages $repo

Now, back in this repo on the `master` branch, generate the documentation directly into that clone:

    lein update-in :codox assoc :output-path \"$repo\" -- codox

Then go to that repo's directory, commit the changes with a message pointing to the current commit on `master`, and push:

    cd $repo
    git add .
    git commit -m "Sync documentation with master @ $rev"
    git push


## License

Copyright © 2016-2018 Christopher Brown.
[MIT Licensed](https://chbrown.github.io/licenses/MIT/#2016-2018).

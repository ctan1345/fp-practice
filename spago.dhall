{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "psci-support", "lists", "maybe", "tuples", "newtype", "strings", "profunctor", "unicode", "transformers", "tree-rose", "simple-csv", "either", "node-fs-aff", "open-folds", "arrays", "bifunctors", "foldable-traversable", "node-buffer", "node-fs", "nonempty", "ordered-collections", "debug" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

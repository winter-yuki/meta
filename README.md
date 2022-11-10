# meta

Simple functional language:
```
\fun flip xs => \case xs \of
   Leaf -> Leaf
|  Branch l x r -> Branch (flip r) x (flip l)
\esac

\fun test xs => flip (flip xs)
```

And code transformations:
* Deforestation

## Getting started

Clone repo and run:
```
$ stack run -- defs.la -d 5 test
```

# meta

Simple functional language:
```
\fun flip xs => \case xs \of
   Leaf -> Leaf
|  Branch l x r -> Branch (flip l) x (flip r)
\esac

\fun test xs => flip (flip xs)
```

And code transformations:
* Deforestation

## Getting started

Clone repo and run:
```
$ stack run -- test.l -d 5 test
```

\fun flip xs => \case xs \of
   Leaf -> Leaf
|  Branch l x r -> Branch (flip l) x (flip r)
\esac

\fun test xs => flip (flip xs)

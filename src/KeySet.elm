module KeySet exposing (..)

{- KeySet.elm:

   This file serves as a wrapper module that allows any type
   that can be assosciated with a (int) key as a member in
   a set. This requires a function to convert this arbitrary
   type into an int key as well as an inverse function.
   e.g. This allows me to use Tiles in sets.

   The functions in this module are analogous to the normal
   set functions.
-}

-- wrapper module of set for tiles
import Set

type alias KeyGen v = v -> Int
type alias InvKey v = Int -> v
type KeySet v = KeySet (KeyGen v) (InvKey v) (Set.Set Int)

-- creates an empty keyset with a given key function and inverse function
empty : KeyGen v -> InvKey v -> KeySet v
empty keygen invkey =
    KeySet keygen invkey (Set.empty)

-- creates a keyset from a single element with corresponding functions
singleton : v -> KeyGen v -> InvKey v -> KeySet v
singleton val keygen invkey =
    let
        key = keygen val
    in
    KeySet keygen invkey (Set.singleton key)

-- insert an element into a keyset
insert : v -> KeySet v -> KeySet v
insert val (KeySet keygen invkey s) =
    let
        key = keygen val
    in
    KeySet keygen invkey (Set.insert key s)

-- remove an element from a keyset
remove : v -> KeySet v -> KeySet v
remove val (KeySet keygen invkey s) =
    let
        key = keygen val
    in
    KeySet keygen invkey (Set.remove key s)

-- checks if an element is in the keyset
member : v -> KeySet v -> Bool
member val (KeySet keygen _ s) =
    let
        key = keygen val
    in
    Set.member key s

-- checks if the keyset is empty
isEmpty : KeySet v -> Bool
isEmpty keys =
    let
        (KeySet keygen invkey _) = keys
    in
    keys == empty keygen invkey

-- checks the size of the keyset
size : KeySet v -> Int
size (KeySet _ _ s) =
    Set.size s

-- converts keyset into a list using the inverse function
toList : KeySet v -> List v
toList (KeySet _ invkey s) =
    let
        keys = Set.toList s
    in
    List.map invkey keys

-- creates a keyset from a list
fromList : KeyGen v -> InvKey v -> List v -> KeySet v
fromList keygen invkey vs =
    let
        keys = List.map keygen vs
    in
    KeySet keygen invkey (Set.fromList keys)

-- gets the intersection of two keysets of the same type
intersect : KeySet v -> KeySet v -> KeySet v
intersect (KeySet keygen invkey s1) (KeySet _ _ s2) =
    KeySet keygen invkey (Set.intersect s1 s2)

-- gets the diff of two keysets of the same type
diff : KeySet v -> KeySet v -> KeySet v
diff (KeySet keygen invkey s1) (KeySet _ _ s2) =
    KeySet keygen invkey (Set.diff s1 s2)

-- unions two keysets of the same type
union : KeySet v -> KeySet v -> KeySet v
union (KeySet keygen invkey s1) (KeySet _ _ s2) =
    KeySet keygen invkey (Set.union s1 s2)

-- filters out elements given a filter function from a keyset
filter : (v -> Bool) -> KeySet v -> KeySet v
filter f (KeySet keygen invkey s1) =
    let
        foo x =
            f (invkey x)
    in
    KeySet keygen invkey (Set.filter foo s1)

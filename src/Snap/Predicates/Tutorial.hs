module Snap.Predicates.Tutorial
  ( -- * Motivation
    -- $motivation

    -- * Introduction
    -- $introduction

    -- * Example Predicate
    -- $example

    -- * Routes
    -- $routes
  )
where

{- $motivation

The purpose of the @snap-predicates@ package is to facilitate the
convenient definition of safe Snap handlers. Here safety
means that a handler can declare all pre-conditions which must be
fulfilled such that the handler can produce a successful response.
It is then statically guaranteed that the handler will not be
invoked if any of these pre-conditions fails.

-}

{- $introduction

The @snap-predicates@ package defines a 'Data.Predicate.Boolean' type
which carries \-\- in addition to actual truth values 'Data.Predicate.T'
and 'Data.Predicate.F' \-\- meta-data for each case:

@
data Boolean f t =
    F (Maybe f)
  | T t
  deriving (Eq, Show)
@

Further there is a type-class 'Data.Predicate.Predicate' defined which
contains an evaluation function 'Data.Predicate.apply', where the
predicate instance is applied to some value, yielding 'Data.Predicate.T'
or 'Data.Predicate.F'.

@
class Predicate p a where
    type FVal p
    type TVal p
    apply :: p -> a -> Boolean (FVal p) (TVal p)
@

All concrete predicates are instances of this type-class, which does not
specify the type against which the predicate is evaluated, nor the types
of actual meta-data for the true/false case of the Boolean returned.
Snap related predicates are normally defined against 'Snap.Core.Request'
and in case they fail, they return an status code and an optional message.

Besides these type definitions, there are some ways to connect two
'Data.Predicate.Predicate's to form a new one as the logical @or@ or the
logical @and@ of its parts. These are:

  * 'Data.Predicate.:|:' and 'Data.Predicate.:||:' as logical @OR@s

  * 'Data.Predicate.:&:' as logical @AND@

Besides evaluating to 'Data.Predicate.T' or 'Data.Predicate.F' depending
of the truth values of its parts, these connectives also propagate the
meta-data appropriately.

If 'Data.Predicate.:&:' evaluates to 'Data.Predicate.T' it has to combine
the meta-data of both predicates, and it uses the product type
'Data.Predicate.:*:' for this. This type also has a right-associative
data constructor using the same symbol, so one can combine many predicates
without having to nest the meta-data pairs.

In the @OR@ case, the two predicates have potentially meta-data of
different types, so we use a sum type 'Either' whenever we combine
two predicates with 'Data.Predicate.:||:'. For convenience a type-alias
'Data.Predicate.:+:' is defined for 'Either', which allows simple infix
notation. However, for the common case, where both predicates have
meta-data of the same type, there is often no need to distinguish which
@OR@-branch was true. In this case, the 'Data.Predicate.:|:' combinator
can be used.

Finally there are 'Data.Predicate.Const' and 'Data.Predicate.Fail' to
always evaluate to 'Data.Predicate.'T' or 'Data.Predicate.F' respectively.

For an example of how these operators are used, see below in 'Routes' below.
-}

{- $example

@
data 'Snap.Predicates.Accept' = 'Snap.Predicates.Accept' ByteString deriving Eq

instance 'Data.Predicate.Predicate' 'Snap.Predicates.Accept' 'Snap.Core.Request' where
    type 'Data.Predicate.FVal' 'Snap.Predicates.Accept' = (Word, Maybe ByteString)
    type 'Data.Predicate.TVal' 'Snap.Predicates.Accept' = ()
    'Data.Predicate.apply' ('Snap.Predicates.Accept' x) r =
        if x \`elem\` headerValues r \"Accept\"
            then 'Data.Predicate.T' ()
            else 'Data.Predicate.F' $ Just (406, Just $ \"Expected 'Accept: \" <> x <> \"'.\")
@

This is a simple example testing the value of a 'Snap.Core.Request's accept
header against some given value. The function @headerValues@ is not shown,
but gets the actual Accept-Header values of the request.

As mentioned before, Snap predicates usually fix the type @a@ from
'Data.Predicate.Predicate' above to 'Snap.Core.Request'. The associated
types 'Data.Predicate.FVal' and 'Data.Predicate.TVal' denote the meta-data
types of the predicate. In this example, there is no useful information for
the 'Data.Predicate.T'-case, so @TVal@ becomes '()'. The 'Data.Predicate.F'-case
is set to the pair @(Word, Maybe ByteString)@ and indeed, if the predicate
fails it sets the right HTTP status code (406) and some helpful message.

-}

{- $routes

So how are 'Data.Predicate.Predicate's used in some Snap application?
One way is to just apply them to a given request inside a snap handler, e.g.

@
someHandler :: 'Snap.Core.Snap' ()
someHandler = do
    req <- 'Snap.Core.getRequest'
    case apply ('Snap.Predicates.Accept' \"application/json\" 'Data.Predicate.:&:' 'Snap.Predicates.Param' \"baz\") req of
        'Data.Predicate.T' (_ 'Data.Predicate.:*:' bazValue) -> ...
        'Data.Predicate.F' (Just (i, msg))  -> ...
        'Data.Predicate.F' Nothing          -> ...
@

However another possibility is to augment route definitions via Snap.Core.route, e.g.

@
sitemap :: 'Snap.Routes.Routes' Snap ()
sitemap = do
    'Snap.Routes.get'  \"\/a\" handlerA $ 'Snap.Predicates.AcceptJson' 'Data.Predicate.:&:' ('Snap.Predicates.Param' \"name\" 'Data.Predicate.:|:' 'Snap.Predicates.Param' \"nick\") 'Data.Predicate.:&:' 'Snap.Predicates.Param' \"foo\"
    'Snap.Routes.get'  \"\/b\" handlerB $ 'Snap.Predicates.AcceptJson' 'Data.Predicate.:&:' ('Snap.Predicates.Param' \"name\" 'Data.Predicate.:||:' 'Snap.Predicates.Param' \"nick\") 'Data.Predicate.:&:' 'Snap.Predicates.Param' \"foo\"
    'Snap.Routes.get'  \"\/c\" handlerC $ 'Data.Predicate.Fail' (410, Just \"Gone.\")
    'Snap.Routes.post' \"\/d\" handlerD $ 'Snap.Predicates.AcceptThrift'
    'Snap.Routes.post' \"\/e\" handlerE $ 'Snap.Predicates.Accept' \"plain/text\"
@

The handlers then encode their pre-conditions in their type-signature:

@
handlerA :: 'Snap.Predicates.AcceptJson' 'Data.Predicate.:*:' ByteString 'Data.Predicate.:*:' ByteString -> Snap ()
handlerB :: 'Snap.Predicates.AcceptJson' 'Data.Predicate.:*:' (ByteString 'Data.Predicate.:+:' ByteString) 'Data.Predicate.:*:' ByteString -> Snap ()
handlerC :: 'Snap.Predicates.AcceptJson' 'Data.Predicate.:*:' Char -> Snap ()
handlerD :: 'Snap.Predicates.AcceptThrift' -> Snap ()
handlerE :: () -> Snap ()
@

As usualy these type-declarations have to match, or else the code will
not compile. One thing to note is that 'Data.Predicate.Fail' works with
all handler signatures, which is safe, because the handler is never
invoked, or else Fail is used in some logical disjunction.

Given the route and handler definitions above, one can then integrate
with Snap via 'Snap.Routes.expandRoutes', which turns the
'Snap.Routes.Routes' monad into a list of
@'Snap.Core.MonadSnap' m => [(ByteString, m a)]@.
Additionally routes can be turned into Strings via 'Snap.Routes.showRoutes'.

-}

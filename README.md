Snap-Predicates
===============

This library provides the definition of a type-class `Predicate`
together with several concrete implementations which are used to
constrain the set of possible `Snap` handlers in a type-safe
way.

A module [Snap.Predicates.Tutorial](http://hackage.haskell.org/packages/archive/snap-predicates/latest/doc/html/Snap-Predicate-Tutorial.html)
is included, outlining the basic concepts.

Example Usage
-------------

```haskell
main :: IO ()
main = do
    mapM_ putStrLn (showRoutes sitemap)
    quickHttpServe (route . expandRoutes $ sitemap)

sitemap :: Routes Snap ()
sitemap = do
    head_ "/status" (const $ return ())

    get  "/foo" listFoo $
        accept :&: paramDef "off" 0 :&: paramDef "size" 100

    post "/foo" createFoo $
        accept :&: contentType

listFoo :: Media "text" "plain" :*: Int :*: Int -> Snap ()
listFoo (_mt :*: _off :*: _size) = return ()

createFoo :: Media "text" "plain" :*: Content "application" "x-protobuf" -> Snap ()
createFoo (_mt :*: _ct) = return ()
```

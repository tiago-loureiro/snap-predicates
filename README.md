Snap-Predicates
===============

This library provides the definition of a type-class `Predicate`
together with several concrete implementations which are used to
constrain the set of possible `Snap` handlers in a type-safe
way.

A module [Snap.Predicates.Tutorial](http://hackage.haskell.org/packages/archive/snap-predicates/latest/doc/html/Snap-Predicates-Tutorial.html)
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
        Accept Text Plain :&: ParamDef "off" 0 :&: ParamDef "size" 100

    post "/foo" createFoo $
        Accept Text Plain :&: ContentType Application Protobuf

listFoo :: MediaType Text Plain :*: Int :*: Int -> Snap ()
listFoo (_mt :*: _off :*: _size) = return ()

createFoo :: MediaType Text Plain :*: Content Application Protobuf -> Snap ()
createFoo (_mt :*: _ct) = return ()
```

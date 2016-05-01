Note: work in progress.

# servant-reflex

This library lets you automatically derive [`reflex-dom`](https://github.com/reflex-frp/reflex-dom) clients that query each endpoint of a [`servant`](htps://github.com/haskell-servant/servant) webservice.

## Building the library and test server

### With `reflex-platform`:

First build the library:

```
git clone https://github.com/imalsogreg/servant-reflex
cd servant-reflex
git submodule update --init --recursive
./build.sh
./toSite.sh
```

(the `toSite.sh` script copies the generated javascript to the server's static assets directory)


Then build the test server (not in a nix-shell):

```
./init-sandbox.sh
cd testserver && cabal install --only-dep && cabal build && cd ..

```


### Or, with a system-wide ghcjs

You will need a recent [GHCJS installation](https://github.com/ghcjs/ghcjs), or use the [reflex-platform](https://github.com/reflex-frp/reflex-platform). A [snap](https://github.com/snapframework) server using [servant-snap](https://github.com/haskell-servant/servant-snap) is provided for serving the api. `Snap 1.0` is not yet on Hackage (and servant-snap is still experimental), but we bundle the sources as a git submodule. To install everything with cabal:

```
git clone https://github.com/imalsogreg/servant-reflex
cd servant-reflex
git submodule update --init --recursive
./init-sandbox.sh
cd testserver && cabal install --only-dep && cabal build && cd ..
cabal install --only-dep
cabal build
./toSite
```

The `toSite.hs` script copies the ghcjs-generated files into the server's static directory.

## Running the example site

The server must be run from the directory where static assets live:

```
cd testserver
dist/build/back/back -p 8001
```

And simply browse to `localhost:8001`

## Example

We have a webservice API defined in a module where both the server (compiled with ghc) and the frontend (compiled with ghcjs) can see it:

```haskell
type API = "getint"  :> Get '[JSON] Int
      :<|> "sayhi"   :> QueryParam  "username" String
                     :> QueryParams "greetings" String
                     :> QueryFlag   "gusto"
                     :> Get '[JSON] String
      :<|> "double" :> ReqBody '[JSON] Double
                    :> Post '[JSON] Double
      :<|> Raw
```

`servant-reflex` then computes functions that can query the API through an `XhrRequest`.

```haskell

 runGUI :: forall t m.MonadWidget t m => do

  -- servant-reflex computes FRP functions for each API endpoint
  let (getint :<|> sayhi :<|> doubleit :<|> _) = client (Proxy :: Proxy API)
                                                        (Proxy :: Proxy m)
                                                        (constDyn (BasePath "/"))
```

These client functions are computed from the API and manage serialization, XhrRequest generation, and deserialization for you. `a` parameters become `Behavior t (Maybe a)` values. You provide a trigger event and receive an `Event t (Maybe r, XhrResponse)`, with responses from the API server (which you would write with `servant-server`).

```haskell
   -- No need to write these functions. servant-reflex creates them for you!
   getint :: MonadWidget t m
          => Event t ()  -- ^ Trigger the XHR Request
          -> m (Event t (Maybe Int, XhrResponse)) -- ^ Consume the answer

   sayhi :: MonadWidget t m
         => Behavior t (Maybe String) -- ^ One input parameter - the 'name'
         -> Behavior t [String]       -- ^ Another input - list of preferred greetings
         -> Behavior t Bool           -- ^ Flag for capitalizing the response
         -> Event t ()                -- ^ Trigger the XHR Request
         -> m (Event t (Maybe String, XhrResponse))

   doubleit :: MonadWidget t m
            => Behavior t (Maybe Double)
            -> Event t ()
            -> m (Event t (Maybe Double, XhrResponse))
```

Plug any of these functions into your reactive frontend to consume backend services without having to build XhrRequests by hand.

```haskell
  elClass "div" "int-demo" $ do
    intButton  <- button "Get Int"
    serverInts <- fmap fst <$> getint intButton
    display =<< holdDyn (Just 0) serverInts

  elClass "div" "hello-demo" $ do
    nameText <- (current . value)               <$> textInput def
    greetings <- (fmap words . current . value) <$> textInput def
    withGusto <- checkbox def
    helloButton <- button "Say hi"
    hellos <- fmap fst <$> sayhi nameText greetings withGusto helloButton
    display =<< holdDyn Nothing hellos

  elClass "div" "demo-double" $ do
    inputDouble  <- (fmapMaybe readMaybe . current) <$> textInput def
    doubleButton <- button "Double it"
    outputDouble <- fmap fst <$> doubleit inputDouble doubleButton
    display =<< holdDyn Nothing outputDouble
```

For a great introduction to recative DOM building, see the [README](https://github.com/reflex-frp/reflex-platform) for the `reflex-platform`.

## Input validation

The frontend's widgets are sometimes in a state where a valid XHR request can be generated, and sometimes not. When all of the input parameters (`Behavior t (Maybe a)`) are `Just _`, the trigger event will communicate with the server. When any of the inputs is `Nothing`, no XHR request will be made (the event will be silently dropped). In the future input parameters will be encoded as `Behavior t (Either e a)`, and triggers that occur when a Request can't be generated will immediately return a `Left e`, which you could use to draw an error in the page.

## Invalid responses

For convenience, successful XHR responses are decoded into a `MimeUnrender ct a => Just a`. The `XHRResponse` is also returned, which you can inspect to get more fine-grained information about the response.

## TODOs

We're still working on thi

0.3.3
-----

Explicitly escape reserved characters from path pieces and query parameters


0.3.2
-----

- **Addition of 'Servant.Reflex.Common.Req.ClientOpts'** which
allows the users to apply a final adjustment to Requests before
thery are sent, such as setting the 'withCredentials' flag
or print the Request to the console for debugging
[PR #47](https://github.com/imalsogreg/servant-reflex/pull/47)

0.3
---

- **Addition of the 'Servant.Reflex.Multi' module** which allows the user
to choose some '(Applicative f, Traversable f) => f' for collecting
endpoint parameters. The traversable instance is used for passing
multiple 'XhrRequest's in response to a single event trigger. The
Applicative instance is used to combine parameters. For example,
'Capture "id" Name :> QueryParam "greeting" Text :> Get '[JSON] Text'
will generate a client function:
`Dynamic t (f Name) -> Dynamic t (f greeting) -> Event t tag ->
 m (Event t (f (ReqResult tag Text)))`
Calling that function with 'f' chosen to be list ('[]'), this call
`clientFun
(constDyn ["Greg", "Bernie"])
(constDyn ["Greetings", "Hey"])
trigger`
would produce four XhrRequests per 'trigger' event, one for each
combination of possible parameters, and one response event
would carry a list of the four responses.
A 'zip-wise' combination of the parameters can be chosen with
a different applicative wrapper: 'ZipList'

- **Event tags**. A new type parameter lets users choose a payload
value to attach to outgoing requests at the point of triggering,
and which is embedded in the response, allowing individual
request-response pairs to be followed. The response's tag has been
added to each constructor `ReqResult tag a` type.

- **Improved Unicode support** Thanks to Schell Scivally
(https://github.com/schell) for identifying and fixing a UTF-8
encoding bug!

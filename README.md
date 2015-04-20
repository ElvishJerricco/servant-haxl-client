# servant-client

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

This library lets you automatically derive Haskell functions that let you query each endpoint of a *servant* webservice.

## Example

``` haskell
type MyApi = "books" :> Get [Book] -- GET /books
        :<|> "books" :> ReqBody Book :> Post Book -- POST /books

myApi :: Proxy MyApi
myApi = Proxy

getAllBooks :: BaseUrl -> EitherT String IO [Book]
postNewBook :: Book -> BaseUrl -> EitherT String IO Book
-- 'client' allows you to produce operations to query an API from a client.
(getAllBooks :<|> postNewBook) = client myApi
```

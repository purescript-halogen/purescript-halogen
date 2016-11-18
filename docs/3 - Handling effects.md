# Handling effects

Halogen components have no built-in mechanisms for dealing with effects during query evaluation. That doesn't mean that they _can't_ have effects, only that there is no implicit allowance for them. They're made explicit in the usual way: via the type signature.

Let's take another look at the type of the button component from the last chapter:

``` purescript
myButton :: forall m. H.Component HH.HTML Query Message m
```

The `m` parameter we left as polymorphic here is our means of introducing effect handling into a component `eval` function.

## Making an AJAX request

It's occasionally useful to be able to fetch data from an API, so let's use that as an example. We're going to make use of the `affjax` library as it provides a nice `Aff`-based interface for AJAX requests. Our data source will be GitHub's user API:

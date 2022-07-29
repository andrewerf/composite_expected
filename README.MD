# Composite Expected
## Intro

This is a proof-of-concept project aiming to show a useful way
to do error handling without exceptions. The core idea is that
we want to avoid code like that (which could be implemented with simple expected):

```c++
auto res1 = f();
if ( !res1 )
{
    ...
}

auto res2 = g( res1 );
if ( !res2 )
{
    ...
}

auto res3 = t( res2 );
........
```

We rather want to build execution pipeline and then process possible errors. 
It becomes possible with the proposed composite-expected:

```c++
auto res = f() >> g >> t;
if ( !res )
{
    std::visit( [] ( const auto& error ) { ... }, res.error() );
    ...    
}
........
```

## How it works

The composite-expected is based on the simple idea: let us store more
than one possible error. `composite_expected` has `n` template parameters
(where `n > 0`), first parameter is type of the stored value and the rest are
possible errors (index of the actual error could be taken via `errorIndex()`).
Let's then define function:
```
>> :: composite_expected<T, Errs...> 
        -> ( T -> composite_expected<T2, Errs2...> ) 
            -> composite_expected<T2, Errs..., Errs2...>
```

(This is actually a member function, but here Haskell type signature is used for the ease of notation.)

The above-mentioned signature should be understood as:
the function `>>` taking `composite_expected` with value type `T` and
error types `Errs...` as first argument, and functor which maps `T` to
the `composite_expected` with value type `T2` and error types `Errs2...` as 
second argument.
Function `>>` does simply check whether first argument contains value or error and then:
1) If it contains value, then `>>` returns new instance of `composite_expected`
containing result of invoking the functor.
2) Otherwise, it returns new instance of `composite_expected` containing the same error.

Such behaviour is possible because returned `composite_expected` is the union of all possible errors.

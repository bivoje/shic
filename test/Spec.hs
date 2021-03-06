{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

{-|
All the quickcheck spec modules follow some convention.

Instanciation of 'Arbitrary' class should ensure that
@arbitrary :: Gen TYPE@ covers full-domain of the type.
Which means that variable @x@ of type @TYPE@ that never
generated by @arbitrary :: Gen TYPE@ is disallowed.
The range of @arbitrary :: TYPE@ be equal to range of
all the constructors of the type.

If more specified generator is needed, targeting only
subset of the type domain, define a generator function.
Generator functions should be prefixed with @gen@ keyword.

There might be some @gen@ prefixed functions that covers
full-domain. They exist for convenience.

-}
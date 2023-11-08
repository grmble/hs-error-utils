# Error Utils


Repo for 2 haskell libraries:

* fail-utils: utils for MonadFail, only needs base
* fused-errorutils: fail-utils re-exported, with utility carrier
  for conversion to the Throw effect.  Also same utility functions
  for the Throw effect, specialised for Text instead of String

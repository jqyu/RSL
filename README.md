# RSLite

Heavily simplified version of [RSL](https://github.com/jqyu/RSL)

Forgoes memoization, custom user environment, and state store.

Utilizes `Aff` instead of `Eff` monad in order to simplify the asynchronous aspects, since we don't have decent access to thread locks and mutexes.

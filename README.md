bazooka
=====

a parser combinator library

use as dependency (rebar3)
-----
1. rebar.config:

```erlang
{deps, [
  {bazooka, {git, "https://github.com/kevindotklein/bazooka.git", {branch, "main"}}}
]}.
```

2. shell:

```
$ rebar3 get-deps
```

compile and run tests
-----
1. compile:

```
$ rebar3 compile
```


2. run tests:

```
$ rebar3 eunit
```
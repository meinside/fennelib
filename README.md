# fennelib

Personal fennel library for studying and tinkering.

Some functions will be just (re)implementations of other Lispy languages'.

## How to use

Place files in a sub directory like `./fennellib/`, and use them:

```fennel
; import modules
(local c (require :fennelib.collections)) ; fennelib/collections.fnl

(c.map #(* 2 $1) [1 2 3 4 5])
```

## Modules

### collections.fnl

Functions for handling collections.

### test.fnl

Functions for testing and debugging.

### (more to be added)

## Todo

- [ ] Add tests for each function

## License

MIT


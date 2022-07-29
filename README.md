# fennelib

Personal fennel library for studying and tinkering,

which aims to be a general toolset for ordinary tasks.

Some functions will be just (re)implementations of other Lispy languages'.

## How to use

Place files in a sub directory like `./fennellib/`,

or add this repository as a submodule like:

```bash
git submodule add https://github.com/meinside/fennelib
```

and use them:

```fennel
; import specific modules
(local c (require :fennelib.collections)) ; fennelib/collections.fnl

(c.map #(* 2 $1) [1 2 3 4 5])
```

or import all functions with:

```fennel
(local f (require :fennelib)) ; fennelib/init.fnl

(f.prettify {:a 1 :b 2 :c {:d 4 :e 5}})
```

## Modules

### collections.fnl

Functions for handling collections.

### convert.fnl

Functions for converting various things.

Supported types so far:

- [x] JSON string <-> table
- [x] CSV string -> table
- [ ] and other things

### http.fnl

Functions for HTTP requests.

Supported methods so far:

- [x] HTTP GET
- [x] HTTP POST with body
- [ ] and other things

### num.fnl

Functions for handling numbers. (includes original math functions)

### test.fnl

Functions for testing and debugging.

### (more to be added)

## Dependencies

### luarocks packages

- lua-http ($ luarocks install http)
- json-lua ($ luarocks install json-lua)
- ftcsv ($ luarocks install ftcsv)

## Todo

- [ ] Add tests for each function

## License

MIT


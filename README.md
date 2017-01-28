# VCR

Store and replay results of http calls for easier testing of external services.

## Installation

The library is not on quicklisp yet. To use it just clone it inside a directory from which ASDF can load it. Then run `(ql:quickload :vcr)` to experiment with it in the command line or include it in the `:depends-on` list inside your application's asd configuration file.

## Configuration

You can set the default directory (called "\*shelf\*") with the following command:

```
(setf vcr:*shelf*
      (asdf:system-relative-pathname :my-app :t/cassettes/))
```

(where `:my-app` is the name of your application. duh...) 

The default request function is `drakma:http-request`. However, if you want, you can set a different one with:

```
(setf vcr:*original-fn-symbol* 'dex:get)
```

## Usage

Inside your app.

```
(defpackage :my-app
  (use :cl :vcr))

(with-vcr "my-super-awesome-tape-name"
  (drakma:http-request "http://example.com"))
```

The first time drakma will make a request to "example.com", but if you call the same address withing the same tape again, it will be fetched from the cache.

*NB! When you are using `drakma:http-request` inside the lexical clojure of `with-vcr` only the content, response code and headers will be returned no matter if it actually makes a requests or hits the cache.* This is a temporary solution and probably will be fixed in the future.

## Running tests

VCR uses prove as a testing framework.

To run the tests run `(asdf:test-system :vcr)`.

## Issues / Contribution

If you have issues - open a github issue or contact me at `tsikov AT gmail DOT com`
If you want to contribute - open an issue or make a PR. Thanks!

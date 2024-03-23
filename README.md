# Experimental Effects-based Wonderful Webserver

Currently an experiment.

## Eeww usage

Create a `certs/` dir to store PEM files for LetsEncrypt, and then:

```
eeww --email avsm@recoil.org --org OCaml --domain ocaml.org --prod 
# optionally: --site site/ --certs-dir certs/
```

(Remove the `--prod` to use the staging endpoint while testing).

This will change to a config file soon.



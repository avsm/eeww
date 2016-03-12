#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

module Config = struct
  include Config_default
  let vars =
    [ "NAME", "pbkdf";
      "VERSION", Git.describe ~chop_v:true "master";
      "MAINTAINER", "Alfredo Beaumont <alfredo.beaumont\\@gmail.com>" ]
end

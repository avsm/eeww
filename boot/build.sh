#!/bin/sh -e

WITH_OCAML=0
NEEDED_OCAML="5.0.0+dev10-2022-11-25"

MODE=$1
shift

if [ "$MODE" != "native" ]; then
  echo Forcing a local compiler to be built for $MODE
  WITH_OCAML=1
elif [ -x "$(command -v ocamlc)" ]; then
  if [ "$(ocamlc -version | tr -d '\015')" != "$NEEDED_OCAML" ]; then
    echo "OCaml compiler detected is not $NEEDED_OCAML, so building local version"
    WITH_OCAML=1
  else
    echo "Using system OCaml $NEEDED_OCAML compiler"
  fi
else
  if [ -x $(pwd)/_obj/bin/ocamlc ] ; then
    export PATH=$(pwd)/_obj/bin:$PATH
  else
    echo "OCaml compiler not detected, building a local version of $NEEDED_OCAML"
    WITH_OCAML=1
  fi
fi

if [ -x "$(command -v gmake)" ]; then
  echo 'Using gmake instead of make'
  MAKE=gmake
else
  MAKE=make
fi

if [ $WITH_OCAML -eq 1 ]; then
  PREFIX="`pwd`/_obj"

  cd boot/ocaml

  if [ -z "$WINPORT" -a -n "$COMSPEC" ]; then
    # If the current environment provides gcc (could be Cygwin, MSYS2, LXSS) then don't select a
    # Windows port.
    if [ ! -x "$(command -v gcc)" ]; then
      if [ -x "$(command -v cl)" ]; then
        if cl /nologo /? | grep -q ' for \(AMD64\|x64\)'; then
          WINPORT=msvc64
        else
          WINPORT=msvc
        fi
      else
        # Prefer a 64-bit mingw compiler if we're on a 64-bit machine
        if [ -x "$(command -v x64_64-w64-mingw32-gcc)" ]; then
          if [ -x "$(command -v i686-w64-mingw32-gcc)" ]; then
            if [ "$PROCESSOR_ARCHITEW6432" != "AMD64" -a "$PROCESSOR_ARCHITECTURE" != "AMD64" ]; then
              WINPORT=mingw
            else
              WINPORT=mingw64
            fi
          else
            WINPORT=mingw64
          fi
        elif [ -x "$(command -v i686-w64-mingw32-gcc)" ]; then
          WINPORT=mingw
        fi
      fi
    fi
  fi

  if [ -z "$WINPORT" ]; then
    PRE_WORLD=
    POST_WORLD=
  else
    PRE_WORLD=flexdll
    POST_WORLD=flexlink.opt

    # mingw ports need C++ linking support for ocaml-mccs to work
    if [ ! -e flexdll/Makefile -a "${WINPORT%64}" = "mingw" ] ; then
      git submodule update --init flexdll
      cd flexdll
      git fetch dra27 || git remote add dra27 https://github.com/dra27/flexdll.git -f
      git checkout dra27/linking-c++
      cd ..
    fi

    cp config/s-nt.h byterun/caml/s.h
    cp config/m-nt.h byterun/caml/m.h
  fi

  case MODE in
  bytecode-only)
    if [ -z "$WINPORT" ]; then
      ./configure --prefix $PREFIX --no-native-compiler
    fi
    WORLD=world
    ;;
  flambda)
    if [ -z "$WINPORT" ]; then
      ./configure --prefix $PREFIX --flambda
    else
      sed -i -e "/^FLAMBDA=/s/false/true/" config/Makefile
    fi
    WORLD=world.opt
    ;;
  *)
    WORLD=world.opt
    if [ -z "$WINPORT" ]; then
      ./configure --prefix $PREFIX
    fi
    ;;
  esac
  $MAKE -j $PRE_WORLD $WORLD $POST_WORLD
  $MAKE install
  export PATH=$PREFIX/bin:$PATH
  cd ../..
fi

cd boot/dune
$MAKE release
cp _build/install/default/bin/dune $PREFIX/bin/dune

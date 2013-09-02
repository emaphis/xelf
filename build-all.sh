#!/bin/bash
sbcl --load build-unix.lisp
wine sbcl --load build-wine.lisp

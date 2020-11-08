# ithkuil-mode
Major mode for the Ithkuil constructed language.

This repo is still in its early stages, so it doesn't have many features and it's probably buggy.

## Requirements

`ithkuil-mode` requires [Helm](https://emacs-helm.github.io/helm/).

## Basic usage

<img src="/assets/ithkuil-mode-screenshot.png" />

`ithkuil-mode` provides utilities to manipulate formatives in Ithkuil 2020, as it existed in [version 0.16](http://www.ithkuil.net/morpho-phonology_v_0_16.pdf).

You can write out an Ithkuil formative using the formative notation, for example:

> S1/PRC/EXS-ExampleRoot-DYN/BSC-UPX/SEP/DEL/CSL/M/NRM

or you can use the command `ithkuil-insert-template` (`C-c C-i`) to insert a sample formative which you can then edit.

`ithkuil-mode` provides a series of functions for changing the values of various categories.

`C-c C-t` (`ithkuil-set-category-at-point`) prompt you to set the value of the category under the cursor, and uses Helm to provide a searchable list of valid values.

A set of additional functions allow you to set the value of any category while your cursor is anywhere in the formative:

    C-c C-s s: ithkuil-set-stem
    C-c C-s v: ithkuil-set-version
    C-c C-s c: ithkuil-set-context
    C-c C-s f: ithkuil-set-function
    C-c C-s p: ithkuil-set-specification
    C-c C-s m: ithkuil-set-membership
    C-c C-s t: ithkuil-set-structure
    C-c C-s e: ithkuil-set-extension
    C-c C-s a: ithkuil-set-affiliation
    C-c C-s p: ithkuil-set-perspective
    C-c C-s n: ithkuil-set-essence

`ithkuil-mode` can parse basic formatives and understand when they are not formatted correctly.

As a proof of concept, `ithkuil-mode` has a function to convert the `Vv` slot into actual Ithkuil, but only for the `Vv` slot.

## Features that would be nice, but aren't yet supported

- [ ] Support Unicode, so users are not limited to ASCII-only roots.
- [ ] Function to encode formatives into Ithkuil (not just for `Vv` slot), and a decoding function.
- [ ] Parse more complex formatives, including affixes and incorporated stems.
- [ ] Include a full list of roots and affixes, with descriptions. Allow users to insert roots/affixes by searching for words in their descriptions.
- [ ] Connect stems to roots, so when a user tries to set the value of a stem, `ithkuil-mode` shows the user what each stem means for that root.
- [ ] Connect degrees to affixes in the same way as stems are connected to roots.
- [ ] Support the ability to write partial formatives and have functions that help the user add in the missing parts.
- [ ] Show some sort of visual indicator, like translucent boxes around each slot.

---
title: A first exercise
date: 2017-09-19
authors: dlaing
project: reflex
extra-css: /css/reflex/basics-exercises/solutions/ex00.css
extra-js: /js/reflex/basics-exercises/solutions.min.js
---

## Getting set up

Checkout a copy of the [reflex platform](https://github.com/reflex-frp/reflex-platform), read any OS compatibility notes from the README that apply to you, and then run the `./try-reflex` script to install Nix for you.
This should also setup the binary caches for you, which will save you lots of time (although it may not feel like it the first time this runs).

Once you have done that, or if you have Nix installed and the `reflex` binary caches setup already, check out this repository and open a `nix-shell` in the exercises directory:
```
> git clone https://github.com/qfpl/reflex-tutorial
> cd reflex-tutorial/code/exercises
> nix-shell
```

## Poking around with the first exercise

There is a script in here for each of the exercises (and more will be added as the series continues).

For now, run:
```
nix-shell> ./ex00.sh
```
and then visit `http://localhost:8080` in your browser.

This script is running `ghcid` along with some magic to trigger a reload in the browser tab when the Haskell or CSS files change.

In this case the Haskell looks like this:
```haskell
-- src/Ex00/Exercises.hs
ex00 :: Reflex t 
     => Event t () 
     -> Event t () 
     -> (Event t Text, Event t Text)
ex00 eFirst eSecond =
 ( "Boring"        <$ eFirst
 , "Really boring" <$ eSecond
 )
```
but you don't need to understand any of the `reflex` specific bits.

If you can spot the `Text` literals in there then you are ready for this exercise.

The CSS looks like this:
```css
/* css/exercises/ex00.css */
.ex00 {
  color: red;
}
```

You should open these files in an editor and tinker with the strings in the Haskell program and the color in the CSS class.

After you save you'll either get errors in the terminal running the `ex00.sh` script or the browser tab will reload with the new content.

Click around in the browser tab after each save to convince yourself that this is happening.

This is still fairly new technology, I'm new to using it, and I'm not sure if what I'm doing behind the scenes counts as abusing the technology.
For any of the above reasons, you might have to restart the script or manually reload the tab from time to time.
I'll try to make this more reliable over time.

## The goal

The goal of this exercise was to get you comfortable with the workflow, but some people might need more than that to feel a sense of accomplishment.

If that's the case then I'm happy to accommodate you: your new goal is to edit the above files and to try to end up in a state where you have something in your browser that behaves like this:

<div id="ex00"></div>


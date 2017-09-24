---
title: "Reflex exercises: Getting started"
date: 2017-09-19
authors: dlaing
project: reflex
extra-css: /css/reflex/basics-exercises/solutions/ex00.css
extra-js: /js/reflex/basics-exercises/solutions.min.js
---

## Getting set up with Nix

This is all set up with Nix. 
If you don't have Nix installed, you can get going with the `try-reflex` script from the `reflex-platform` repository:
```
> git clone https://github.com/reflex-frp/reflex-platform
> git checkout b7c00b35
> cd reflex-platform
> ./try-reflex
```
and then you'll be able to use the `work-on` script later on, which is pretty handy.

(The specific commit is referenced there so that it matched the version of the `reflex-platform` that I used in my Nix files)

You could also get Nix set up by doing:
```
> sudo mkdir /nix
> sudo chown myuser /nix
> curl https://nixos.org/nix/install | sh
```
as per our post on [Getting started with Nix](https://blog.qfpl.io/posts/nix/getting-started-with-nix/).

There are some platform-specific notes on the `reflex-platform` page that imply that things might be broken on Arch Linux and things might need some tweaking on Linux Mint.

I've also had to fiddle around to get `reflex` working on OS X.
I'll try again soon and add some notes here once I have some more information on what is going on there.

### Binary cache setup

You probably want to get hold of some of the dependencies in binary form, rather than getting hold of them in source form and building them.
It'll save you a lot of time.

If you're on NixOS, you can add the `reflex` binary caches by adding 
```
nix.binaryCaches = [ "https://cache.nixos.org" "https://nixcache.reflex-frp.org" ];
nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
```
to `/etc/nixos/configuration.nix` and then running `sudo nixos-rebuild switch`.

If you're not on NixOS, you can add 
```
binary-caches = [ "https://cache.nixos.org" "https://nixcache.reflex-frp.org" ];
binary-cache=public-keys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
```
to `/etc/nix/nix.conf` and you should be good to go.

## Getting set up with this tutorial

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

There might be some worrying looking output in the terminal that is running the script, along the lines of `threadWait: invalid argument (Bad file descriptor)` or `ConnectionClosed`, but those messages usually won't cause trouble.

## The goal

The goal of this exercise was to get you comfortable with the workflow, but some people might need more than that to feel a sense of accomplishment.

If that's the case then I'm happy to accommodate you: your new goal is to edit the above files and to try to end up in a state where you have something in your browser that behaves like this:

<div id="ex00"></div>


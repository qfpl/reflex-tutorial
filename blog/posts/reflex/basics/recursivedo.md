---
title: RecursiveDo
date: 2017-09-05
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

`Behavior`s are specified at all points of time

 This is also true of `Dynamic`s

`Event`s are only specified at some unique points of time

So all `Behavior`s have a value before any of the `Event`s fire

It is perfectly valid to have `Event`s that build a `Behavior` and also depend on the `Behavior`

This is where the one frame delay comes in - we are using the old value of the `Behavior` to build the new value of the `Behavior`

This means some dependencies can be loops, which is fine

We just need a way to specify them

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => 
           Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter        eAdd eClear =  do



  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  pure dCount
```

##

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter        eAdd eClear =  do



  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  pure dCount
```

##

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear =  do



  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  pure dCount
```

##

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear =  do
  let dLimitOK = (<) <$> dCount <*> dLimit


  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  pure dCount
```

##

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear =  do
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  pure dCount
```

##

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear =  do
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAddOK
    , const 0 <$ eClear
    ]
    
  pure dCount
```

##

```haskell
{-# LANGUAGE RecursiveDo #-}
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear =  do
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAddOK
    , const 0 <$ eClear
    ]
    
  pure dCount
```

##

```haskell
{-# LANGUAGE RecursiveDo #-}
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear = mdo
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAddOK
    , const 0 <$ eClear
    ]
    
  pure dCount
```

##

```haskell
{-# LANGUAGE RecursiveDo #-}
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear = mdo
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAddOK
    , const 0 <$ eClear
    ]
    
  pure dCount
```


<div id="basics-recursiveDo-3"></div>

```haskell
limit :: (Reflex t, MonadFix m, MonadHold t m) 
      => Event t () 
      -> Event t ()
      -> Event t ()
      -> m (Dynamic t Int)
limit eStart eAdd eClear = do
  eLoadLimit <- performEvent (loadLimitDb <$ eStart)

  dLimit <- foldDyn ($) 5 . mergeWith (.) $ [
      const   <$> eLoadLimit
    , (+ 1)   <$  eAdd
    , const 0 <$  eClear
    ]
    
  performEvent_ (saveLimitDb <$> update dLimit) 
    
  return dLimit
```

<div id="basics-recursiveDo-1"></div>


```haskell
data Settings =
  Settings {
    settingLimit :: Int
  , settingStep  :: Int
  }
```

```haskell
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Settings
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dSettings eAdd eClear = mdo
  let dLimit      = settingsLimit <$> dSettings
      dStep       = settingsStep  <$> dSettings
      check c s l = c + s <= l
      dLimitOK    = check <$> dCount <*> dStep <*> dLimit
      eAddOK      = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+  )   <$> tag (current dStep) eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter (Settings <$> dLimit <*> dStep) eAdd eClear
```

<div id="basics-recursiveDo-4"></div>

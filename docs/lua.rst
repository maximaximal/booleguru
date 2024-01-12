##########################################################################################
Lua
##########################################################################################

You can write your own extension scripts, transformers, or formula builders,
using Lua!

Lua Scripts returning Formulas
------------------------------

Create a `f.lua` file, which contains a Lua function named `f`::

  function f(var_name)
    local a = v(var_name)
    local b = v(var_name .. '_2')
    return a * b
  end

  return f

Then, add the path that contains the `f.lua` file to the `BOOLEGURU_LUA_PATH`
environment variable, e.g. using the following::

  export BOOLEGURU_LUA_PATH=$(pwd)

Then you are good to go! Startup booleguru and call the function you just
created using the `:(f 'test')` syntax::

  ./booleguru ':(f "test")'

Now, booleguru loads your script which it finds from `f.lua` because you set the
correct `BOOLEGURU_LUA_PATH`, then you call it inside the script context in
`:(...)` using its name `f`. Booleguru returns the following::

  test & test_2

Randomness in Lua Scripts
-------------------------

If you require some random value in Lua, we recommend to give the seed as a
parameter to your function. This makes experiments repeatable. You can add a
`seed` parameter as the last parameter to your function and add use the
following Lua snippet in the beginning of your formulas for convenience::

  if seed ~= nil then
    seed = 42
  end
  math.randomseed(seed)

This script uses the provided seed or fixes it to 42.

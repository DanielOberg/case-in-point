Case In Point (cip)
===================

What Case In Point (cip) does is to extract your examples, runs them and, if they are correct, 
calls git. If an example fails then it tells you which example that failed and what the result 
was.


Motivation
----------
Let's face it, Hackage isn't exacly filled to the brim with examples in
the docs. I find this sad. 

A small example that capture the essence of a function is golden when
skimming though the docs. 

What if we could help the developers skimming though or code and at the
same time help ourselves? Why can't the example we give to other developers be no different than 
any other testcase and when we are at it, why not let it be a 
way to actually find out what result your function is actually giving
you instead of having to load up ghci?

Better documentation and more testcases, what's not to like?

Example
-------
Very, very simple example:

    --   >>> map show [1,2,3]
    --   ["1", "2", "3"]
    --
    --   >>> map show [2,1,3]
    --   ["2", "1", "3"]
    --

Notice the *required last empty comment line!*

Use the tool like this

    > cip file_to_check_examples.hs

Used together with tomdoc[^2] like comments make for some sweet code:

    -- | Duplicate some text an abitrary number of times.
    --
    -- text  - The String to be duplicated.
    -- count - The Integer number of times to duplicate the text.
    --
    -- Examples
    --
    --   > multiplex "Tom" 4
    --   "TomTomTomTom"
    --
    -- Returns the duplicated String.
    multiplex text count = concat $ take count $ repeat text

or if you prefer more haddock compatible syntax (the only thing that
really matters is the example):

    -- | Simple function that adds a equality sign between two strings
    --
    -- Examples
    --
    --   >>> combineIntoEqualTest "cat" "dog" 
    --   "cat == dog"
    --
    combineIntoEqualTest :: [Char] -- ^ example
                         -> [Char] -- ^ expected result
                         -> [Char] -- ^ equality sign inserted between the strings
    combineIntoEqualTest example expected_result = example ++ " == " ++ expected_result



Workflow example
-----------------

1. Write your projects README file first and put it in a git repo. This is based upon 
   Readme Driven Development[^1].
2. Write a comment that describe a function.
3. Add an example for how it is supposed to look (in the comment
   itself) and its expected return value.
4. Write the function.
5. Run cip on the source. If the examples are correct it will call git
   for you.


Syntax for examples
-------------------

The line MUST start with "--".

There MUST be no space before and there MUST be atleast one space after
(like this: "\n-- ").

There SHOULD be three spaces after, not one.

It MUST have a > after. It SHOULD have three 
(eg. "\n--   >>> testfunction 123")

The expected result MUST be on the following (comment) line and end with an empty
line.


Limitations
------------
Only supports two lines at the moments. Haven't decided the syntax for 
multiline support yet (it will be implemented).


[^1]: http://tom.preston-werner.com/2010/08/23/readme-driven-development.html
[^2]: http://tomdoc.org/


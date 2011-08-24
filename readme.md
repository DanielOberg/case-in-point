Case In Point (cip)
===================

What it does is to extract your examples, runs them and if it passes it 
commits the changes to your repository. If an example fails then it
tells you which example that failed.

It is meant for those projects where you want to get stuff done as 
quickly as possible in small projects.

Example
-------

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


Use the tool like this

    > cip file_to_check_examples.hs


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

Workflow
--------
It works like this:

1. Write your projects README file first. This is based upon 
   Readme Driven Development[^1].
2. Write a comment that describe a function.
3. Add an example for how it is supposed to look (in the comment
   itself) and its expected return value.
4. Does the example look good (short and sweet)? Otherwise back to the
   drawingboard.


Limitations
------------
Only supports two lines at the moments. Haven't decided the syntax for 
multiline support yet (it will be implemented).


[^1]: http://tom.preston-werner.com/2010/08/23/readme-driven-development.html
[^2]: http://tomdoc.org/


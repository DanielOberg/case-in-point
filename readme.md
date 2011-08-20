Test And Commit
===============

Test And Commit uses something similar that can only be described as
Test Driven Developments little brother: Example Driven Development.

It is meant for those projects where you want to get stuff done as 
quickly as possible in small projects.

It works like this:

1. Write your projects README file first. This is based upon 
   Readme Driven Development[^1].
2. Write a comment that describe a function.
3. Add an example for how it is supposed to look (in the comment
   itself).
4. Does the example look good (short and sweet)? Otherwise back to the
   drawingboard.

As previously stated Test And Commit is a tool for Example Driven 
Development in Haskell.

What it does is to extract your examples, runs them and if it passes it 
commits the changes to your repository. If an example fails then it
tells you which example that failed.

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

Use the tool like this

    > testandcommit test.hs
    git commit -am "PASSED: multiplex 'Tom' 4"
       or if the code snippet fails:
    FAILED: multiplex "Tom" 4
    Result was: "Tom Tom Tom Tom"

[^1]: http://tom.preston-werner.com/2010/08/23/readme-driven-development.html
[^2]: http://tomdoc.org/


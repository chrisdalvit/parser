# Read Me

This project implements a recursive descent parser in Haskell [(Graham Hutton and Erik Meijer, 1998)][1] for term rewrite systems ([TRS][2]'s). Term rewrite systems are a computation model used in theoretical computer science and the fundation for functional programming. 

With this commandline tool you can parse single terms, rules or whole term rewrite systems. TRS's can be read from files and single terms can be evaluated with a TRS. 
For a given precedence it's possible to check if a TRS is terminating according to the lexicographic path order (LPO). The tool can also check if a TRS is pattern-disjoint.

You can use the tool by simply cloning the code, compile it with `ghc` and execute the output file.

# Commands
Commands begin with `:` followed by the command name and space separated arguments. Terms, TRS's and precedences can have names. Names can be used in other commands to refer to a term, TRS or precedence.

* `:term NAME TERM`
    
    parses `TERM` - if possible - into a term and stores it into a variable `NAME`
* `:trs`

    reads multiple rules from the input and parses them into a set of rules (TRS). The input is terminated by entering a empty line
* `:trsfile FILE NAME`

    reads the `FILE`, parses a TRS and stores it in a variable `NAME`. The file has to contain every rule in a new line.
* `:lpo TRS PRED`

    Checks LPO termination of `TRS` for a given precedence `PRED`.
* `:pred NAME F1>F2 F3>F4 ...`

    Read a precedence `F1>F2 F3>F4 ...` and store it under `NAME`. The pairs `Fi>Fj` can not contain spaces and each pair has to be space separated.
* `:pd TRS`

    Check if `TRS` is pattern disjoint. 
* `:evalfile FILE`

    Evaluates file `FILE`. The file must contain a set of rules in the first part and a term in the second part. Both parts are separated by a blank line.
* `:printvars` 
    
    prints all the variable names with the corresponding values
* `:q` 
    
    quits the tool

# Syntax
* `Term` can either be a `Var` or a `Func`. Variables have to be ASCII lowercase characters. Functionsymbols must have at least one alpha-numeric character. Example:

    ````
    f42(c,g(y)) 
    <==> 
    Func "f42" [ Var "c", Func "g" [Var "y"] ]
    ````

* Rules contain a left- and righthandside. The lhs has to be a `Func` and the rhs is a `Term`. All variables in the rhs must also occur in the lhs.

* TRS is a list of rules, with additional checking e.g. that no conflicting function arities occur between the rules.


[1]:http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
[2]:https://en.wikipedia.org/wiki/Rewriting

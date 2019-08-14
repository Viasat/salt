# salt

"S-expressions for Actions with Logic Temporal"

An experimental tool to convert a subset of Clojure into TLA+. Although it is experimental it has been used to produce real, useful TLA+ specifications.

You might want to use this if you know Clojure and are writing TLA+ specifications. Specifically it provides:

* an interactive REPL based authoring experience
* facilities for automated unit tests of invariants and actions
* automatic formatting of resulting TLA+ code
* a mapping from TLA+ to Clojure concepts that may facilitate learning TLA+

To use it:
1. Write a specification using salt.
2. Evaluate the salt specification in the REPL as part of the development process.
3. Once it is ready to be run in the TLA+ Toolbox, invoke the salt transpiler to emit TLA+ code.
4. Run the resulting TLA+ specification in the TLA+ Toolbox to assess the temporal properties of the specification.

In your salt specification file require the salt language as:

```(:require [salt.lang :refer :all])```

From the Clojure REPL, transpile a salt source file into a TLA+ file using an incantation like this:

```(spit <tla+-filename> (salt.transpiler/transpile-text (slurp <salt-filename>)))```

Alternatively, the salt transpiler can be invoked from the command line:

```lein uberjar```

```java -cp target/salt-0.0.1-standalone.jar clojure.main -m salt.main <salt-filename>```

# Table of Contents

 * [Language Identifiers](#language-identifiers)  
 * [Clojure to TLA+ Concepts](#clojure-to-tla-concepts)
 * [Standard Modules](#standard-modules)
 * [Docs](#docs)
   * [Primitives](#primitives)
   * [Code Structure](#code-structure)
   * [Arithmetic](#arithmetic)
   * [Specs](#specs)
   * [Logic](#logic)
   * [Temporal Logic](#temporal-logic)
   * [Sets](#sets)
   * [Vectors](#vectors)
   * [Maps](#maps)
   * [Maps As Records](#maps-as-records)
   * [Maps As Tuples](#maps-as-tuples)
   * [Functions](#functions)
 * [Example Salt](#example-salt)
 * [Example TLA+](#example-tla)

# Language Identifiers

The following identifiers are reserved for the salt language.

Identifiers from the Clojure language:

```ns```
```require```
```comment```
```if```
```let```
```cond```
```and```
```or```
```not```
```expt```
```difference```
```union```
```intersection```
```subset?```
```contains?```
```select```
```first```
```rest```
```into```
```str```
```count```
```conj```
```defn```
```def```
```fn```

Identifiers from the Clojure language, whose semantics were modified to match TLA+:

```mod*```
```range*```
```map*```
```every?*```
```get*```

Identifiers that were added specifically to support transpiling, they are neither part of Clojure nor TLA+:

```ALLOW-```
```atomic-```
```CHANGED-```
```line-```
```always-```
```eventually-```
```leads-to-```
```fm-```
```defm-```
```maps-```

Identifiers from the TLA+ language:

```CONSTANT```
```ASSUME```
```VARIABLE```
```UNCHANGED```
```==```
```=>```
```<=>```
```=```
```not=```
```>```
```<```
```>=```
```<=```
```E```
```A```
```WF```
```SF```
```+```
```-```
```*```
```div```
```Nat```
```X```
```UNION```
```SUBSET```
```Cardinality```
```CHOOSE```
```SubSeq```
```SelectSeq```
```Seq```
```EXCEPT```
```DOMAIN```

# Clojure to TLA Concepts

| Clojure | Clojure example | TLA+ | TLA+ example |
| --- | --- | --- | --- |
| set | ```#{1 2}```| set | ```{1, 2}``` |
| vector | ```[1 2]``` | tuple | ```<<1, 2>>``` |
| map | ```{1 10 2 20}``` | function | ```(1 :> 10 @@ 2 :> 20)```|
| map | ```{:a 10 :b 20}``` | record | ```[a \|-> 100, b \|-> 200]``` |
| function | ```(defn Add [x y] (+ 1 2))```| operator | ```Add( x, y ) == x + y```|
| lambda | ```(fn [x] (> x 2))``` | lambda in SelectSeq | ```LAMBDA  x: (x > 2)``` |
| lambda | ```#(* 2 %)``` | lambda in Except| ```@ * 2``` |

# Standard Modules

Portions of some of the TLA+ standard modules have been implemented. They can be referenced from a salt file as follows:

```
  (:require [salt.lang :refer :all]
            [tlaplus.FiniteSets :refer :all]
            [tlaplus.Integers :refer :all]
            [tlaplus.Naturals :refer :all]
            [tlaplus.Sequences :refer :all])
```

The salt transpiler will detect these values in the salt source file and produce the corresponding EXTENDS statement in the TLA+ output.

# Docs

Primitives
----
Integers are represented the same in salt and TLA+:

|  | salt | tla+ |
| --- | --- | --- |
| code | `1` | `1` |
| result | `1` | `1` |

Strings are sequences of characters

|  | salt | tla+ |
| --- | --- | --- |
| code | `"hello"` | `"hello"` |
| result | `"hello"` | `"hello` |

Symbols are identifiers. For example "x" is a symbol in the following:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(let [x 1] x)` | `LET x == 1`<br>`IN  x` |
| result | `1` | `1` |

Boolean literals:

|  | salt | tla+ |
| --- | --- | --- |
| code | `true` | `TRUE` |
| result | `true` | `TRUE` |



|  | salt | tla+ |
| --- | --- | --- |
| code | `false` | `FALSE` |
| result | `false` | `FALSE` |

Code Structure
----
Let statements are used to establish bindings between symbols and values:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(let [x 1 y 2] (+ x y))` | `LET x == 1`<br>`    y == 2`<br>`IN  x + y` |
| result | `3` | `3` |

Conditional statements are expressed as:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(if true 100 0)` | `IF  TRUE`<br>`THEN 100`<br>`ELSE 0` |
| result | `100` | `100` |

To perform many checks on a value use 'cond':

|  | salt | tla+ |
| --- | --- | --- |
| code | `(let`<br>` [x 3]`<br>` (cond (= x 1) true (= x 2) true (= x 3) 7 :default false))` | `LET x == 3`<br>`IN  CASE`<br>`    (x = 1) -> TRUE`<br>`    [] (x = 2) -> TRUE`<br>`    [] (x = 3) -> 7`<br>`    [] OTHER -> FALSE`<br>`    ` |
| result | `7` | `7` |

Arithmetic
----
Arithmetic on integers:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(+ 1 2)` | `1 + 2` |
| result | `3` | `3` |

Subtraction

|  | salt | tla+ |
| --- | --- | --- |
| code | `(- 3 2)` | `3 - 2` |
| result | `1` | `1` |

Multiplication

|  | salt | tla+ |
| --- | --- | --- |
| code | `(* 3 2)` | `3 * 2` |
| result | `6` | `6` |

Integer division

|  | salt | tla+ |
| --- | --- | --- |
| code | `(div 10 2)` | `10 \\div 2` |
| result | `5` | `5` |

Integer division results are truncated

|  | salt | tla+ |
| --- | --- | --- |
| code | `(div 9 2)` | `9 \\div 2` |
| result | `4` | `4` |

Compute the modulus of two numbers.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(mod* 10 3)` | `10 % 3` |
| result | `1` | `1` |

Compute exponentiation

|  | salt | tla+ |
| --- | --- | --- |
| code | `(expt 2 3)` | `2^3` |
| result | `8` | `8` |

Refer to the set of natural numbers. The clojure version of this uses a very small set of natural numbers by default.  NOTE: Nat is invoked as a function in clojure so that the upper limit can be dynamically bound, if necessary for testing.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(contains? (Nat) 2)` | `2 \\in Nat` |
| result | `true` | `TRUE` |

Logic
----
Use standard logic operators

|  | salt | tla+ |
| --- | --- | --- |
| code | `(and true false)` | `/\  TRUE`<br>`/\  FALSE` |
| result | `false` | `FALSE` |

The 'or' operator

|  | salt | tla+ |
| --- | --- | --- |
| code | `(or true false)` | `\/  TRUE`<br>`\/  FALSE` |
| result | `true` | `TRUE` |

Specify that if x is true then y must be true as well

|  | salt | tla+ |
| --- | --- | --- |
| code | `(=> true false)` | `TRUE => FALSE` |
| result | `false` | `FALSE` |

Use the TLA+ <=> operator

|  | salt | tla+ |
| --- | --- | --- |
| code | `(<=> true false)` | `TRUE <=> FALSE` |
| result | `false` | `FALSE` |

Check for equality

|  | salt | tla+ |
| --- | --- | --- |
| code | `(= 5 5)` | `5 = 5` |
| result | `true` | `TRUE` |

Equality works on complex types

|  | salt | tla+ |
| --- | --- | --- |
| code | `(= #{"a" "b"} #{"a" "b"})` | `{ "a", "b" } = { "a", "b" }` |
| result | `true` | `TRUE` |

Check for two items not being equal to each other

|  | salt | tla+ |
| --- | --- | --- |
| code | `(not= 1 2)` | `1 # 2` |
| result | `true` | `TRUE` |

Use the standard inequality operators:

|  | salt | tla+ |
| --- | --- | --- |
| code | `[(< 1 2) (<= 1 1) (> 2 1) (>= 2 2)]` | `<< (1 < 2), (1 <= 1), (2 > 1), (2 >= 2) >>` |
| result | `[true true true true]` | `<<TRUE, TRUE, TRUE, TRUE>>` |

Operate on an item that exists in a set

|  | salt | tla+ |
| --- | --- | --- |
| code | `(E [x #{1 3 2}] true)` | `\\E x \\in { 1, 3, 2 } :`<br>`    TRUE` |
| result |   |   |

Specify something is true for all items in a set

|  | salt | tla+ |
| --- | --- | --- |
| code | `(A [x #{1 3 2}] (> x 2))` | `\\A x \\in { 1, 3, 2 } :`<br>`    x > 2` |
| result | `false` |   |

Specs
----
Start a spec with a standard namespace declaration.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(ns`<br>` Buffer`<br>` (:require`<br>`  [salt.lang :refer :all]`<br>`  [tlaplus.Naturals :refer :all]`<br>`  [tlaplus.Sequences :refer :all]))` | `---------------------------- MODULE Buffer ----------------------------`<br>`EXTENDS Naturals, Sequences`<br>` `<br>` ` |
| result |   |   |

Define the CONSTANTS, which serve as a sort of 'input' to the specification and define the scope of the model.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(CONSTANT Clients Servers Data)` | `CONSTANT Clients, Servers, Data`<br>` `<br>` ` |
| result |   |   |

Make assertions about constants

|  | salt | tla+ |
| --- | --- | --- |
| code | `(ASSUME (and (subset? Clients Servers) (< Limit 100)))` | `ASSUME`<br>`    /\  Clients \\subseteq Servers`<br>`    /\  Limit < 100`<br>` `<br>` ` |
| result |   |   |

Define the variables that make up the state:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(VARIABLE messages leaders)` | `VARIABLE messages, leaders`<br>` `<br>` ` |
| result |   |   |

Specify the initial state of variables. This is an addition to TLA+ which does not have an equivalent of the ALLOW- keyword. NOTE: it is not possible to specify general predicates here.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(ALLOW- [messages [] leaders #{}])` | `/\  messages = << >>`<br>`/\  leaders = {}` |
| result | `true` |   |

To indicate allowed state transitions reference variable symbols with a prime suffix. This is an addition to TLA+ which does not have an equivalent to the ALLOW- keyword. NOTE: it is not possible to specify general predicates here.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(ALLOW- [messages' [] leaders' #{}])` | `/\  messages' = << >>`<br>`/\  leaders' = {}` |
| result |   |   |

If there are multiple ALLOW- blocks in a single rule that need to be applied together then wrap them in an atomic- block. NOTE: There is no coresponding TLA+ identifier as the default behavior from TLA+ is to atomically apply all the state changes.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(atomic-`<br>` (and (ALLOW- [messages' []]) (ALLOW- [leaders' #{}])))` | `/\  messages' = << >>`<br>`/\  leaders' = {}` |
| result |   |   |

Indicate variables that are not changed

|  | salt | tla+ |
| --- | --- | --- |
| code | `(UNCHANGED [messages leaders])` | `UNCHANGED << messages, leaders >>` |
| result | `true` |   |

As a departure from TLA+, just the changed variables can be indicated instead. This implies that other variables are unchanged.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(CHANGED- [leaders])` | `UNCHANGED << messages >>` |
| result | `true` |   |

Include horizontal separator lines in the spec to delimit sections.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(line-)` | `--------------------------------------------------------------------------------`<br>` ` |
| result |   |   |

Include comments in the spec.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(comment "this is a single line comment")` | ` `<br>`\\* this is a single line comment`<br>` ` |
| result |   |   |

Comments can be multi-line

|  | salt | tla+ |
| --- | --- | --- |
| code | `(comment "this is a\\nmulti line comment")` | ` `<br>`(*`<br>`this is a`<br>`multi line comment`<br>`*)`<br>` ` |
| result |   |   |

Temporal Logic
----
Say something is always true

|  | salt | tla+ |
| --- | --- | --- |
| code | `(always- (Next) vars)` | `[][Next]_vars` |
| result |   |   |

Say something is eventually true

|  | salt | tla+ |
| --- | --- | --- |
| code | `(eventually- (Done))` | `<>Done` |
| result |   |   |

Say that something being true leads to something else being true

|  | salt | tla+ |
| --- | --- | --- |
| code | `(leads-to- P Q)` | `P ~> Q` |
| result |   |   |

Specify weak fairness

|  | salt | tla+ |
| --- | --- | --- |
| code | `(WF vars (Next))` | `WF_vars(Next)` |
| result |   |   |

Specify strong fairness

|  | salt | tla+ |
| --- | --- | --- |
| code | `(SF vars (Next))` | `SF_vars(Next)` |
| result |   |   |

Sets
----
Set literals are defined as:

|  | salt | tla+ |
| --- | --- | --- |
| code | `#{1 3 2}` | `{ 1, 3, 2 }` |
| result | `#{1 3 2}` | `{1, 2, 3}` |

A sequence of values is defined with range* Note that range* produces a set and is inclusive of the final value to match TLA+ semantics.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(range* 2 5)` | `2..5` |
| result | `#{4 3 2 5}` | `{2 3 4 5}` |

Standard set operations come from the clojure.set namespace

|  | salt | tla+ |
| --- | --- | --- |
| code | `[(union #{1 2} #{3 2})`<br>` (difference #{1 2} #{3 2})`<br>` (intersection #{1 2} #{3 2})]` | `<< ({ 1, 2 } \\union { 3, 2 }), ({ 1, 2 } \\ { 3, 2 }), ({ 1, 2 } \\intersect { `<br>`            3, 2 }) >>` |
| result | `[#{1 3 2} #{1} #{2}]` | `<<{1, 2, 3}, {1}, {2}>>` |

Collapse many sets into one with UNION

|  | salt | tla+ |
| --- | --- | --- |
| code | `(UNION #{#{4 3} #{3 5} #{1 2}})` | `UNION { { 4, 3 }, { 3, 5 }, { 1, 2 } }` |
| result | `#{1 4 3 2 5}` | `{1, 2, 3, 4, 5}` |

The cartesian product of two sets is computed by the 'X' operator

|  | salt | tla+ |
| --- | --- | --- |
| code | `(X #{1 3 2} #{"a" "b"})` | `{ 1, 3, 2 } \\X { "a", "b" }` |
| result | `#{[2 "b"] [3 "a"] [2 "a"] [1 "a"] [1 "b"] [3 "b"]}` | `{<<1, "a">>, <<1, "b">>, <<2, "a">>, <<2, "b">>, <<3, "a">>, <<3, "b">>}` |

Check if a set is a subset of another

|  | salt | tla+ |
| --- | --- | --- |
| code | `(subset? #{1 3} #{1 4 3 2})` | `{ 1, 3 } \\subseteq { 1, 4, 3, 2 }` |
| result | `true` | `TRUE` |

Define all of the sets that can be made from a set of values.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(SUBSET #{1 3 2})` | `SUBSET { 1, 3, 2 }` |
| result | `#{#{} #{3} #{2} #{1} #{1 3 2} #{1 3} #{1 2} #{3 2}}` | `{{}, {1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3}}` |

Check if a item is contained in a set.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(contains? #{1 3 2} 2)` | `2 \\in { 1, 3, 2 }` |
| result | `true` | `TRUE` |

Filter the values in a set to be those matching a predicate

|  | salt | tla+ |
| --- | --- | --- |
| code | `(select (fn [x] (> x 10)) #{15 5})` | `{ x \\in { 15, 5 } :`<br>`    x > 10 }` |
| result | `#{15}` | `{15}` |

Apply a function to all elements of a set

|  | salt | tla+ |
| --- | --- | --- |
| code | `(map* (fn [x] (+ 1 x)) #{1 2})` | `{ 1 + x :`<br>`    x \\in { 1, 2 } }` |
| result |   | `{2, 3}` |

Compute the size of a set

|  | salt | tla+ |
| --- | --- | --- |
| code | `(Cardinality #{20 10})` | `Cardinality( { 20, 10 } )` |
| result | `2` | `2` |

Use the TLA+ CHOOSE operator.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(CHOOSE [x #{1 3 2}] (>= x 3))` | `CHOOSE x \\in { 1, 3, 2 } : (x >= 3)` |
| result | `3` | `3` |

Vectors
----
Clojure vector literals are TLA+ tuples:

|  | salt | tla+ |
| --- | --- | --- |
| code | `[1 "a"]` | `<< 1, "a" >>` |
| result | `[1 "a"]` | `<<1, "a">>` |

Extract the first item from a vector:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(first [1 "a"])` | `Head(<< 1, "a" >>)` |
| result | `1` | `1` |

Extract a value by index. NOTE: the index starts with 1.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(get* [10 20 30] 2)` | `<< 10, 20, 30 >>[2]` |
| result | `20` | `20` |

Produce a new vector containing all but the first item:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(rest [1 "a"])` | `Tail(<< 1, "a" >>)` |
| result | `["a"]` | `<<"a">>` |

Combine the contents of two vectors into a new vector

|  | salt | tla+ |
| --- | --- | --- |
| code | `(into [1 "a"] [2])` | `<< 1, "a" >> \\o << 2 >>` |
| result | `[1 "a" 2]` | `<<1, "a", 2>>` |

Combine two strings, which TLA+ treats as tuples:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(str "hel" "lo")` | `"hel" \\o "lo"` |
| result | `"hello"` | `"hello"` |

Compute the length of a vector or string

|  | salt | tla+ |
| --- | --- | --- |
| code | `[(count [1 "a"]) (count "hello")]` | `<< Len( << 1, "a" >> ), Len( "hello" ) >>` |
| result | `[2 5]` | `<<2, 5>>` |

Extract a subsequence by index from a vector

|  | salt | tla+ |
| --- | --- | --- |
| code | `(SubSeq [10 20 30 40] 2 3)` | `SubSeq(<< 10, 20, 30, 40 >>, 2, 3)` |
| result | `[20 30]` | `<<20, 30>>` |

Filter out the values in a vector

|  | salt | tla+ |
| --- | --- | --- |
| code | `(SelectSeq (fn [x] (> x 2)) [1 2 3 4])` | `SelectSeq(<< 1, 2, 3, 4 >>, LAMBDA  x: (x > 2))` |
| result | `[3 4]` | `<<3, 4>>` |

Add an item to the end of a vector

|  | salt | tla+ |
| --- | --- | --- |
| code | `(conj [1 2] 3)` | `Append(<< 1, 2 >>, 3)` |
| result | `[1 2 3]` | `<<1, 2, 3>>` |

Generate all possible vectors from a set of values.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(Seq #{100 200})` | `Seq( { 100, 200 } )` |
| result | `#{[100] [100 200 200] [200 100 200] [200 200] [200 100 100]`<br>`  [100 200] [] [100 100] [100 200 100] [200 200 200]`<br>`  [100 100 100] [200 200 100] [200] [200 100] [100 100 200]}` | `Seq({100, 200})` |

The idiom of calling 'every?*' with a set as a predicate translates into a corresponding TLA+ idiom for checking a TLA+ tuple.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(every?* #{1 4 3 2 5} [1 3])` | `<< 1, 3 >> \\in (Seq( { 1, 4, 3, 2, 5 } ))` |
| result | `true` | `TRUE` |

Compute all of the indexes present in a vector

|  | salt | tla+ |
| --- | --- | --- |
| code | `(DOMAIN [10 20 30])` | `DOMAIN << 10, 20, 30 >>` |
| result | `#{1 3 2}` | `1..3` |

Maps
----
Use 'maps-' to generate all possible maps for a set of possible keys and a set of possible values.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(maps- #{20 10} #{100 200})` | `[{ 20, 10 } -> { 100, 200 }]` |
| result | `#{{20 100, 10 200} {20 200, 10 100} {20 100, 10 100}`<br>`  {20 200, 10 200}}` | `{ (10 :> 100 @@ 20 :> 100),`<br>`  (10 :> 100 @@ 20 :> 200),`<br>`  (10 :> 200 @@ 20 :> 100),`<br>`  (10 :> 200 @@ 20 :> 200) }` |

Define a map via 'fm-'.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(fm- [a #{20 10}] 30)` | `[a \\in { 20, 10 } \|-> 30]` |
| result | `{20 30, 10 30}` | `(10 :> 30 @@ 20 :> 30)` |

Use 'defm-' to define a map like with 'fm-', but assign the result a name.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(defm- MyMaps [a #{1 2}] (* a 10))` | `MyMaps == [a \\in { 1, 2 } \|-> (a * 10)]`<br>` `<br>` ` |
| result |   |   |

Extract a value by key.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(get* (fm- [a #{20 10}] (* a 10)) 10)` | `[a \\in { 20, 10 } \|-> (a * 10)][10]` |
| result | `100` | `100` |

Maps As Records
----
Map literals whose keys are keywords become TLA+ records, which are a special type of a TLA+ function

|  | salt | tla+ |
| --- | --- | --- |
| code | `{:a 100, :b 200}` | `[a \|-> 100,`<br>` b \|-> 200]` |
| result | `{:a 100, :b 200}` | `[a \|-> 100, b \|-> 200]` |

Access the values in a TLA+ record.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(get* {:a 100, :b 200} :b)` | `[a \|-> 100,`<br>` b \|-> 200].b` |
| result | `200` | `200` |

Use 'DOMAIN' on maps to obtain the keys.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(DOMAIN {:a 100, :b 200})` | `DOMAIN [a \|-> 100,`<br>`        b \|-> 200]` |
| result | `#{:b :a}` | `{"a", "b"}` |

Produce a new TLA+ record with modified values, like assoc-in

|  | salt | tla+ |
| --- | --- | --- |
| code | `(EXCEPT {:a 1, :b 2, :c 3} [:b] 20)` | `[[a \|-> 1,`<br>`  b \|-> 2,`<br>`  c \|-> 3] EXCEPT !["b"] = 20]` |
| result | `{:a 1, :b 20, :c 3}` | `[a \|-> 1, b \|-> 20, c \|-> 3]` |

Produce a new nested TLA+ record with modified values, like assoc-in

|  | salt | tla+ |
| --- | --- | --- |
| code | `(EXCEPT`<br>` {:a {:x 1, :y 10}, :b {:x 2, :y 20}, :c {:x 3, :y 30}}`<br>` [:b :x]`<br>` 200)` | `[[a \|-> [x \|-> 1,`<br>`         y \|-> 10],`<br>`  b \|-> [x \|-> 2,`<br>`         y \|-> 20],`<br>`  c \|-> [x \|-> 3,`<br>`         y \|-> 30]] EXCEPT !["b"]["x"] = 200]` |
| result | `{:a {:x 1, :y 10}, :b {:x 200, :y 20}, :c {:x 3, :y 30}}` | `[ a \|-> [x \|-> 1, y \|-> 10],`<br>`  b \|-> [x \|-> 200, y \|-> 20],`<br>`  c \|-> [x \|-> 3, y \|-> 30] ]` |

Produce a new TLA+ record with new values computed by lambda function, like update-in

|  | salt | tla+ |
| --- | --- | --- |
| code | `(EXCEPT {:a {:x 1 :y 10}`<br>`         :b {:x 2 :y 20}`<br>`         :c {:x 3 :y 30}}`<br>`        [:b :x]`<br>`        #(* % 2))` | `[[a \|-> [x \|-> 1,`<br>`         y \|-> 10],`<br>`  b \|-> [x \|-> 2,`<br>`         y \|-> 20],`<br>`  c \|-> [x \|-> 3,`<br>`         y \|-> 30]] EXCEPT !["b"]["x"] = @ * 2]` |
| result | `{:a {:x 1, :y 10}, :b {:x 4, :y 20}, :c {:x 3, :y 30}}` | `[[`<br>`    a \|-> [`<br>`        x \|-> 1,`<br>`        y \|-> 10],`<br>`    b \|-> [`<br>`        x \|-> 2,`<br>`        y \|-> 20],`<br>`    c \|-> [`<br>`        x \|-> 3,`<br>`        y \|-> 30]] EXCEPT !["b"]["x"] = @ * 2]` |

Produce a new TLA+ record with new values and with lambdas, like combining assoc-in and update-in

|  | salt | tla+ |
| --- | --- | --- |
| code | `(EXCEPT {:a {:x 1 :y 10}`<br>`         :b {:x 2 :y 20}`<br>`         :c {:x 3 :y 30}}`<br>`        [:b :x] #(* % 2)`<br>`        [:a] "new")` | `[[a \|-> [x \|-> 1,`<br>`         y \|-> 10],`<br>`  b \|-> [x \|-> 2,`<br>`         y \|-> 20],`<br>`  c \|-> [x \|-> 3,`<br>`         y \|-> 30]] EXCEPT`<br>`        !["b"]["x"] = @ * 2,`<br>`        !["a"] = "new"]` |
| result | `{:a "new", :b {:x 4, :y 20}, :c {:x 3, :y 30}}` | `[a \|-> "new", b \|-> [x \|-> 4, y \|-> 20], c \|-> [x \|-> 3, y \|-> 30]]` |

Use 'maps-' to generate all possible TLA+ records for pairs of keys and value sets

|  | salt | tla+ |
| --- | --- | --- |
| code | `(maps- [:name #{"bob" "sue"} :age #{20 10}])` | `[name : { "bob", "sue" },`<br>` age : { 20, 10 }]` |
| result | `#{{:name "sue", :age 20} {:name "bob", :age 20}`<br>`  {:name "sue", :age 10} {:name "bob", :age 10}}` | `{[name \|-> "bob", age \|-> 10], [name \|-> "bob", age \|-> 20], [name \|-> "sue", age \|-> 10], [name \|-> "sue", age \|-> 20]}` |

Define a map via 'fm-', will auto-convert to a TLA+ record.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(fm- [a #{"a" "b"}] 30)` | `[a \\in { "a", "b" } \|-> 30]` |
| result | `{"a" 30, "b" 30}` | `[a \|-> 30, b \|-> 30]` |

Maps As Tuples
----
Maps whose keys start with 1 and proceed in increments of 1 are treated as TLA+ tuples.

|  | salt | tla+ |
| --- | --- | --- |
| code | `{1 100, 2 200}` | `<< 100, 200 >>` |
| result | `{1 100, 2 200}` | `<<100, 200>>` |

Use 'DOMAIN' on maps that correspond to TLA+ tuples.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(DOMAIN {1 100, 2 200})` | `DOMAIN << 100, 200 >>` |
| result | `#{1 2}` | `1..2` |

Extract a value from a TLA+ tuple by index. NOTE: the index starts with 1.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(get* {1 100, 2 200} 1)` | `<< 100, 200 >>[1]` |
| result | `100` | `100` |

Define a map via 'fm-', will auto-convert to a TLA+ tuple

|  | salt | tla+ |
| --- | --- | --- |
| code | `(fm- [a #{1 2}] (* a 10))` | `[a \\in { 1, 2 } \|-> (a * 10)]` |
| result | `{1 10, 2 20}` | `<<10, 20>>` |

Functions
----
Define a function using fn. Depending on the context it will be transpiled to different forms.

|  | salt | tla+ |
| --- | --- | --- |
| code | `[(SelectSeq (fn [x] (> x 2)) [1 2 3 4])`<br>` (map* (fn [x] (+ 1 x)) #{1 2})]` | `<< (SelectSeq(<< 1, 2, 3, 4 >>, LAMBDA  x: (x > 2))), (`<br>`{ 1 + x :`<br>`    x \\in { 1, 2 } }) >>` |
| result | `[[3 4] #{3 2}]` | `<<<<3, 4>>, {2, 3}>>` |

Define a new TLA+ operator with defn:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(defn Add [x y] (+ x y))` | `Add( x, y ) ==`<br>`    x + y`<br>` `<br>` ` |
| result |   |   |

Invoke a function as normal:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(Add 1 2)` | `Add(1, 2)` |
| result |   | `3` |

Define a recursive function:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(defn Add [x r] (if (> x 5) (Add (- x 1) (+ r 1)) r))` | `RECURSIVE Add(_, _)`<br>` `<br>`Add( x, r ) ==`<br>`    IF  (x > 5)`<br>`    THEN Add((x - 1), (r + 1))`<br>`    ELSE r`<br>` `<br>` ` |
| result |   |   |

Define a higher-order function:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(defn Work [f a b] (f a b))` | `Work( f(_, _), a, b ) ==`<br>`    f(a, b)`<br>` `<br>` ` |
| result |   |   |

Define functions that take no arguments as usual:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(defn Work [] (union a b))` | `Work == a \\union b`<br>` `<br>` ` |
| result |   |   |

Invoke a function with no arguments.

|  | salt | tla+ |
| --- | --- | --- |
| code | `(Work)` | `Work` |
| result |   |   |

Define TLA+ operators that only rely on constants with def:

|  | salt | tla+ |
| --- | --- | --- |
| code | `(def Work (union A B))` | `Work == A \\union B`<br>` `<br>` ` |
| result |   |   |

Reference a TLA+ operator that only relies on constants

|  | salt | tla+ |
| --- | --- | --- |
| code | `Work` | `Work` |
| result |   |   |




# Example Salt
The following is a full sample salt file:

```
(comment "example spec ported from
https://github.com/tlaplus/Examples/blob/master/specifications/transaction_commit/TwoPhase.tla")

(ns TwoPhase
  (:require [salt.lang :refer :all]))

(CONSTANT RM)

(VARIABLE
 rmState
 tmState
 tmPrepared
 msgs)

(defn Message [] (union (maps- [:type #{"Prepared"}
                                :rm RM])
                        (maps- [:type #{"Commit" "Abort"}])))

(defn TPTypeOk []
  (and (contains? (maps- RM #{"working" "prepared" "committed" "aborted"}) rmState)
       (contains? #{"init" "committed" "aborted"} tmState)
       (subset? tmPrepared RM)
       (subset? msgs Message)))

(defn TPInit []
  (ALLOW- [rmState (fm- [rm RM]
                        "working")
           tmState "init"
           tmPrepared #{}
           msgs #{}]))

(defn TMRcvPrepared [rm]
  (and (= tmState "init")
       (contains? msgs {:type "Prepared"
                        :rm rm})
       (ALLOW- [tmPrepared' (union tmPrepared #{rm})])
       (CHANGED- [tmPrepared])))

(defn TMCommit []
  (and (= tmState "init")
       (= tmPrepared RM)
       (ALLOW- [tmState' "committed"
                msgs' (union msgs #{{:type "Commit"}})])
       (CHANGED- [tmState, msgs])))

(defn TMAbort []
  (and (= tmState "init")
       (ALLOW- [tmState' "aborted"
                msgs' (union msgs #{{:type "Abort"}})])
       (CHANGED- [tmState msgs])))

(defn RMPrepare [rm]
  (and (= (get* rmState rm) "working")
       (ALLOW- [rmState' (EXCEPT rmState [rm] "prepared")
                msgs' (union msgs #{{:type "Prepared"
                                     :rm rm}})])
       (CHANGED- [rmState msgs])))

(defn RMChooseToAbort [rm]
  (and (= (get* rmState rm) "working")
       (ALLOW- [rmState' (EXCEPT rmState [rm] "aborted")])
       (CHANGED- [rmState])))

(defn RMRcvCommitMsg [rm]
  (and (contains? msgs {:type "Commit"})
       (ALLOW- [rmState' (EXCEPT rmState [rm] "committed")])
       (CHANGED- [rmState])))

(defn RMRcvAbortMsg [rm]
  (and (contains? msgs {:type "Abort"})
       (ALLOW- [rmState' (EXCEPT rmState [rm] "aborted")])
       (CHANGED- [rmState])))

(defn TPNext []
  (or (TMCommit)
      (TMAbort)
      (E [rm RM]
         (or (TMRcvPrepared rm)
             (RMPrepare rm)
             (RMChooseToAbort rm)
             (RMRcvCommitMsg rm)
             (RMRcvAbortMsg rm)))))

(defn TPSpec []
  (and (TPInit)
       (always- (TPNext) [rmState tmState tmPrepared msgs])))

```

# Example TLA
The following is a full sample TLA+ output produced by the salt file above:

```
---------------------------- MODULE TwoPhase ----------------------------

(*
example spec ported from
https://github.com/tlaplus/Examples/blob/master/specifications/transaction_commit/TwoPhase.tla
*)
CONSTANT RM

VARIABLE rmState, tmState, tmPrepared, msgs

Message == [type : {"Prepared"},
            rm : RM] \union [type : { "Commit", "Abort" }]

TPTypeOk == 
    /\  rmState \in [RM -> { "committed", "prepared", "aborted", "working" }]
    /\  tmState \in { "committed", "aborted", "init" }
    /\  tmPrepared \subseteq RM
    /\  msgs \subseteq Message

TPInit == 
    /\  rmState = [rm \in RM |-> "working"]
    /\  tmState = "init"
    /\  tmPrepared = {}
    /\  msgs = {}

TMRcvPrepared( rm ) ==
    /\  tmState = "init"
    /\  [type |-> "Prepared",
         rm |-> rm] \in msgs
    /\  tmPrepared' = tmPrepared \union {rm}
    /\  UNCHANGED << rmState, tmState, msgs >>

TMCommit == 
    /\  tmState = "init"
    /\  tmPrepared = RM
    /\  tmState' = "committed"
    /\  msgs' = msgs \union {[type |-> "Commit"]}
    /\  UNCHANGED << rmState, tmPrepared >>

TMAbort == 
    /\  tmState = "init"
    /\  tmState' = "aborted"
    /\  msgs' = msgs \union {[type |-> "Abort"]}
    /\  UNCHANGED << rmState, tmPrepared >>

RMPrepare( rm ) ==
    /\  rmState[rm] = "working"
    /\  rmState' = [rmState EXCEPT ![rm] = "prepared"]
    /\  msgs' = msgs \union {[type |-> "Prepared",
                              rm |-> rm]}
    /\  UNCHANGED << tmState, tmPrepared >>

RMChooseToAbort( rm ) ==
    /\  rmState[rm] = "working"
    /\  rmState' = [rmState EXCEPT ![rm] = "aborted"]
    /\  UNCHANGED << tmState, tmPrepared, msgs >>

RMRcvCommitMsg( rm ) ==
    /\  [type |-> "Commit"] \in msgs
    /\  rmState' = [rmState EXCEPT ![rm] = "committed"]
    /\  UNCHANGED << tmState, tmPrepared, msgs >>

RMRcvAbortMsg( rm ) ==
    /\  [type |-> "Abort"] \in msgs
    /\  rmState' = [rmState EXCEPT ![rm] = "aborted"]
    /\  UNCHANGED << tmState, tmPrepared, msgs >>

TPNext == 
    \/  TMCommit
    \/  TMAbort
    \/  \E rm \in RM :
            \/  TMRcvPrepared(rm)
            \/  RMPrepare(rm)
            \/  RMChooseToAbort(rm)
            \/  RMRcvCommitMsg(rm)
            \/  RMRcvAbortMsg(rm)

TPSpec == 
    /\  TPInit
    /\  [][TPNext]_<< rmState, tmState, tmPrepared, msgs >>


=============================================================================

```

# Copyright

Copyright (c) 2019, ViaSat Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, PUNITIVE, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


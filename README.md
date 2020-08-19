# Sting

Sting is an experimental editor that tracks changes to the abstract syntax tree of the program you're editing. What differentiates it from other version control tools is that changes are tracked as far down the syntax tree as possible. Instead of presenting a single monolithic history for your entire program, as is git and its ilk, Sting allows users to explore the history of each term in their AST independently. In this respect, it aims to combine ideas from [Variolite](https://marybethkery.com/projects/Verdant/variolite-supporting-exploratory-programming.pdf) and [Unison](https://www.unisonweb.org/). Currently, the tool is only written to support a subset of Python, but the long term goal is to support a variety of other languages. 



## Tracking History

History in Sting is tracked through five data-structures. 

The *log* is a map from hashes to their associated definitions (in terms of other hashes).

The *chain* is a set of edges between hashes and their parents (the hashes that of definitions that got modified).

The *branch map* is a relation between hashes and names known as *branches*. As in git, at most one of these names per *chain*-induced connected component can be "active", which means that whenever a new node is added to the component, the branch will be moved to point to the new node instead. 

The *namespace* is a map from hashes to a set of names. It keeps track of variable bindings the user made throughout the history of the document. 

The *view* is the set of currently active hashes. 



### Creating New Definitions

When you define a new function or variable, Sting takes its hash and stores it in the *log*. Then, it stores the name you assigned it in the *namespace*. Finally, it adds the hash to the *view* set. 



### Hiding Definitions

Definitions can't be deleted in Sting, but they can be hidden. This just means we remove the associated hash from the *view*. The hash can always be added back if you want to add it back to your program.



### Updating Definitions

Updating name `n` to map to hash *h2* instead of *h1* is similar to making a new definition. We add *h2* and its definition to the *log*, we add the mapping from *h2* to `n` to the *namespace*, and we swap out *h2* for *h1* in the *view*. There's a few extra steps here, though. First, we need to map *h2* to *h1* in the *chain*. Next, if there's an active branch in *h1*'s connected component, we move this branch to point to *h2*. Finally, we need to propagate the update to anything in the *view* that depends on `n`. 



### Update Propagation

Say we had a program like this:

```python
def foo(a):
  return bar(a) + 1

def bar(a):
  return sin(a / 2)
```

and we modified the definition of `bar` to be

```python
def bar(a):
  return cos(a / 2)
```

It's not enough to insert a new definition for `bar` in the *log*. We also need to recursively update the definition of `foo` to use the new definition of `bar`. 

At each step of update propagation, we run the user's test suite. If swapping in the new definition causes tests in *view* to fail that previously succeeded, we stop propagation and alert the user. This means that different parts of the user's program can end up depending on different versions of a definition. Both versions will be added to *view*. 



### Going Back in Time

To show an old version or alternate branch of a definition, we swap outs its hash in the *view* set. This old definition may depend on old versions of other functions, which must be added to the *view* set as well. We continue recursively like this until all the necessary hashes are included. If we are switching to a particular branch, we mark this branch as the active one. 



### Merging

Let's say we want to merge history B into A.

Merging the *log* is trivial. If A and B have log keys in common, they must map to the same definition, so there's never any conflict. The *chain*, *view*, *namespace*, and *branch map* are clearly commutative monoids. The active branch stays unchanged by a merge. 



### Collaborative Editing

As merges are always free from conflicts, Sting can automatically merge in history from other users whenever new definitions are made. This allows a google-docs like collaborative editing experience.



## Editor Interface

Editor windows show a subset of the definitions in the *view* set for editing. The interface allows users to add other members of *view* into a buffer. There are other controls for removing a definition from the buffer while keeping it in *view*. Deleting a definition from the buffer also remove it from *view*. 



### Modules

As Sting programs are not stored as text files in the file-system, as in conventional Python programs, they cannot rely on filenames to separate modules. Instead, Sting adds a module declaration syntax. A line like

```haskell
module top_module.inner_module
```

will declare all definitions that follow as part of `top_module.inner_module`. The *namespace* stores all names in fully qualified form. 



### Showing Different Definitions with the Same Name

It is possible for a single name to apply to multiple hashes. This can happen when update propagation is interrupted by a failing test case, or after a merge. In this case, the interface will add a different numeric suffix to each instance of the name. These names are not tracked in the *namespace*; they are added on the fly whenever this problem is encountered. 



## Running Code

Sting gives users a unified interface for writing and running code, inspired by [Unison](https://www.unisonweb.org/) and [REPLugger](https://www.youtube.com/watch?v=F8p5bj01UWk). 



### Calling a function

Lines that begin with a pipe character are interpreted as expressions to run. The result is displayed inline. They will also get re-run whenever their definition changes. 

```python
| foo(1,2,3)
```

These are definitions, and are stored in the *log* and *view* just like any other definition. 

When these lines are placed within a function, they run every time their parent function is run. The results are shown in a search-able table. For example, the following code will show tables with the `sin` and `cos` values of 3 and 4 along with `foo` values. 

```python
def foo(x):
  | sin(x)
  | cos(x)
  return x + 2
| foo(3)
| foo(4)
```

These tables can be filtered using *context points*. The user can select another line of the program. This filters the tables to only show calls with this line in their call-stacks. 

When these pipe-statements include an assert statement, they can be interpreted as unit tests. 



### Widgets

Tests and functions can also be re-triggered if any of their inputs come from an interactive widget. This will include IPython-style sliders. 



## Documentation

A separate window in the editor shows formatted docstrings for the function under the cursor. It also shows any pipe-statements (and associated outputs) in *view* that involve this function. 



## Remote Execution

Pipe statements can also specify that a remote server should execute the line instead. This can be useful when the computation requires specialized hardware unavailable locally, or otherwise performs environment-sensitive IO. Remote servers will need to have a Sting instance listening on the relevant port. The syntax will be something like

```python
|@remote_name do_gpu_computation()
```


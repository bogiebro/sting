# Sting

Sting is an experimental editor that tracks changes to the abstract syntax tree of the program you're editing. What differentiates it from other version control tools is that changes are tracked as far down the syntax tree as possible. Instead of presenting a single monolithic history for your entire program, as is git and its ilk, Sting allows users to explore the history of each term in their AST independently. In this respect, it aims to combine ideas from [Variolite](https://marybethkery.com/projects/Verdant/variolite-supporting-exploratory-programming.pdf) and [Unison](https://www.unisonweb.org/). This facilitates easier experimentation and exploratory programming. Currently, the tool is only written to support a subset of Python, but the long term goal is to support a variety of other languages (e.g. Haskell, Unison, Idris, Julia, Datalog). The design will evolve as different languages and use cases are considered, but right now, the system is targeted at Python modeling and data analysis tasks. 



##  History Model

History in Sting is tracked through five data-structures. 

The *log* is a map from hashes to their associated definitions (in terms of other hashes).

The *chain* is a set of edges between hashes and their parents (the hashes that of definitions that got modified).

The *branch map* is a relation between hashes and names known as *branches*. As in git, at most one of these names per *chain*-induced connected component can be "active", which means that whenever a new node is added to the component, the branch will be moved to point to the new node instead. 

The *namespace* is a map from hashes to a set of names. It keeps track of variable bindings the user made throughout the history of the document. 

The *view* is the set of currently active hashes. 

**Creating new definitions**: When you define a new function or variable, Sting takes its hash and stores it in the *log*. Then, it stores the name you assigned it in the *namespace*. Finally, it adds the hash to the *view* set. 

**Hiding definitions**: Definitions can't be deleted in Sting, but they can be hidden. This just means we remove the associated hash from the *view*. The hash can always be added back if you want to add it back to your program.

**Navigating history**: To show an old version or alternate branch of a definition, we swap outs its hash in the *view* set. This old definition may depend on old versions of other functions, which must be added to the *view* set as well. We continue recursively like this until all the necessary hashes are included. If we are switching to a particular branch, we mark this branch as the active one. 

**Updating definitions**: Updating name `n` to map to hash *h2* instead of *h1* is similar to making a new definition. We add *h2* and its definition to the *log*, we add the mapping from *h2* to `n` to the *namespace*, and we swap out *h2* for *h1* in the *view*. There's a few extra steps here, though. First, we need to map *h2* to *h1* in the *chain*. Next, if there's an active branch in *h1*'s connected component, we move this branch to point to *h2*. Finally, we need to propagate the update to anything in the *view* that depends on `n`. 

**Update propagation**:

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

**Merging**: Let's say we want to merge history B into A. Merging the *log* is trivial. If A and B have log keys in common, they must map to the same definition, so there's never any conflict. The *chain*, *view*, *namespace*, and *branch map* are clearly commutative monoids. The active branch is unchanged by a merge. 

**Real time collaborative editing**: As merges are always free from conflicts, Sting can automatically merge in history from other users whenever new definitions are made. This allows a google-docs like real-time collaborative editing experience. 



## Editing Interface

**Editing an old definition**: Editor windows show a subset of the definitions in the *view* set for editing. This set is called the *buffer*. A fuzzy search menu, similar to the "Go to symbol" prompt in Sublime Text and Textmate, allows users to add definitions into the buffer. It will be triggered with a hot-key or toolbar button. The prompt lists definitions in *view* first. Those that are no longer in *view* are marked with a different formatting. If the user selects a definition that is not in *view*, this definition is both added to the buffer and added to *view*. Deleting a definition from the buffer also removes it from *view*. 

**Adding a New Definition**: Any new definitions entered into the buffer will get added to the *view* as well. 

**Modules**: As Sting programs are not stored as text files in the file-system, as in conventional Python programs, they cannot rely on filenames to separate modules. Instead, Sting adds a module declaration syntax. A line like

```python
module top_module.inner_module
```

will declare all definitions that follow as part of `top_module.inner_module`. The *namespace* stores all names in fully qualified form. 

 **Showing different definitions with the same name**: It is possible for a single name to apply to multiple hashes. This can happen when update propagation is interrupted by a failing test case, or after a merge. In this case, the interface will add a different numeric suffix to each instance of the name. These names are not tracked in the *namespace*; they are added on the fly whenever this problem is encountered. 

**Removing a Definition from the Buffer**: If you want to remove a definition from the buffer without removing it from *view*, this will be done with a hot key or toolbar button. 

**Documentation**: A separate window in the editor shows formatted docstrings for the function under the cursor. It also shows any pipe-statements (and associated outputs) in *view* that involve this function. The user can open as many documentation windows as they desire. Cursor movement will only affect the most recent window opened, so previously opened windows can act as a reference. 



## Status Indicators

To the left of the code, as in many editors, is the gutter. This displays indicator icons next to lines that require special attention from the user. 

**Error Indicators** appear as a red dot. When the user's cursor rests on a line with an error indicator, the associated error is shown in the *info panel* at the bottom of the window. This applies to both compile-time errors (syntax and type errors) as well as errors that occurred while running the code. 

**Merge conflict** indicators appear whenever a function is using an older definition than the one shown in *view*. This indicates that the given line leads to failing tests when used with the newer definition. When the user's cursor rests on a line with a merge conflict indicator, the errors that occurred when trying to use the newer definition are shown in the *info panel*. When the indicator is toggled on and off, it switches between a working version of the line that uses the old definition, and a failing version of the line that uses the new definition. Switching to the failing version will result in more error indicators being displayed (those associated with the failing upgrade).

**Callstack parent** indicators appear whenever the given line is a parent of any of the contexts in the *context panel*. Clicking these indicators will toggle whether the context panel should filter its list of contexts to only include those with the given parent. 

**Callstack child** indicators appear whenever the given line is a child of any of the contexts in the *context panel*. Clicking these indicators will toggle whether the context panel should filter its list of contexts to only those that include the given child. If a given line occurs as both a parent and a child of the selection in the context panel, Sting will default to showing the **callstack parent** indicator. 



## Running Code

Sting gives users a unified interface for writing and running code, inspired by [IPython](),  [Unison](https://www.unisonweb.org/) and [REPLugger](https://www.youtube.com/watch?v=F8p5bj01UWk). 

**Calling a function**:

Lines that begin with a pipe character are interpreted as expressions to run. The result is displayed inline. They will also get re-run whenever their definition changes. 

```python
| foo(1,2,3)
```

These are definitions, and are stored in the *log* and *view* just like any other definition. 

When these lines are placed within a function, they run every time their parent function is run. The results are shown in line, depending on the arguments chosen in the *context panel*. For example, the following code will allow users to check the the `sin` and `cos` values of 3 and 4 along with `foo` values. 

```python
def foo(x):
  | sin(x)
  | cos(x)
  return x + 2
| foo(3)
| foo(4)
```

When these pipe-statements include an assert statement, they can be interpreted as unit tests. 

Pipe statements have entries in the *namespace* as well. They are named based on their enclosing function. In the example above, the calls to `| foo(3)` and `| foo(4)` would both have name `_pipe_`, and the calls to `| sin(x)` and `| cos(x)` would both have name `foo._pipe_`. 

**The context panel**:

When a pipe statement is nested inside a function that has been called with multiple different sets of arguments, Sting will not know which result to show inline. Unless the user takes action, it will not display anything. A separate panel on the right side of the editor window will list all the invocations of the function containing the user's cursor, and allows the user to select which invocation they wish to see.

This list is searchable, using a search dialog at the top of the context panel, which filters the visible invocations to those that match the search string as it is typed. It is also filtered by the **callstack parent** and **callstack child** indicators mentioned previously. An active **callstack parent** indicator will filter entries in the context panel to those in the callstack of invocations on the line with the active indicator. An active **callstack child** indicator filter entries in the context panel to those with a callstack that includes the line with the active indicator. 

Selecting an entry in the context panel also affects the display of pipe statements for functions that do not currently contain the user's cursor. Other functions that occur in the callstack of the selected invocation will show the results from when they are called during this invocation. 

The context panel will also show the callstack for the selected invocation below the list of invocations. Clicking on a line in the callstack will move the user's cursor to that line of the program. 

**Widgets**: Tests and functions can also be re-triggered if any of their inputs come from an interactive widget. These will be very similar to the interactive widgets in IPython notebooks. A special python library can be imported, something like `from sting.widgets import slider`.  Definitions can then be of the form `x=slider(min=0, max=10)`. 

**Remote Execution**: Pipe statements can also specify that a remote server should execute the line instead. This can be useful when the computation requires specialized hardware unavailable locally, or otherwise performs environment-sensitive IO. Remote servers will need to have a Sting instance listening on the relevant port. The syntax will be something like

```python
|@remote_name do_gpu_computation()
```

Inline results, callstacks in the context panel, and indicators in the gutter will all be displayed for remote executions exactly the same way as they are for local executions. 


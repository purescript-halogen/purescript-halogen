# The Halogen guide

1. [Introduction](1 - Introduction.md)
2. [Defining a component](2 - Defining a component.md)
  - [State](2 - Defining a component.md#state)
  - [Query algebra](2 - Defining a component.md#query-algebra)
  - [Output messages](2 - Defining a component.md#output-messages)
  - [Rendering](2 - Defining a component.md#rendering)
    - [The HTML DSL](2 - Defining a component.md#the-html-dsl)
  - [Query evaluation](2 - Defining a component.md#query-evaluation)
    - [`HalogenM`](2 - Defining a component.md#halogenm)
    - [Evaluating actions](2 - Defining a component.md#evaluating-actions)
    - [Evaluating requests](2 - Defining a component.md#evaluating-requests)
  - [Putting it all together](2 - Defining a component.md#putting-it-all-together)
3. [Handling effects](3 - Handling effects.md)
  - [Using `Eff` during `eval`](3 - Handling effects.md#using-eff-during-eval)
  - [Using `Aff` during `eval`](3 - Handling effects.md#using-aff-during-eval)
  - [Mixing `Eff` and `Aff`](3 - Handling effects.md#mixing-eff-and-aff)
4. [Running a component](4 - Running a component.md)
  - [`runUI`](4 - Running a component.md#runui)
  - [`Aff`-based utility functions](4 - Running a component.md#aff-based-utility-functions)
5. [Parent and child components](5 - Parent and child components.md)
  - [Slot addresses](5 - Parent and child components.md#slot-addresses)
  - [Rendering](5 - Parent and child components.md#rendering)
  - [Querying](5 - Parent and child components.md#querying)
  - [Component definition](5 - Parent and child components.md#component-definition)
  - [Input values](5 - Parent and child components.md#input-values)
  - [Multiple types of child component](5 - Parent and child components.md#multiple-types-of-child-component)
5. Component lifecycle hooks*
6. Event sources*
7. Non-`Aff` effect monads*

\* Coming soon... ish.

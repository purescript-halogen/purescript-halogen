# The Halogen guide

1. [Introduction](1%20-%20Introduction.md)
2. [Defining a component](2%20-%20Defining%20a%20component.md)
    - [State](2%20-%20Defining%20a%20component.md#state)
    - [Query algebra](2%20-%20Defining%20a%20component.md#query-algebra)
    - [Output messages](2%20-%20Defining%20a%20component.md#output-messages)
    - [Rendering](2%20-%20Defining%20a%20component.md#rendering)
        - [The HTML DSL](2%20-%20Defining%20a%20component.md#the-html-dsl)
    - [Query evaluation](2%20-%20Defining%20a%20component.md#query-evaluation)
        - [`HalogenM`](2%20-%20Defining%20a%20component.md#halogenm)
        - [Evaluating actions](2%20-%20Defining%20a%20component.md#evaluating-actions)
        - [Evaluating requests](2%20-%20Defining%20a%20component.md#evaluating-requests)
    - [Putting it all together](2%20-%20Defining%20a%20component.md#putting-it-all-together)
3. [Handling effects](3%20-%20Handling%20effects.md)
    - [Using `Eff` during `eval`](3%20-%20Handling%20effects.md#using-eff-during-eval)
    - [Using `Aff` during `eval`](3%20-%20Handling%20effects.md#using-aff-during-eval)
    - [Mixing `Eff` and `Aff`](3%20-%20Handling%20effects.md#mixing-eff-and-aff)
4. [Running a component](4%20-%20Running%20a%20component.md)
    - [`runUI`](4%20-%20Running%20a%20component.md#runui)
    - [`Aff`-based utility functions](4%20-%20Running%20a%20component.md#aff-based-utility-functions)
5. [Parent and child components](5%20-%20Parent%20and%20child%20components.md)
    - [Slot address](5%20-%20Parent%20and%20child%20components.md#slot-address)
    - [Rendering](5%20-%20Parent%20and%20child%20components.md#rendering)
    - [Querying](5%20-%20Parent%20and%20child%20components.md#querying)
    - [Component definition](5%20-%20Parent%20and%20child%20components.md#component-definition)
    - [Input values](5%20-%20Parent%20and%20child%20components.md#input-values)
    - [Multiple types of child component](5%20-%20Parent%20and%20child%20components.md#multiple-types-of-child-component)
        - [Rendering](5%20-%20Parent%20and%20child%20components.md#rendering-1)
        - [Querying](5%20-%20Parent%20and%20child%20components.md#querying-1)
        - [Custom `ChildPath` definitions](5%20-%20Parent%20and%20child%20components.md#custom-childpath-definitions)
5. Component lifecycle hooks*
6. Event sources**
7. Non-`Aff` effect monads**

\* Coming soon...
\** ...ish

**Note:** Please feel free to open issues if you encounter things in the guide that are confusing or need further explanation!

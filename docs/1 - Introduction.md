# Introduction


A Halogen application consists of a tree of components. Each component is a self contained unit that has its own state, and re-renders when the state changes.

A component modifies its state by evaluating inputs known as queries, or actions which may arise Internally. Each component defines four types which govern how it operates:

1. `State` - Defines the data types which will drive the behaviour of the component.

2. `Query` - Defines the external messages which may be passed to the component. We use the term "query" rather than something like "input" as queries can both return details about the component state as well as update component state.

3. `Action` - Defines the events that arise internally to the component. Typically, these arise from user interaction, such as a button click. However actions can also be triggered as a result of a query.

4. `Output Message` - Defines the outbound events (or messages) that a child component can broadcast up to the parent. A component can also emit output messages during query evaluation. As with queries, these output messages have a type specified by the component. Output messages are used to notify external listeners of activity within the component. The motivating example for this is to allow a parent component to observe activity within a child component.

5. `Input` - Defines inputs provided to the component directly from its parent or the mount point for a root component.

Component definitions are pure. Running the root component to actually produce the UI is the place effects occur. As UIs tend to be asynchronous, this generally means running in the `Aff` monad.

Halogen's types do allow for more than just HTML based UIs, but the guide will focus on this primary use case.

Let's take a look at a [defining a component](2%20-%20Defining%20a%20component.md).

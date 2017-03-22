# Introduction

A Halogen application consists of a tree of components. Each component is a self contained unit that has its own state, and re-renders when the state changes.

A component modifies its state by evaluating inputs known as queries. Each component defines a type that describes all the possible queries it will expect. We use the term "query" rather than something like "input" as they can return details about the component state as well as being able to update it.

A component can also emit messages during query evaluation. As with queries, these messages have a type specified by the component. Messages are used to notify listeners of activity within the component. The motivating example for this is to allow a parent component to observe activity within a child.

Component definitions are pure. Running the root component to actually produce the UI is the place effects occur. As UIs tend to be asynchronous, this generally means running in `Aff`.

Halogen's types do allow for more than just HTML based UIs, but the guide will focus on this primary use case.

Let's take a look at a [defining a component](2%20-%20Defining%20a%20component.md).

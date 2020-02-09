# Introduction

A Halogen application consists of a tree of components. Each component is a self contained unit that has its own state, and re-renders when the state changes.

A component's state is changed by the component evaluating inputs known as queries, or by evaluating actions which arise "internally" from the component's rendered HTML.

Each component defines five types which govern how it operates:

1. `State` - the value which will drive the behaviour and appearance of the component.

2. `Query` - the external messages which may be sent to the component. We use the term "query" as queries are not just instructions to the component, they can also be used to request information from the component.

3. `Action` - the events that arise internally to the component. Typically, these arise from user interaction, such as a button click.

4. `Message` - the outbound messages that a child component can send to their parent. Output messages are used to notify external listeners of activity within the component, allowing a parent component to observe activity within a child component.

5. `Input` - inputs provided to the component directly from its parent or the mount point for a root component.

Component definitions are pure. Running the root component to actually produce the UI is the place effects occur. As UIs tend to be asynchronous, this generally means running in the `Aff` monad.

Halogen's types do allow for more than just HTML based UIs, but the guide will focus on this primary HTML use case.

Let's take a look at a [defining a component](2%20-%20Defining%20a%20component.md).

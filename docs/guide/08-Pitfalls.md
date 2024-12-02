# Pitfalls

This part of the guide is a collection of common problems which can arise when working with Halogen. It should help build understanding the eco-system in and around Halogen.

# Working with Inputs

When passing properties to an input field you should be aware of specific constellations. This is especially the case when using the value property.
One good pattern is to use the value property as the last (or very last) property. Have a look at the following example:

```
HH.input
  [ HP.value $ show state.currentValue
  , HP.type_ HP.InputRange
  , HP.min 0.0
  , HP.max 1.0
  , HP.step $ HP.Step 0.01
  , HP.required true
  , HE.onValueInput $ \val -> UpdateCurrentValue val
  ]
```

This example won't set the initial value for currentValue properly. Unfortunately the order of properties has an effect. The correct way of this input looks as follows:

```
HH.input
  [ HP.type_ HP.InputRange
  , HP.min 0.0
  , HP.max 1.0
  , HP.step $ HP.Step 0.01
  , HP.required true
  , HP.value $ show state.currentValue
  , HE.onValueInput $ \val -> UpdateCurrentValue val
  ]
```

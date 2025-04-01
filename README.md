# Nexus-Lang

### GEN1 - 1.0.0
> Created in 31/03/2025, Brazil. Ownered by MewPlush.<br>
> Based of the tutorials of "CodePulse".<br>
> Version GEN1 - 1.0.0, read as: GEN[Gen Number] - [Biggest Number].[Middlest Number].[Quantity Number]
> - [Biggest Number]: The Main Number, generally the GEN number.
> - [Middlest Number]: The Middlest Number, compliments the Biggest Number on small updates.
> - [Quantity Number]: The amount of quantity added to the Version.

# How to Install and Run Nexus?

# The Syntax (GEN1 - 1.0.0)

## Variables (GEN1 - 1.0.0)
> Create variables, they can't be constant and can be re-assigned whenever you like, can be used in expressions.
```nxs
nexus myVar = "Hello, World!"
```
<br>
Variables can be accessed and can also change value using the same syntax, by not assigning a value, the variable keeps the null value.

```
nexus myVar
```

<br>

You can create multiple variables with the same value
```nxs
nexus a, b, c = 12
```

You can assign functions return values
```nxs
nexus a = add_one(3) # 4
```

Variables can be assigned mid expressions
```
write(5 + (nexus a = 12)) # 17
write(5 + a) # 17
```

## Types (GEN1 - 1.0.0)
> There are 4 main types for Nexus Lang (for now), which are: `list`, `int`, `str`, `function`.

---

### STR
```
"Hello, World!"
```
String needs to be surrounded in double quote marks. They can store all ASCII Characters and all DIGITS (0-9).

---
String related operations are:

**Add**
```
"Hello, " + "World!"
```
join string values.


### INT
Int (Integer) is a type which contains a number value, that respects python's size limit. 
```
12
```
They can contain any number within DIGITS (0-9).

Related operations follow basic math rules, they include: `sum (+)`, `sub (-)`, `div (/)`, `mult (*)`, `pow (^)`.

---

### LIST

### Built-In Functions on Types

You can get a variable or value type using:
```
type(5) # "int"
```

You can transform a type of a variable/value using:
```
str(5) # "5"
int("12") # 12
```

Check more on built-in functions.

You can't make functions, neither variables request specific values without using `if expressions`.
It is expected a `request(variable, type)` function soon. To make this request easier.
For now, the best way is to use `is_int()`, `is_str()`, `is_list()` or `is_func()` functions.






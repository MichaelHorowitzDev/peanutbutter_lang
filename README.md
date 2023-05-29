# PeanutButterLang
A hobbyist programming language with questionable experimental features.

This was a project that I created because I wanted to learn how to make a programming langugage, specifically in Haskell. I used the book [Crafting Interpreters by Robert Nystrom](https://craftinginterpreters.com) for getting most of my knowledge on this subject.

<h2>Features</h2>
<ul>
  <li>Supports mutable and immutable variables</li>
  <li>Data types such as Ints, Doubles, Strings, Bools, Lists</li>
  <li>Functions and complex data types</li>
  <li>Functional programming encouraged</li>
</ul>

PeanutButter files end in .pb but any file extension will work as long as it's a text file.

<h2>Variables</h2>
To create a variable, use the keyword var:
<br></br>

```swift
var x = 20;
```
To make it immutable, use the keyword let:
```swift
let x = 5.2;
```
Strings are surrounded by double quotation marks:
```swift
let string = "Hello, World!";
```
Booleans:
```true``` or ```false```
Lists:
```swift
[4, 8.3, "apple", false];
```

<h2>Lists</h2>
Lists can be created by having elements separated by commas and enclosed in parentheses. They can also be created by using a range with two numbers:
<br></br>

```swift
[1..5];
```
is equivalent to
```swift
[1, 2, 3, 4, 5];
```
Lists also support the same slicing and subscript operations that Python supports:
```swift
[1..10][3]; // 4
[1..10][2:7]; // [3, 4, 5, 6, 7] 
[1..10][2:]; // everything but the first two elements [3, 4, 5, 6, 7, 8, 9, 10]
[1..10][:2]; // only the first two elements [1, 2]
```

<h2>Functions</h2>
Functions are similar to most programming languages.
<br></br>

```swift
func square(x) {
  return x * x;
}
```

An important distinction is that functions are mostly pure. They can perform mutation internally but cannot mutate outside variables or even reference a mutable variable in an outer scope.
The following code will error when run:
```swift
var x = 10;
func f() {
  print x;
}
f();
```
If the variable were immutable though, this code would work fine:
```swift
let x = 10;
func f() {
  print x;
}
f();
```

<h2>Complex Data Types</h2>
The language supports complex data types similar to classes. Unlike classes, which combine behavior and state, these data types are similar to a namespace as they are immutable.
They are created like so:
<br></br>

```swift
data Vector(x, y) {
	func add(vector) {
		return Vector(self.x + vector.x, self.y + vector.y);
	}
}

let x = Vector(2, 7);
let y = Vector(3, -1);
let total = x.add(y);
```
Data types with no arguments can have their parentheses omitted:
```swift
data Greeting {
  func english() {
    putStr("Hello\n");
  }
  func chinese() {
    putStr("你好\n");
  }
}

Greeting().english(); // prints "Hello"
Greeting().chinese(); // prints "你好"
```

Methods and properties are called on data types with dot syntax but there is also another use for dot syntax. Take the following code:

```swift
print map([1..10], \x -> x * x);
```
This code uses the built-in map function to map over a list and square every number. Using dot syntax, we can rewrite this function in a cleaner way:
```swift
print [1..10].map(\x -> x * x);
```
Dot syntax is simple syntactic sugar. Given a function that takes two arguments, using dot syntax will pass the previous expression implicity as the first argument to the function.

<h2>Native Functions</h2>
The language supports a few native functions for making certain tasks possible.
<br></br>

| Function | Usage |
| -------- | ----- |
| clock | will return a current utc time for day |
| sqrt | square root of a number |
| length | length of a list |
| map | map over a list |
| filter | filter a list |
| foldr | reduce a list to a single value |
| append | append a value to a list; highly inefficient and not recommended for use |
| toList | convert a string to a list |
| toString | convert a list to a string; will return an empty string if element is not a string |
| input | get a string as input from a user |
| putStr | print a string to standard output |
| show | convert any value to a string |



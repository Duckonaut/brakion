Example: Running in interactive mode

```
> var a = 5;
> a == 5;
> a $ 5;
syntax error: Unexpected character.
 --> <shell>:1:3
  |
1 | a $ 5;
  |   ^
  |

> a == ;
syntax error: Expected expression.
 --> <shell>:1:6
  |
1 | a == ;
  |      ^
  |

> a == b;
runtime error: Undefined variable 'b'
 --> <shell>:1:6
  |
1 | a == b;
  |      ^
  |
```

![img](assets/error_example.png)

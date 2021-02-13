## Show Code #1 (4 of 4 points)

Programs runs very well. I appreciate the user interface telling me exactly what is going on. I have a note about style. In your Player you wrote the following code.

```
var goLoc: Option[Room] = None

dir.take(1).toLowerCase match {
  case "n" => goLoc = currentLoc.getExit(0)
  case "s" => goLoc = currentLoc.getExit(1)
  case "e" => goLoc = currentLoc.getExit(2)
  case "w" => goLoc = currentLoc.getExit(3)
  case "u" => goLoc = currentLoc.getExit(4)
  case "d" => goLoc = currentLoc.getExit(5)
}
```

Note that `match` can be used as an expression. So you can simplify this and get rid of the `var` by changing it to the following.

```
val goLoc = dir.take(1).toLowerCase match {
  case "n" => currentLoc.getExit(0)
  case "s" => currentLoc.getExit(1)
  case "e" => currentLoc.getExit(2)
  case "w" => currentLoc.getExit(3)
  case "u" => currentLoc.getExit(4)
  case "d" => currentLoc.getExit(5)
}
```

Personally, I prefer something like the following that completely skips the match.

```
val goLoc = currentLoc.getExit("nsewud".indexOf(dir(0).toLower))
```

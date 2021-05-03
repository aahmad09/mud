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

## Show Code #6 (4 of 4 points)

Code compiles and runs with actors. I noticed an odd delay on the first command that I entered, but it wasn't significant. Also, since you have
started putting in networking, please remember to print the port number so I won't have to look it up.

## Show Code #9 (4 of 4 points)

Code compiles and runs. There are NPCs and they move around. After the show code, when I saw code for ActivityManager that anotehr student was
working on that was almost identical to yours, I realized that I don't think your activity manager is doing quite what you want. In particular,
the line that resets resets numUpdates is going to cause problems combined with the fact that when you schedule you don't do delay+numUpdates. To see why,
consider the following scenario. There is an event that takes place after 10 seconds, so a delay of 100. Some other thing, that will repeat, gets scheduled
with a delay of 1 second (delay of 10). When numUpdates gets to 10 you process the quick event, but it immediately reschedules itself to happen one second later.
That means it passes a delay of 10. However, your code is going to have it happen in the very next update because 10 < numUpdates at that time. Because
you don't schedule at delay+numUpdates, things frequenty get scheduled in the past if there is some event that should happen long in the future.

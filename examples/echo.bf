>,                Skip one cell ahead and read character
>++[<----->-]<    Subtract 10 from input
[                 Check if input value is 0 (was newline)
  >++[<+++++>-]<  Restore input value (add 10)
  >,              Read new input into next cell
  >++[<----->-]<  Subtract 10 from input
]                 If we read newline condition is now 0 and we're done
<[<]              Search backwards for cell with 0 value (Remember how we skipped the first cell?)
>[.>]             Print as long as we find non-zero valued cell
